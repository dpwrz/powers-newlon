# ============================================================
# STEP 4 - VALIDATE 1.2-STYLE DIRECT-FPPG GUARDRAIL INSIDE 2.0
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr", "rpart"))
engine <- resolve_model_engine()
df <- readr::read_csv("data/processed/model_table.csv", show_col_types = FALSE)
df <- add_v7_targets(df)

all_targets <- sort(unique(df$season))
all_targets <- all_targets[all_targets >= TRAIN_START + 2 & all_targets <= TRAIN_END]
targets <- tail(all_targets, min(VALIDATION_YEARS, length(all_targets)))
cat("[VALIDATE] Walk-forward holdout seasons: ", paste(targets, collapse = ", "), "\n", sep = "")
cat("[VALIDATE] Trees/fit: regression=", VALIDATION_ENSEMBLE_TREES,
    ", tier=", VALIDATION_TIER_TREES,
    ", breakout=", VALIDATION_BREAKOUT_TREES, "\n", sep = "")

base_preds <- list()
for (target_year in targets) {
  train <- df |> dplyr::filter(season < target_year)
  test <- df |> dplyr::filter(season == target_year)

  for (i in seq_along(POSITIONS)) {
    pos <- POSITIONS[i]
    tr <- train |> dplyr::filter(position == pos)
    te <- test |> dplyr::filter(position == pos)
    if (nrow(tr) < 50 || nrow(te) == 0) next
    feature_cols <- get_model_features(pos, names(df))

    cat("[VALIDATE] ", target_year, " ", pos, " (train=", nrow(tr), ", test=", nrow(te),
        ", features=", length(feature_cols), ")...\n", sep = "")

    reg_fit <- fit_fantasy_model(
      tr, feature_cols, "target_fppg", engine,
      seed = SEED + target_year + i * 100,
      n_trees = VALIDATION_ENSEMBLE_TREES
    )
    detail <- predict_fantasy_model_detail(reg_fit, te)
    te$model_only_fppg <- pmax(0, detail$model_prediction)
    te$model_disagreement_sd <- detail$model_disagreement_sd

    tier_fit <- fit_fantasy_classifier(
      tr, feature_cols, "tier_target",
      class_levels = c("Depth", "Starter", "Elite"),
      seed = SEED + target_year + i * 100 + 10000,
      n_trees = VALIDATION_TIER_TREES
    )
    tier_prob <- predict_fantasy_classifier(tier_fit, te)
    te$elite_probability <- tier_prob[, "Elite"]
    te$starter_probability <- pmin(1, tier_prob[, "Starter"] + tier_prob[, "Elite"])

    breakout_fit <- fit_fantasy_classifier(
      tr, feature_cols, "breakout_target",
      class_levels = c("No", "Yes"),
      seed = SEED + target_year + i * 100 + 20000,
      n_trees = VALIDATION_BREAKOUT_TREES
    )
    breakout_prob <- predict_fantasy_classifier(breakout_fit, te)
    te$breakout_probability <- breakout_prob[, "Yes"]

    te$baseline_fppg <- pmax(0, suppressWarnings(as.numeric(te$recent_weighted_fppg)))
    te$baseline_fppg[!is.finite(te$baseline_fppg)] <- 0
    te$target_year <- target_year
    base_preds[[length(base_preds) + 1]] <- te
  }
}

base <- dplyr::bind_rows(base_preds)
if (nrow(base) == 0) stop("Validation created no predictions.")

# ------------------------------------------------------------
# Tune the regression/recent-production blend by position.
# ------------------------------------------------------------
candidate_rows <- list()
for (w in BLEND_WEIGHT_CANDIDATES) {
  temp <- base
  temp$candidate_model_weight <- w
  temp$candidate_prediction <- blend_projection(temp$model_only_fppg, temp, model_weight = w)
  temp$candidate_error <- temp$candidate_prediction - temp$target_fppg
  candidate_rows[[length(candidate_rows) + 1]] <- temp
}

candidate_preds <- dplyr::bind_rows(candidate_rows)
blend_metrics <- candidate_preds |>
  dplyr::group_by(position, candidate_model_weight) |>
  dplyr::summarise(
    n = dplyr::n(),
    MAE = mean(abs(candidate_error), na.rm = TRUE),
    RMSE = sqrt(mean(candidate_error^2, na.rm = TRUE)),
    correlation = suppressWarnings(stats::cor(candidate_prediction, target_fppg, use = "complete.obs")),
    .groups = "drop"
  ) |>
  dplyr::arrange(position, MAE, RMSE)

selected_weights <- blend_metrics |>
  dplyr::group_by(position) |>
  dplyr::slice_min(order_by = MAE, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    position,
    selected_model_weight = candidate_model_weight,
    selected_recent_weight = 1 - candidate_model_weight,
    validation_MAE = MAE,
    validation_RMSE = RMSE
  )

readr::write_csv(blend_metrics, "output/validation_blend_candidates.csv")
readr::write_csv(selected_weights, "output/selected_blend_weights.csv")
cat("\n[VALIDATE] Selected 2026 veteran model weights:\n")
print(selected_weights)

# ------------------------------------------------------------
# Honest rolling blend tuning for historical scoring.
# ------------------------------------------------------------
rolling_weight_rows <- list()
for (target_year in targets) {
  for (pos in POSITIONS) {
    prior_eval <- base |>
      dplyr::filter(.data$position == .env$pos, .data$target_year < .env$target_year)

    if (nrow(prior_eval) == 0) {
      best_w <- DEFAULT_MODEL_PREDICTION_WEIGHT
      basis <- "default_no_prior_holdout"
    } else {
      prior_scores <- lapply(BLEND_WEIGHT_CANDIDATES, function(w) {
        pred <- blend_projection(prior_eval$model_only_fppg, prior_eval, model_weight = w)
        err <- pred - prior_eval$target_fppg
        data.frame(
          candidate_model_weight = w,
          MAE = mean(abs(err), na.rm = TRUE),
          RMSE = sqrt(mean(err^2, na.rm = TRUE))
        )
      }) |> dplyr::bind_rows() |> dplyr::arrange(MAE, RMSE)
      best_w <- prior_scores$candidate_model_weight[1]
      basis <- paste(sort(unique(prior_eval$target_year)), collapse = "+")
    }

    rolling_weight_rows[[length(rolling_weight_rows) + 1]] <- data.frame(
      target_year = target_year,
      position = pos,
      selected_model_weight = best_w,
      selected_recent_weight = 1 - best_w,
      tuning_basis_years = basis
    )
  }
}
rolling_weights <- dplyr::bind_rows(rolling_weight_rows)
readr::write_csv(rolling_weights, "output/validation_rolling_blend_weights.csv")

raw_preds <- base |>
  dplyr::left_join(rolling_weights, by = c("target_year", "position")) |>
  dplyr::mutate(
    raw_predicted_fppg = mapply(
      function(mp, rw, rookie, w) {
        if (!is.finite(rw)) rw <- 0
        if (!is.finite(rookie)) rookie <- 0
        use_w <- ifelse(rookie == 1, 1, w)
        max(0, use_w * mp + (1 - use_w) * rw)
      }, model_only_fppg, recent_weighted_fppg, is_rookie, selected_model_weight
    )
  )

# ------------------------------------------------------------
# Inherited decision-layer rolling calibration. A holdout season may only use earlier
# holdout predictions to learn its correction.
# ------------------------------------------------------------
rolling_calibration_rows <- list()
for (target_year in targets) {
  for (pos in POSITIONS) {
    prior_eval <- raw_preds |>
      dplyr::filter(.data$position == .env$pos, .data$target_year < .env$target_year)

    if (nrow(prior_eval) < CALIBRATION_MIN_ROWS) {
      params <- c(intercept = 0, slope = 1, residual_sd = 0, n = nrow(prior_eval))
      basis <- if (nrow(prior_eval) == 0) "identity_no_prior_holdout" else "identity_insufficient_history"
    } else {
      params <- fit_linear_calibration(prior_eval$raw_predicted_fppg, prior_eval$target_fppg)
      basis <- paste(sort(unique(prior_eval$target_year)), collapse = "+")
    }

    rolling_calibration_rows[[length(rolling_calibration_rows) + 1]] <- data.frame(
      target_year = target_year,
      position = pos,
      calibration_intercept = as.numeric(params["intercept"]),
      calibration_slope = as.numeric(params["slope"]),
      calibration_residual_sd = as.numeric(params["residual_sd"]),
      calibration_n = as.numeric(params["n"]),
      calibration_basis_years = basis
    )
  }
}
rolling_calibration <- dplyr::bind_rows(rolling_calibration_rows)
readr::write_csv(rolling_calibration, "output/validation_rolling_calibration.csv")

preds <- raw_preds |>
  dplyr::left_join(rolling_calibration, by = c("target_year", "position")) |>
  dplyr::mutate(
    predicted_fppg = apply_linear_calibration(raw_predicted_fppg, calibration_intercept, calibration_slope),
    model_error = predicted_fppg - target_fppg,
    raw_model_error = raw_predicted_fppg - target_fppg,
    baseline_error = baseline_fppg - target_fppg,
    abs_model_error = abs(model_error),
    abs_raw_model_error = abs(raw_model_error),
    abs_baseline_error = abs(baseline_error),
    cohort = ifelse(is_rookie == 1, "Rookie", "Veteran"),
    career_stage = dplyr::case_when(
      is_rookie == 1 ~ "Rookie",
      experience <= 2 ~ "Early career (1-2)",
      experience <= 5 ~ "Prime (3-5)",
      TRUE ~ "Veteran (6+)"
    ),
    calibration_bucket = cut(
      predicted_fppg,
      breaks = c(-Inf, 5, 10, 15, 20, Inf),
      labels = c("<5", "5-10", "10-15", "15-20", "20+"),
      right = FALSE
    ),
    actual_elite = as.integer(tier_target == "Elite"),
    actual_starter = as.integer(tier_target %in% c("Starter", "Elite")),
    actual_breakout = as.integer(breakout_target == "Yes")
  ) |>
  dplyr::group_by(position, target_year) |>
  dplyr::mutate(
    actual_position_rank = rank(-target_fppg, ties.method = "min"),
    predicted_position_rank = rank(-predicted_fppg, ties.method = "min"),
    rank_error = predicted_position_rank - actual_position_rank,
    abs_rank_error = abs(rank_error),
    elite_cutoff = as.numeric(ELITE_RANK[position]),
    replacement_cutoff = as.numeric(REPLACEMENT_RANK[position]),
    actual_tier = dplyr::case_when(
      actual_position_rank <= elite_cutoff ~ "Elite",
      actual_position_rank <= replacement_cutoff ~ "Starter",
      TRUE ~ "Depth"
    ),
    predicted_tier = dplyr::case_when(
      predicted_position_rank <= elite_cutoff ~ "Elite",
      predicted_position_rank <= replacement_cutoff ~ "Starter",
      TRUE ~ "Depth"
    )
  ) |>
  dplyr::ungroup()

# ------------------------------------------------------------
# Final 2026 calibration uses all completed OOF holdouts, but it
# is NOT used to score the historical direct-model validation above.
# ------------------------------------------------------------
future_calibration_rows <- list()
for (pos in POSITIONS) {
  d <- base |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next
  w_vec <- selected_weights$selected_model_weight[selected_weights$position == pos]
  w <- if (length(w_vec) == 0 || !is.finite(w_vec[1])) DEFAULT_MODEL_PREDICTION_WEIGHT else as.numeric(w_vec[1])
  d$future_raw_prediction <- blend_projection(d$model_only_fppg, d, model_weight = w)
  params <- fit_linear_calibration(d$future_raw_prediction, d$target_fppg)
  future_calibration_rows[[length(future_calibration_rows) + 1]] <- data.frame(
    position = pos,
    intercept = as.numeric(params["intercept"]),
    slope = as.numeric(params["slope"]),
    residual_sd = as.numeric(params["residual_sd"]),
    n = as.numeric(params["n"]),
    selected_model_weight = w
  )
}
selected_calibration <- dplyr::bind_rows(future_calibration_rows)
readr::write_csv(selected_calibration, "output/selected_calibration.csv")

# Calibrate final 2026 decision probabilities from all honest OOF classifier predictions.
prob_cal_rows <- list()
for (pos in POSITIONS) {
  d <- preds |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next
  for (event in c("Elite", "Starter+", "Breakout/Upside")) {
    if (event == "Elite") { prob <- d$elite_probability; actual <- d$actual_elite }
    if (event == "Starter+") { prob <- d$starter_probability; actual <- d$actual_starter }
    if (event == "Breakout/Upside") { prob <- d$breakout_probability; actual <- d$actual_breakout }
    params <- fit_probability_calibration(prob, actual)
    prob_cal_rows[[length(prob_cal_rows) + 1]] <- data.frame(
      position = pos, event = event,
      intercept = as.numeric(params["intercept"]),
      slope = as.numeric(params["slope"]),
      n = as.numeric(params["n"])
    )
  }
}
selected_probability_calibration <- dplyr::bind_rows(prob_cal_rows)
readr::write_csv(selected_probability_calibration, "output/selected_probability_calibration.csv")

# ------------------------------------------------------------
# Strategy comparison on exactly the same held-out rows.
# ------------------------------------------------------------
fixed80 <- preds
fixed80$.fixed80 <- blend_projection(fixed80$model_only_fppg, fixed80, model_weight = 0.80)
comparison_long <- dplyr::bind_rows(
  preds |> dplyr::transmute(position, target_year, target_fppg, method = "Baseline recent production", prediction = baseline_fppg),
  preds |> dplyr::transmute(position, target_year, target_fppg, method = "Ensemble only", prediction = model_only_fppg),
  fixed80 |> dplyr::transmute(position, target_year, target_fppg, method = "Fixed 80/20 blend", prediction = .fixed80),
  preds |> dplyr::transmute(position, target_year, target_fppg, method = "V6 rolling tuned raw blend", prediction = raw_predicted_fppg),
  preds |> dplyr::transmute(position, target_year, target_fppg, method = "1.2 rolling calibrated blend", prediction = predicted_fppg)
) |>
  dplyr::mutate(error = prediction - target_fppg)

model_comparison <- comparison_long |>
  dplyr::group_by(position, method) |>
  dplyr::summarise(
    n = dplyr::n(),
    MAE = mean(abs(error), na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    correlation = suppressWarnings(stats::cor(prediction, target_fppg, use = "complete.obs")),
    bias = mean(error, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(position, MAE, RMSE)
readr::write_csv(model_comparison, "output/validation_model_comparison.csv")

# Core multi-year metrics by position.
metrics <- preds |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    seasons_tested = dplyr::n_distinct(target_year),
    n = dplyr::n(),
    model_MAE = mean(abs_model_error, na.rm = TRUE),
    baseline_MAE = mean(abs_baseline_error, na.rm = TRUE),
    MAE_improvement_pct = 100 * (baseline_MAE - model_MAE) / pmax(0.001, baseline_MAE),
    model_RMSE = sqrt(mean(model_error^2, na.rm = TRUE)),
    baseline_RMSE = sqrt(mean(baseline_error^2, na.rm = TRUE)),
    RMSE_improvement_pct = 100 * (baseline_RMSE - model_RMSE) / pmax(0.001, baseline_RMSE),
    correlation = suppressWarnings(stats::cor(predicted_fppg, target_fppg, use = "complete.obs")),
    spearman_rank_correlation = suppressWarnings(stats::cor(predicted_fppg, target_fppg, method = "spearman", use = "complete.obs")),
    bias = mean(model_error, na.rm = TRUE),
    mean_abs_rank_error = mean(abs_rank_error, na.rm = TRUE),
    .groups = "drop"
  )

# Explicitly quantify whether calibration helps.
calibration_effect <- preds |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    n = dplyr::n(),
    raw_MAE = mean(abs_raw_model_error, na.rm = TRUE),
    calibrated_MAE = mean(abs_model_error, na.rm = TRUE),
    MAE_change_pct = 100 * (raw_MAE - calibrated_MAE) / pmax(0.001, raw_MAE),
    raw_RMSE = sqrt(mean(raw_model_error^2, na.rm = TRUE)),
    calibrated_RMSE = sqrt(mean(model_error^2, na.rm = TRUE)),
    RMSE_change_pct = 100 * (raw_RMSE - calibrated_RMSE) / pmax(0.001, raw_RMSE),
    raw_bias = mean(raw_model_error, na.rm = TRUE),
    calibrated_bias = mean(model_error, na.rm = TRUE),
    .groups = "drop"
  )

# Stability by holdout year.
year_metrics <- preds |>
  dplyr::group_by(target_year, position) |>
  dplyr::summarise(
    n = dplyr::n(),
    model_MAE = mean(abs_model_error, na.rm = TRUE),
    baseline_MAE = mean(abs_baseline_error, na.rm = TRUE),
    MAE_improvement_pct = 100 * (baseline_MAE - model_MAE) / pmax(0.001, baseline_MAE),
    model_RMSE = sqrt(mean(model_error^2, na.rm = TRUE)),
    baseline_RMSE = sqrt(mean(baseline_error^2, na.rm = TRUE)),
    correlation = suppressWarnings(stats::cor(predicted_fppg, target_fppg, use = "complete.obs")),
    bias = mean(model_error, na.rm = TRUE),
    .groups = "drop"
  )

# Fantasy-tier diagnostics.
tier_metrics <- preds |>
  dplyr::group_by(position, actual_tier) |>
  dplyr::summarise(
    n = dplyr::n(),
    MAE = mean(abs_model_error, na.rm = TRUE),
    RMSE = sqrt(mean(model_error^2, na.rm = TRUE)),
    bias = mean(model_error, na.rm = TRUE),
    avg_actual_fppg = mean(target_fppg, na.rm = TRUE),
    avg_predicted_fppg = mean(predicted_fppg, na.rm = TRUE),
    .groups = "drop"
  )

cohort_metrics <- preds |>
  dplyr::group_by(position, cohort) |>
  dplyr::summarise(
    n = dplyr::n(),
    MAE = mean(abs_model_error, na.rm = TRUE),
    RMSE = sqrt(mean(model_error^2, na.rm = TRUE)),
    bias = mean(model_error, na.rm = TRUE),
    correlation = suppressWarnings(stats::cor(predicted_fppg, target_fppg, use = "complete.obs")),
    .groups = "drop"
  )

career_metrics <- preds |>
  dplyr::group_by(position, career_stage) |>
  dplyr::summarise(
    n = dplyr::n(),
    MAE = mean(abs_model_error, na.rm = TRUE),
    RMSE = sqrt(mean(model_error^2, na.rm = TRUE)),
    bias = mean(model_error, na.rm = TRUE),
    .groups = "drop"
  )

# FPPG calibration buckets after the honest rolling correction.
calibration <- preds |>
  dplyr::group_by(position, calibration_bucket) |>
  dplyr::summarise(
    n = dplyr::n(),
    avg_predicted_fppg = mean(predicted_fppg, na.rm = TRUE),
    avg_actual_fppg = mean(target_fppg, na.rm = TRUE),
    calibration_error = avg_predicted_fppg - avg_actual_fppg,
    MAE = mean(abs_model_error, na.rm = TRUE),
    .groups = "drop"
  )

# Ranking quality at V6 elite/replacement cutoffs.
rank_metrics <- preds |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    n = dplyr::n(),
    spearman_rank_correlation = suppressWarnings(stats::cor(predicted_position_rank, actual_position_rank, method = "spearman", use = "complete.obs")),
    mean_abs_rank_error = mean(abs_rank_error, na.rm = TRUE),
    elite_recall_pct = 100 * sum(actual_position_rank <= elite_cutoff & predicted_position_rank <= elite_cutoff, na.rm = TRUE) /
      pmax(1, sum(actual_position_rank <= elite_cutoff, na.rm = TRUE)),
    starter_recall_pct = 100 * sum(actual_position_rank <= replacement_cutoff & predicted_position_rank <= replacement_cutoff, na.rm = TRUE) /
      pmax(1, sum(actual_position_rank <= replacement_cutoff, na.rm = TRUE)),
    .groups = "drop"
  )

# Fantasy-specific top-N recall/precision.
fantasy_tier_rows <- list()
for (pos in POSITIONS) {
  d <- preds |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next
  cutoffs <- FANTASY_TIER_CUTOFFS[[pos]]
  for (nm in names(cutoffs)) {
    k <- as.integer(cutoffs[[nm]])
    actual_hit <- d$actual_position_rank <= k
    predicted_hit <- d$predicted_position_rank <= k
    tp <- sum(actual_hit & predicted_hit, na.rm = TRUE)
    fantasy_tier_rows[[length(fantasy_tier_rows) + 1]] <- data.frame(
      position = pos,
      fantasy_tier = nm,
      cutoff = k,
      actual_slots = sum(actual_hit, na.rm = TRUE),
      predicted_slots = sum(predicted_hit, na.rm = TRUE),
      true_positives = tp,
      recall_pct = 100 * tp / pmax(1, sum(actual_hit, na.rm = TRUE)),
      precision_pct = 100 * tp / pmax(1, sum(predicted_hit, na.rm = TRUE))
    )
  }
}
fantasy_tier_metrics <- dplyr::bind_rows(fantasy_tier_rows)

# Decision-model quality: ranking discrimination and probability error.
probability_metrics <- preds |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    n = dplyr::n(),
    elite_AUC = binary_auc(actual_elite, elite_probability),
    elite_Brier = mean((elite_probability - actual_elite)^2, na.rm = TRUE),
    elite_recall_at_50 = 100 * sum(actual_elite == 1 & elite_probability >= 0.50, na.rm = TRUE) / pmax(1, sum(actual_elite == 1, na.rm = TRUE)),
    elite_precision_at_50 = 100 * sum(actual_elite == 1 & elite_probability >= 0.50, na.rm = TRUE) / pmax(1, sum(elite_probability >= 0.50, na.rm = TRUE)),
    starter_AUC = binary_auc(actual_starter, starter_probability),
    starter_Brier = mean((starter_probability - actual_starter)^2, na.rm = TRUE),
    starter_recall_at_50 = 100 * sum(actual_starter == 1 & starter_probability >= 0.50, na.rm = TRUE) / pmax(1, sum(actual_starter == 1, na.rm = TRUE)),
    starter_precision_at_50 = 100 * sum(actual_starter == 1 & starter_probability >= 0.50, na.rm = TRUE) / pmax(1, sum(starter_probability >= 0.50, na.rm = TRUE)),
    breakout_AUC = binary_auc(actual_breakout, breakout_probability),
    breakout_Brier = mean((breakout_probability - actual_breakout)^2, na.rm = TRUE),
    breakout_recall_at_50 = 100 * sum(actual_breakout == 1 & breakout_probability >= 0.50, na.rm = TRUE) / pmax(1, sum(actual_breakout == 1, na.rm = TRUE)),
    breakout_precision_at_50 = 100 * sum(actual_breakout == 1 & breakout_probability >= 0.50, na.rm = TRUE) / pmax(1, sum(breakout_probability >= 0.50, na.rm = TRUE)),
    .groups = "drop"
  )

probability_long <- dplyr::bind_rows(
  preds |> dplyr::transmute(position, event = "Elite", probability = elite_probability, actual = actual_elite),
  preds |> dplyr::transmute(position, event = "Starter+", probability = starter_probability, actual = actual_starter),
  preds |> dplyr::transmute(position, event = "Breakout/Upside", probability = breakout_probability, actual = actual_breakout)
) |>
  dplyr::mutate(probability_bucket = cut(
    probability,
    breaks = c(-Inf, .20, .40, .60, .80, Inf),
    labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
    right = FALSE
  ))

probability_calibration <- probability_long |>
  dplyr::group_by(position, event, probability_bucket) |>
  dplyr::summarise(
    n = dplyr::n(),
    avg_predicted_probability = mean(probability, na.rm = TRUE),
    actual_event_rate = mean(actual, na.rm = TRUE),
    probability_calibration_error = avg_predicted_probability - actual_event_rate,
    .groups = "drop"
  )

# Legacy breakout metrics retained for easy comparison with V6.
breakout_metrics <- preds |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    actual_breakouts = sum(actual_breakout == 1, na.rm = TRUE),
    predicted_breakouts = sum(breakout_probability >= 0.50, na.rm = TRUE),
    true_positives = sum(actual_breakout == 1 & breakout_probability >= 0.50, na.rm = TRUE),
    breakout_recall_pct = 100 * true_positives / pmax(1, actual_breakouts),
    breakout_precision_pct = 100 * true_positives / pmax(1, predicted_breakouts),
    .groups = "drop"
  )

# Largest historical misses for manual football review.
biggest_misses <- preds |>
  dplyr::arrange(dplyr::desc(abs_model_error)) |>
  dplyr::select(
    target_year, player_id, player_display_name, position, team,
    age, experience, is_rookie, actual_tier,
    target_fppg, predicted_fppg, raw_predicted_fppg, model_only_fppg, baseline_fppg,
    model_error, abs_model_error, actual_position_rank, predicted_position_rank,
    elite_probability, starter_probability, breakout_probability,
    model_disagreement_sd, selected_model_weight,
    calibration_intercept, calibration_slope
  ) |>
  utils::head(100)

readr::write_csv(preds, "output/validation_predictions.csv")
readr::write_csv(metrics, "output/validation_metrics.csv")
readr::write_csv(calibration_effect, "output/validation_calibration_effect.csv")
readr::write_csv(year_metrics, "output/validation_metrics_by_year.csv")
readr::write_csv(tier_metrics, "output/validation_tier_metrics.csv")
readr::write_csv(cohort_metrics, "output/validation_rookie_veteran.csv")
readr::write_csv(career_metrics, "output/validation_career_stage.csv")
readr::write_csv(calibration, "output/validation_calibration.csv")
readr::write_csv(rank_metrics, "output/validation_rank_metrics.csv")
readr::write_csv(fantasy_tier_metrics, "output/validation_fantasy_tier_recall.csv")
readr::write_csv(probability_metrics, "output/validation_probability_metrics.csv")
readr::write_csv(probability_calibration, "output/validation_probability_calibration.csv")
readr::write_csv(breakout_metrics, "output/validation_breakout_metrics.csv")
readr::write_csv(biggest_misses, "output/validation_biggest_misses.csv")

# Compare 1.2 against both the locked 1.0 baseline and the observed 1.1 release.
baseline_1_0 <- data.frame(
  position = c("QB", "RB", "TE", "WR"),
  v1_0_MAE = c(3.9561611696953505, 2.5850676216082147, 1.5256017893826435, 2.1050147125264087),
  v1_0_RMSE = c(5.103280828536831, 3.3290369328924214, 1.9711031481250074, 2.6767141713706506),
  v1_0_correlation = c(0.6121328655308653, 0.7557527401969868, 0.7645013997773921, 0.7853771496228588),
  v1_0_rank_correlation = c(0.6480846790550591, 0.7369270118291308, 0.7224103089688464, 0.7488189434422485),
  v1_0_bias = c(0.17984358204495157, -0.020600154238384738, -0.000718443627671141, 0.1469837318809034)
)

baseline_1_1 <- data.frame(
  position = c("QB", "RB", "TE", "WR"),
  v1_1_MAE = c(3.86386583128746, 2.6185869810791464, 1.5315487018450495, 2.112090697923645),
  v1_1_RMSE = c(4.948972417039996, 3.3963859441649737, 1.9867502221024123, 2.6812567903280486),
  v1_1_correlation = c(0.6415192954232284, 0.7441977135876053, 0.7604993738021348, 0.7845580643888197),
  v1_1_rank_correlation = c(0.678106728638677, 0.7224124051155755, 0.7106646895298026, 0.7478392647336607),
  v1_1_bias = c(0.2802822814036969, -0.04544737558188601, 0.02580651929365164, 0.12333354205758044)
)

version_comparison <- metrics |>
  dplyr::left_join(baseline_1_0, by = "position") |>
  dplyr::left_join(baseline_1_1, by = "position") |>
  dplyr::mutate(
    MAE_change_vs_1_0_pct = 100 * (v1_0_MAE - model_MAE) / v1_0_MAE,
    RMSE_change_vs_1_0_pct = 100 * (v1_0_RMSE - model_RMSE) / v1_0_RMSE,
    correlation_change_vs_1_0 = correlation - v1_0_correlation,
    rank_correlation_change_vs_1_0 = spearman_rank_correlation - v1_0_rank_correlation,
    abs_bias_change_vs_1_0 = abs(v1_0_bias) - abs(bias),
    MAE_change_vs_1_1_pct = 100 * (v1_1_MAE - model_MAE) / v1_1_MAE,
    RMSE_change_vs_1_1_pct = 100 * (v1_1_RMSE - model_RMSE) / v1_1_RMSE,
    correlation_change_vs_1_1 = correlation - v1_1_correlation,
    rank_correlation_change_vs_1_1 = spearman_rank_correlation - v1_1_rank_correlation,
    abs_bias_change_vs_1_1 = abs(v1_1_bias) - abs(bias)
  )
readr::write_csv(version_comparison, "output/model_version_comparison.csv")

# Plain-text quality summary for phone review.
report_lines <- c(
  paste0("DIRECT 1.2-STYLE BASELINE REPORT - ", CURRENT_SEASON),
  paste0("Walk-forward seasons: ", paste(targets, collapse = ", ")),
  "",
  "POSITION SUMMARY"
)
for (i in seq_len(nrow(metrics))) {
  m <- metrics[i, ]
  w_vec <- selected_weights$selected_model_weight[selected_weights$position == m$position]
  w <- if (length(w_vec) == 0 || !is.finite(w_vec[1])) DEFAULT_MODEL_PREDICTION_WEIGHT else as.numeric(w_vec[1])
  report_lines <- c(report_lines,
    sprintf(
      "%s | n=%d | MAE %.2f vs %.2f baseline (%+.1f%%) | RMSE %.2f vs %.2f (%+.1f%%) | corr %.3f | rank corr %.3f | bias %+.2f | 2026 model weight %.2f",
      m$position, m$n, m$model_MAE, m$baseline_MAE, m$MAE_improvement_pct,
      m$model_RMSE, m$baseline_RMSE, m$RMSE_improvement_pct,
      m$correlation, m$spearman_rank_correlation, m$bias, w
    )
  )
}

report_lines <- c(report_lines, "", "VS MODEL 1.1")
for (i in seq_len(nrow(version_comparison))) {
  v <- version_comparison[i, ]
  report_lines <- c(report_lines,
    sprintf("%s | MAE change %+.1f%% | RMSE change %+.1f%% | corr %+.3f | rank corr %+.3f",
            v$position, v$MAE_change_vs_1_1_pct, v$RMSE_change_vs_1_1_pct,
            v$correlation_change_vs_1_1, v$rank_correlation_change_vs_1_1)
  )
}

report_lines <- c(report_lines, "", "VS LOCKED MODEL 1.0")
for (i in seq_len(nrow(version_comparison))) {
  v <- version_comparison[i, ]
  report_lines <- c(report_lines,
    sprintf("%s | MAE change %+.1f%% | RMSE change %+.1f%% | corr %+.3f | rank corr %+.3f",
            v$position, v$MAE_change_vs_1_0_pct, v$RMSE_change_vs_1_0_pct,
            v$correlation_change_vs_1_0, v$rank_correlation_change_vs_1_0)
  )
}

report_lines <- c(report_lines, "", "CALIBRATION EFFECT")
for (i in seq_len(nrow(calibration_effect))) {
  c1 <- calibration_effect[i, ]
  report_lines <- c(report_lines,
    sprintf("%s | MAE %.2f -> %.2f (%+.1f%%) | bias %+.2f -> %+.2f",
            c1$position, c1$raw_MAE, c1$calibrated_MAE, c1$MAE_change_pct,
            c1$raw_bias, c1$calibrated_bias)
  )
}

report_lines <- c(report_lines, "", "FANTASY DECISION MODELS")
for (i in seq_len(nrow(probability_metrics))) {
  p <- probability_metrics[i, ]
  report_lines <- c(report_lines,
    sprintf("%s | elite AUC %.3f | starter AUC %.3f | breakout/upside AUC %.3f",
            p$position, p$elite_AUC, p$starter_AUC, p$breakout_AUC)
  )
}

report_lines <- c(report_lines, "", "FANTASY TIER RECALL")
for (i in seq_len(nrow(fantasy_tier_metrics))) {
  r <- fantasy_tier_metrics[i, ]
  report_lines <- c(report_lines,
    sprintf("%s %s | recall %.1f%% | precision %.1f%%",
            r$position, r$fantasy_tier, r$recall_pct, r$precision_pct)
  )
}

report_lines <- c(report_lines, "", "See validation_biggest_misses.csv for the 100 largest historical errors.")
writeLines(report_lines, "output/model_quality_report.txt")

cat("\n[VALIDATE] Multi-year direct-FPPG baseline summary:\n")
print(metrics)
cat("\n[VALIDATE] Probability-model summary:\n")
print(probability_metrics)
cat("\n[VALIDATE] Fantasy tier recall:\n")
print(fantasy_tier_metrics)
message("Direct-FPPG guardrail validation + decision diagnostics complete for 2.0 comparison.")
