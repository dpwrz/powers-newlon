# ============================================================
# STEP 4B - FANTASY MODEL 2.0 HONEST WALK-FORWARD VALIDATION
# ============================================================
# Tests:
#   1) opportunity models
#   2) shrinkage efficiency / red-zone TD rates
#   3) structured stat-line FPPG
#   4) rolling residual correction
#   5) rolling validation guardrail against the 1.2 direct model

source("config.R")
source("R/opportunity_engine.R")
ensure_packages(c("dplyr", "readr", "tidyr", "rpart"))
cat("[2.0 HOTFIX 5] Validator loaded: safe start + position-specific residual schemas enabled.\n")

opp_path <- "data/processed/opportunity_model_table_2_0.csv"
direct_path <- "output/validation_predictions.csv"
if (!file.exists(opp_path)) stop("Missing 2.0 opportunity table. Run 02b_build_opportunity_data.R first.")
if (!file.exists(direct_path)) stop("Missing 1.2 direct OOF predictions. Run 04_validate_models.R first.")

df <- readr::read_csv(opp_path, show_col_types = FALSE, progress = FALSE)
direct <- readr::read_csv(direct_path, show_col_types = FALSE, progress = FALSE)
if (file.exists("output/validation_metrics.csv")) file.copy("output/validation_metrics.csv", "output/validation_metrics_1_2_direct.csv", overwrite = TRUE)
if (file.exists("output/model_quality_report.txt")) file.copy("output/model_quality_report.txt", "output/model_quality_report_1_2_direct.txt", overwrite = TRUE)

all_targets <- sort(unique(as.integer(df$season)))
all_targets <- all_targets[all_targets >= TRAIN_START + 2 & all_targets <= TRAIN_END]
targets <- tail(all_targets, min(VALIDATION_YEARS, length(all_targets)))
cat("[2.0 VALIDATE] Walk-forward seasons: ", paste(targets, collapse = ", "), "\n", sep = "")

# Direct validation columns needed by the guardrail/residual layer.
direct_keep <- direct |>
  dplyr::transmute(
    player_id = as.character(player_id),
    position = toupper(as.character(position)),
    target_year = as.integer(target_year),
    direct_1_2_fppg = num20(predicted_fppg),
    direct_1_2_raw_fppg = num20(raw_predicted_fppg),
    baseline_fppg = num20(baseline_fppg),
    elite_probability_1_2 = if ("elite_probability" %in% names(direct)) num20(elite_probability) else 0,
    starter_probability_1_2 = if ("starter_probability" %in% names(direct)) num20(starter_probability) else 0,
    breakout_probability_1_2 = if ("breakout_probability" %in% names(direct)) num20(breakout_probability) else 0
  ) |>
  dplyr::distinct(player_id, position, target_year, .keep_all = TRUE)

rows <- list()
opportunity_metric_rows <- list()
rate_metric_rows <- list()
rolling_param_rows <- list()
rolling_cal_rows <- list()
rolling_weight_rows <- list()

for (yr in targets) {
  train_all <- df |> dplyr::filter(season < yr)
  test_all <- df |> dplyr::filter(season == yr)

  for (i in seq_along(POSITIONS)) {
    pos <- POSITIONS[i]
    tr <- train_all |> dplyr::filter(position == pos)
    te <- test_all |> dplyr::filter(position == pos)
    if (nrow(tr) < 50 || nrow(te) == 0) next
    cat("[2.0 VALIDATE] ", yr, " ", pos, " opportunity -> shrinkage -> residual...\n", sep = "")

    # 1) Opportunity
    opp_models <- fit_opportunity_models20(
      tr, pos,
      n_trees = OPPORTUNITY_VALIDATION_TREES,
      seed = SEED + yr + i * 100000
    )
    if (length(opp_models) == 0) next
    opp_detail <- predict_opportunity_models20(opp_models, te)
    opp_pred <- opp_detail$predictions

    # 2) Shrinkage / red-zone rates, tuned only on seasons before this holdout.
    rate_params <- fit_all_rate_params20(tr, pos)
    rate_params$target_year <- yr
    rolling_param_rows[[length(rolling_param_rows) + 1]] <- rate_params
    rate_pred <- predict_all_rates20(te, pos, rate_params)

    # 3) Structured stat line.
    statline <- compose_statline20(pos, opp_pred, rate_pred, te)
    cur <- dplyr::bind_cols(te, statline)
    cur$target_year <- yr
    cur$position <- pos
    cur$player_id <- as.character(cur$player_id)

    # Keep predicted component columns for diagnostics/residual features.
    for (nm in names(opp_pred)) cur[[paste0("pred_opportunity_", nm)]] <- opp_pred[[nm]]
    for (nm in names(rate_pred)) cur[[paste0("pred_rate_", nm)]] <- rate_pred[[nm]]

    # Direct 1.2 OOF prediction must be joined before the residual model because
    # direct-vs-structured disagreement is a useful residual feature.
    cur <- cur |>
      dplyr::left_join(direct_keep, by = c("player_id", "position", "target_year")) |>
      dplyr::filter(is.finite(direct_1_2_fppg), is.finite(target_fppg))
    if (nrow(cur) == 0) next

    # Rolling calibration of the raw structured projection using only earlier
    # holdouts. This corrects scale/bias without peeking at the current year.
    # Hotfix 4: on the very first holdout/position, `rows` is still empty.
    # bind_rows(list()) has zero columns, so filtering on position/target_year
    # would fail before the intended 1.2 safe-start logic can run.
    if (length(rows) == 0) {
      prior_oof <- tibble::tibble(
        position = character(),
        target_year = integer(),
        opportunity_fppg_raw = double(),
        opportunity_fppg_calibrated = double(),
        opportunity_residual_fppg = double(),
        direct_1_2_fppg = double(),
        target_fppg = double()
      )
    } else {
      prior_oof <- dplyr::bind_rows(rows) |>
        dplyr::filter(position == pos, target_year < yr)
    }
    if (nrow(prior_oof) >= CALIBRATION_MIN_ROWS) {
      cal <- fit_linear_calibration(prior_oof$opportunity_fppg_raw, prior_oof$target_fppg)
      cal_basis <- paste(sort(unique(prior_oof$target_year)), collapse = "+")
    } else {
      cal <- c(intercept = 0, slope = 1, residual_sd = ifelse(nrow(prior_oof) > 1, stats::sd(prior_oof$target_fppg - prior_oof$opportunity_fppg_raw), 0), n = nrow(prior_oof))
      cal_basis <- "identity_safe_start"
    }
    cur$opportunity_fppg_calibrated <- apply_linear_calibration(cur$opportunity_fppg_raw, cal["intercept"], cal["slope"])
    cur$base_direct_gap <- cur$direct_1_2_fppg - cur$opportunity_fppg_calibrated
    rolling_cal_rows[[length(rolling_cal_rows) + 1]] <- data.frame(
      target_year = yr, position = pos, intercept = num20(cal["intercept"]), slope = num20(cal["slope"]),
      residual_sd = num20(cal["residual_sd"]), basis_years = cal_basis
    )

    # 4) Rolling residual model. It only sees prior out-of-sample component errors.
    residual_prediction <- rep(0, nrow(cur))
    if (nrow(prior_oof) >= RESIDUAL_MIN_ROWS) {
      prior_resid <- prior_oof |>
        dplyr::mutate(
          residual_target_20 = target_fppg - opportunity_fppg_calibrated,
          base_direct_gap = direct_1_2_fppg - opportunity_fppg_calibrated
        )
      residual_fit <- fit_residual_model20(
        prior_resid, pos,
        n_trees = RESIDUAL_VALIDATION_TREES,
        seed = SEED + yr + i * 200000
      )
      residual_prediction <- predict_residual20(residual_fit, cur)
    }
    cur$residual_correction_20 <- residual_prediction
    cur$opportunity_residual_fppg <- pmax(0, cur$opportunity_fppg_calibrated + cur$residual_correction_20)

    # 5) Honest rolling guardrail blend. The first holdout uses 100% proven direct
    # model. Later years may add 2.0 only if earlier holdouts earned the weight.
    if (nrow(prior_oof) < 20) {
      best_w <- 0
      weight_basis <- "1.2_safe_start"
    } else {
      cand <- lapply(ARCHITECTURE_BLEND_CANDIDATES, function(w) {
        pred <- w * prior_oof$opportunity_residual_fppg + (1 - w) * prior_oof$direct_1_2_fppg
        data.frame(
          weight = w,
          MAE = mean(abs(pred - prior_oof$target_fppg), na.rm = TRUE),
          RMSE = sqrt(mean((pred - prior_oof$target_fppg)^2, na.rm = TRUE)),
          correlation = suppressWarnings(stats::cor(pred, prior_oof$target_fppg, use = "complete.obs"))
        )
      }) |> dplyr::bind_rows() |> dplyr::arrange(MAE, RMSE, dplyr::desc(correlation))
      best_w <- cand$weight[1]
      weight_basis <- paste(sort(unique(prior_oof$target_year)), collapse = "+")
    }
    cur$rolling_opportunity_weight <- best_w
    cur$rolling_direct_weight <- 1 - best_w
    cur$predicted_fppg_2_0 <- pmax(0, best_w * cur$opportunity_residual_fppg + (1 - best_w) * cur$direct_1_2_fppg)
    rolling_weight_rows[[length(rolling_weight_rows) + 1]] <- data.frame(
      target_year = yr, position = pos,
      rolling_opportunity_weight = best_w,
      rolling_direct_weight = 1 - best_w,
      basis_years = weight_basis
    )

    # Opportunity diagnostics.
    opp_specs <- get_opportunity_specs20(pos)
    for (nm in intersect(names(opp_specs), names(opp_pred))) {
      target_col <- opp_specs[[nm]]$target
      actual <- num20(te[[target_col]])
      pred <- num20(opp_pred[[nm]])
      keep <- is.finite(actual) & is.finite(pred)
      if (sum(keep) < 5) next
      opportunity_metric_rows[[length(opportunity_metric_rows) + 1]] <- data.frame(
        target_year = yr, position = pos, component = nm, target_column = target_col,
        n = sum(keep),
        MAE = mean(abs(pred[keep] - actual[keep])),
        RMSE = sqrt(mean((pred[keep] - actual[keep])^2)),
        correlation = suppressWarnings(stats::cor(pred[keep], actual[keep], use = "complete.obs")),
        actual_to_fppg_correlation = suppressWarnings(stats::cor(actual[keep], te$target_fppg[keep], use = "complete.obs")),
        predicted_to_fppg_correlation = suppressWarnings(stats::cor(pred[keep], te$target_fppg[keep], use = "complete.obs"))
      )
    }

    # Shrinkage diagnostics: how well stable rate estimates predict next-season rates.
    rate_specs <- get_rate_specs20(pos)
    for (nm in intersect(names(rate_specs), names(rate_pred))) {
      sp <- rate_specs[[nm]]
      actual <- num20(te[[sp$target]])
      pred <- num20(rate_pred[[nm]])
      exposure <- num20(te[[sp$target_n]])
      keep <- is.finite(actual) & is.finite(pred) & is.finite(exposure) & exposure > 0
      if (sum(keep) < 5) next
      rate_metric_rows[[length(rate_metric_rows) + 1]] <- data.frame(
        target_year = yr, position = pos, component = nm, kind = sp$kind,
        n = sum(keep),
        MAE = mean(abs(pred[keep] - actual[keep])),
        RMSE = sqrt(mean((pred[keep] - actual[keep])^2)),
        correlation = suppressWarnings(stats::cor(pred[keep], actual[keep], use = "complete.obs")),
        predicted_to_fppg_correlation = suppressWarnings(stats::cor(pred[keep], te$target_fppg[keep], use = "complete.obs"))
      )
    }

    rows[[length(rows) + 1]] <- cur
  }
}

oof <- dplyr::bind_rows(rows)
if (nrow(oof) == 0) stop("2.0 validation produced no out-of-sample predictions.")
readr::write_csv(oof, "output/validation_2_0_predictions.csv")
readr::write_csv(dplyr::bind_rows(rolling_param_rows), "output/validation_2_0_shrinkage_parameters_by_year.csv")
readr::write_csv(dplyr::bind_rows(rolling_cal_rows), "output/validation_2_0_rolling_calibration.csv")
readr::write_csv(dplyr::bind_rows(rolling_weight_rows), "output/validation_2_0_rolling_architecture_weights.csv")

# Aggregate opportunity diagnostics safely.
opp_by_year <- dplyr::bind_rows(opportunity_metric_rows)
if (nrow(opp_by_year) > 0) {
  readr::write_csv(opp_by_year, "output/validation_2_0_opportunity_targets_by_year.csv")
  opp_signal <- opp_by_year |>
    dplyr::group_by(position, component, target_column) |>
    dplyr::summarise(
      MAE = weighted_mean_safe20(MAE, n),
      RMSE = weighted_mean_safe20(RMSE, n),
      correlation = weighted_mean_safe20(correlation, n),
      actual_to_fppg_correlation = weighted_mean_safe20(actual_to_fppg_correlation, n),
      predicted_to_fppg_correlation = weighted_mean_safe20(predicted_to_fppg_correlation, n),
      n = sum(n, na.rm = TRUE), .groups = "drop"
    )
  readr::write_csv(opp_signal, "output/opportunity_signal_correlations.csv")
}

rate_by_year <- dplyr::bind_rows(rate_metric_rows)
if (nrow(rate_by_year) > 0) {
  readr::write_csv(rate_by_year, "output/validation_2_0_shrinkage_rates_by_year.csv")
  rate_signal <- rate_by_year |>
    dplyr::group_by(position, component, kind) |>
    dplyr::summarise(
      MAE = weighted_mean_safe20(MAE, n),
      RMSE = weighted_mean_safe20(RMSE, n),
      correlation = weighted_mean_safe20(correlation, n),
      predicted_to_fppg_correlation = weighted_mean_safe20(predicted_to_fppg_correlation, n),
      n = sum(n, na.rm = TRUE), .groups = "drop"
    )
  readr::write_csv(rate_signal, "output/shrinkage_signal_metrics.csv")
}

# ------------------------------------------------------------
# Fit 2026 calibration and residual correction on all honest OOF rows.
# ------------------------------------------------------------
final_cal_rows <- list()
for (pos in POSITIONS) {
  d <- oof |> dplyr::filter(position == pos)
  cal <- fit_linear_calibration(d$opportunity_fppg_raw, d$target_fppg)
  final_cal_rows[[length(final_cal_rows) + 1]] <- data.frame(
    position = pos, intercept = num20(cal["intercept"]), slope = num20(cal["slope"]),
    residual_sd = num20(cal["residual_sd"]), n = num20(cal["n"])
  )
}
selected_cal <- dplyr::bind_rows(final_cal_rows)
readr::write_csv(selected_cal, "output/selected_2_0_calibration.csv")

# Train final residual models using the calibration that will actually exist in 2026.
for (i in seq_along(POSITIONS)) {
  pos <- POSITIONS[i]
  d <- oof |> dplyr::filter(position == pos)
  cal <- selected_cal |> dplyr::filter(position == pos) |> dplyr::slice(1)
  d$opportunity_fppg_calibrated <- apply_linear_calibration(d$opportunity_fppg_raw, cal$intercept, cal$slope)
  d$base_direct_gap <- d$direct_1_2_fppg - d$opportunity_fppg_calibrated
  d$residual_target_20 <- d$target_fppg - d$opportunity_fppg_calibrated
  fit <- fit_residual_model20(d, pos, n_trees = RESIDUAL_FINAL_TREES, seed = SEED + i * 300000)
  if (!is.null(fit)) saveRDS(fit, paste0("models/residual_model_2_0_", pos, ".rds"))
}

# Final shrinkage parameters for 2026 use the entire historical training window.
final_rate_rows <- list()
for (pos in POSITIONS) {
  d <- df |> dplyr::filter(position == pos)
  if (nrow(d) < 50) next
  final_rate_rows[[length(final_rate_rows) + 1]] <- fit_all_rate_params20(d, pos)
}
selected_rate_params <- dplyr::bind_rows(final_rate_rows)
readr::write_csv(selected_rate_params, "output/selected_2_0_shrinkage_parameters.csv")

# ------------------------------------------------------------
# Select the 2026 guardrail weight using ONLY honest rolling predictions.
# ------------------------------------------------------------
candidate_rows <- list(); selected_rows <- list()
for (pos in POSITIONS) {
  d <- oof |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next
  cand <- lapply(ARCHITECTURE_BLEND_CANDIDATES, function(w) {
    pred <- w * d$opportunity_residual_fppg + (1 - w) * d$direct_1_2_fppg
    data.frame(
      position = pos, opportunity_weight = w, direct_weight = 1 - w,
      MAE = mean(abs(pred - d$target_fppg), na.rm = TRUE),
      RMSE = sqrt(mean((pred - d$target_fppg)^2, na.rm = TRUE)),
      correlation = suppressWarnings(stats::cor(pred, d$target_fppg, use = "complete.obs")),
      bias = mean(pred - d$target_fppg, na.rm = TRUE)
    )
  }) |> dplyr::bind_rows() |> dplyr::arrange(MAE, RMSE, dplyr::desc(correlation))
  candidate_rows[[length(candidate_rows) + 1]] <- cand
  best <- cand[1, , drop = FALSE]
  # The direct model is the proven floor. A non-zero 2.0 weight must at least tie
  # the direct model on MAE after rounding noise; otherwise choose 0.
  direct_row <- cand |> dplyr::filter(opportunity_weight == 0) |> dplyr::slice(1)
  if (best$MAE > direct_row$MAE + 1e-8) best <- direct_row
  selected_rows[[length(selected_rows) + 1]] <- data.frame(
    position = pos,
    selected_opportunity_weight = best$opportunity_weight,
    selected_direct_weight = best$direct_weight,
    validation_MAE = best$MAE,
    validation_RMSE = best$RMSE,
    validation_correlation = best$correlation,
    validation_bias = best$bias
  )
}
selected_weights <- dplyr::bind_rows(selected_rows)
readr::write_csv(dplyr::bind_rows(candidate_rows), "output/validation_2_0_architecture_blend_candidates.csv")
readr::write_csv(selected_weights, "output/selected_2_0_architecture_weights.csv")

# ------------------------------------------------------------
# Honest 2.0 metrics from rolling guardrail weights.
# ------------------------------------------------------------
oof <- oof |>
  dplyr::mutate(
    model_error = predicted_fppg_2_0 - target_fppg,
    direct_error = direct_1_2_fppg - target_fppg,
    baseline_error = baseline_fppg - target_fppg
  ) |>
  dplyr::group_by(position, target_year) |>
  dplyr::mutate(
    actual_position_rank = rank(-target_fppg, ties.method = "min"),
    predicted_position_rank = rank(-predicted_fppg_2_0, ties.method = "min"),
    abs_rank_error = abs(predicted_position_rank - actual_position_rank)
  ) |>
  dplyr::ungroup()

metrics <- oof |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    seasons_tested = dplyr::n_distinct(target_year), n = dplyr::n(),
    model_MAE = mean(abs(model_error), na.rm = TRUE),
    baseline_MAE = mean(abs(baseline_error), na.rm = TRUE),
    MAE_improvement_pct = 100 * (baseline_MAE - model_MAE) / pmax(0.001, baseline_MAE),
    model_RMSE = sqrt(mean(model_error^2, na.rm = TRUE)),
    baseline_RMSE = sqrt(mean(baseline_error^2, na.rm = TRUE)),
    RMSE_improvement_pct = 100 * (baseline_RMSE - model_RMSE) / pmax(0.001, baseline_RMSE),
    correlation = suppressWarnings(stats::cor(predicted_fppg_2_0, target_fppg, use = "complete.obs")),
    spearman_rank_correlation = suppressWarnings(stats::cor(predicted_fppg_2_0, target_fppg, method = "spearman", use = "complete.obs")),
    bias = mean(model_error, na.rm = TRUE),
    mean_abs_rank_error = mean(abs_rank_error, na.rm = TRUE),
    direct_1_2_MAE = mean(abs(direct_error), na.rm = TRUE),
    direct_1_2_RMSE = sqrt(mean(direct_error^2, na.rm = TRUE)),
    direct_1_2_correlation = suppressWarnings(stats::cor(direct_1_2_fppg, target_fppg, use = "complete.obs")),
    direct_1_2_rank_correlation = suppressWarnings(stats::cor(direct_1_2_fppg, target_fppg, method = "spearman", use = "complete.obs")),
    direct_1_2_bias = mean(direct_error, na.rm = TRUE),
    MAE_change_vs_1_2_pct = 100 * (direct_1_2_MAE - model_MAE) / pmax(0.001, direct_1_2_MAE),
    RMSE_change_vs_1_2_pct = 100 * (direct_1_2_RMSE - model_RMSE) / pmax(0.001, direct_1_2_RMSE),
    correlation_change_vs_1_2 = correlation - direct_1_2_correlation,
    rank_correlation_change_vs_1_2 = spearman_rank_correlation - direct_1_2_rank_correlation,
    .groups = "drop"
  )
readr::write_csv(metrics, "output/validation_metrics.csv")
readr::write_csv(metrics, "output/validation_metrics_2_0.csv")

# Method comparison separates where improvement comes from.
method_comparison <- dplyr::bind_rows(
  oof |> dplyr::transmute(position, target_year, target_fppg, method = "1.2 direct FPPG", prediction = direct_1_2_fppg),
  oof |> dplyr::transmute(position, target_year, target_fppg, method = "2.0 structured base", prediction = opportunity_fppg_calibrated),
  oof |> dplyr::transmute(position, target_year, target_fppg, method = "2.0 + residual correction", prediction = opportunity_residual_fppg),
  oof |> dplyr::transmute(position, target_year, target_fppg, method = "2.0 honest guardrail", prediction = predicted_fppg_2_0)
) |>
  dplyr::mutate(error = prediction - target_fppg) |>
  dplyr::group_by(position, method) |>
  dplyr::summarise(
    n = dplyr::n(), MAE = mean(abs(error), na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    correlation = suppressWarnings(stats::cor(prediction, target_fppg, use = "complete.obs")),
    bias = mean(error, na.rm = TRUE), .groups = "drop"
  )
readr::write_csv(method_comparison, "output/validation_2_0_model_comparison.csv")

# Fantasy top-N rank recall from 2.0 honest predictions.
tier_rows <- list()
for (pos in POSITIONS) {
  d <- oof |> dplyr::filter(position == pos)
  cuts <- FANTASY_TIER_CUTOFFS[[pos]]
  if (nrow(d) == 0 || is.null(cuts)) next
  for (nm in names(cuts)) {
    k <- as.integer(cuts[[nm]])
    actual_hit <- d$actual_position_rank <= k
    pred_hit <- d$predicted_position_rank <= k
    tp <- sum(actual_hit & pred_hit, na.rm = TRUE)
    tier_rows[[length(tier_rows) + 1]] <- data.frame(
      position = pos, fantasy_tier = nm, cutoff = k,
      recall_pct = 100 * tp / pmax(1, sum(actual_hit, na.rm = TRUE)),
      precision_pct = 100 * tp / pmax(1, sum(pred_hit, na.rm = TRUE))
    )
  }
}
tier_recall <- dplyr::bind_rows(tier_rows)
if (nrow(tier_recall) > 0) readr::write_csv(tier_recall, "output/validation_fantasy_tier_recall_2_0.csv")

version_comparison <- metrics |>
  dplyr::transmute(
    position,
    v2_0_MAE = model_MAE, v1_2_MAE = direct_1_2_MAE,
    v2_0_RMSE = model_RMSE, v1_2_RMSE = direct_1_2_RMSE,
    v2_0_correlation = correlation, v1_2_correlation = direct_1_2_correlation,
    v2_0_rank_correlation = spearman_rank_correlation, v1_2_rank_correlation = direct_1_2_rank_correlation,
    MAE_change_vs_1_2_pct, RMSE_change_vs_1_2_pct,
    correlation_change_vs_1_2, rank_correlation_change_vs_1_2
  )
readr::write_csv(version_comparison, "output/model_version_comparison.csv")

# Human-readable report.
lines <- c(
  paste0("FANTASY MODEL 2.0 QUALITY REPORT - ", CURRENT_SEASON),
  paste0("Walk-forward seasons: ", paste(targets, collapse = ", ")),
  "Architecture: team volume -> opportunity -> shrinkage efficiency -> red-zone TD expectation -> stat line -> residual correction -> 1.2 guardrail",
  "",
  "POSITION SUMMARY"
)
for (pos in POSITIONS) {
  m <- metrics |> dplyr::filter(position == pos)
  w <- selected_weights |> dplyr::filter(position == pos)
  if (nrow(m) == 0) next
  lines <- c(lines, sprintf(
    "%s | n=%d | MAE %.2f vs %.2f baseline (%+.1f%%) | RMSE %.2f vs %.2f (%+.1f%%) | corr %.3f | rank corr %.3f | bias %+.2f | 2026 opportunity weight %.2f",
    pos, m$n, m$model_MAE, m$baseline_MAE, m$MAE_improvement_pct,
    m$model_RMSE, m$baseline_RMSE, m$RMSE_improvement_pct,
    m$correlation, m$spearman_rank_correlation, m$bias,
    ifelse(nrow(w) > 0, w$selected_opportunity_weight, 0)
  ))
}
lines <- c(lines, "", "VS VALIDATED MODEL 1.2")
for (pos in POSITIONS) {
  m <- metrics |> dplyr::filter(position == pos)
  if (nrow(m) == 0) next
  lines <- c(lines, sprintf(
    "%s | MAE change %+.1f%% | RMSE change %+.1f%% | corr %+.3f | rank corr %+.3f",
    pos, m$MAE_change_vs_1_2_pct, m$RMSE_change_vs_1_2_pct,
    m$correlation_change_vs_1_2, m$rank_correlation_change_vs_1_2
  ))
}
lines <- c(lines, "", "SELECTED 2026 ARCHITECTURE WEIGHTS")
for (pos in POSITIONS) {
  w <- selected_weights |> dplyr::filter(position == pos)
  if (nrow(w) == 0) next
  lines <- c(lines, sprintf("%s | opportunity/residual %.2f | direct 1.2 %.2f", pos, w$selected_opportunity_weight, w$selected_direct_weight))
}
lines <- c(lines, "", "2.0 intentionally preserves the validated 1.2 elite/starter/breakout classifiers.")
if (nrow(tier_recall) > 0) {
  lines <- c(lines, "", "FANTASY TIER RECALL")
  for (j in seq_len(nrow(tier_recall))) {
    r <- tier_recall[j, ]
    lines <- c(lines, sprintf("%s %s | recall %.1f%% | precision %.1f%%", r$position, r$fantasy_tier, r$recall_pct, r$precision_pct))
  }
}
lines <- c(
  lines, "",
  "See opportunity_signal_correlations.csv for volume/share signal.",
  "See shrinkage_signal_metrics.csv for efficiency/TD stability.",
  "See validation_2_0_model_comparison.csv for direct vs structured vs residual performance."
)
writeLines(lines, "output/model_quality_report.txt")

cat("\n[2.0 VALIDATE] Selected 2026 architecture weights:\n")
print(selected_weights)
cat("[2.0 VALIDATE] Validation complete.\n")
