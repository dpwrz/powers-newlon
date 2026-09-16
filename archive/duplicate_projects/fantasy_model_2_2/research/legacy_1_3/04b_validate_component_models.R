# ============================================================
# STEP 4B - 1.3 WALK-FORWARD COMPONENT + HYBRID VALIDATION
# ============================================================
source("config.R")
source("R/component_engine.R")
ensure_packages(c("dplyr", "readr", "tidyr", "rpart"))

component_path <- "data/processed/component_model_table.csv"
direct_path <- "output/validation_predictions.csv"
if (!file.exists(component_path)) stop("Missing component model table. Run 02b_build_component_data.R first.")
if (!file.exists(direct_path)) stop("Missing 1.2 direct validation predictions. Run 04_validate_models.R first.")

df <- readr::read_csv(component_path, show_col_types = FALSE)
direct <- readr::read_csv(direct_path, show_col_types = FALSE)
# Preserve the 1.2 metrics produced immediately before this script.
if (file.exists("output/validation_metrics.csv")) file.copy("output/validation_metrics.csv", "output/validation_metrics_1_2.csv", overwrite = TRUE)
if (file.exists("output/model_quality_report.txt")) file.copy("output/model_quality_report.txt", "output/model_quality_report_1_2.txt", overwrite = TRUE)

all_targets <- sort(unique(df$season))
all_targets <- all_targets[all_targets >= TRAIN_START + 2 & all_targets <= TRAIN_END]
targets <- tail(all_targets, min(VALIDATION_YEARS, length(all_targets)))
cat("[1.3 VALIDATE] Component walk-forward seasons: ", paste(targets, collapse = ", "), "\n", sep = "")

rows <- list()
component_metric_rows <- list()
for (yr in targets) {
  train <- df |> dplyr::filter(season < yr)
  test <- df |> dplyr::filter(season == yr)
  for (i in seq_along(POSITIONS)) {
    pos <- POSITIONS[i]
    tr <- train |> dplyr::filter(position == pos)
    te <- test |> dplyr::filter(position == pos)
    if (nrow(tr) < 50 || nrow(te) == 0) next
    cat("[1.3 VALIDATE] ", yr, " ", pos, " component system...\n", sep = "")
    models <- fit_component_models(tr, pos, n_trees = COMPONENT_VALIDATION_TREES, seed = SEED + yr + i * 60000)
    if (length(models) == 0) next
    pd <- predict_component_models(models, te)
    comps <- pd$predictions
    names(comps) <- paste0("pred_component_", names(comps))
    te2 <- dplyr::bind_cols(te, comps)

    # component_fantasy_projection expects the component names without prefix.
    plain <- pd$predictions
    derived <- component_fantasy_projection(pos, plain, te)
    te2 <- dplyr::bind_cols(te2, derived)
    te2$target_year <- yr
    rows[[length(rows) + 1]] <- te2

    specs <- get_component_specs(pos)
    for (nm in intersect(names(specs), names(pd$predictions))) {
      target_col <- specs[[nm]]$target
      actual <- suppressWarnings(as.numeric(te[[target_col]]))
      pred <- suppressWarnings(as.numeric(pd$predictions[[nm]]))
      keep <- is.finite(actual) & is.finite(pred)
      if (sum(keep) < 5) next
      component_metric_rows[[length(component_metric_rows) + 1]] <- data.frame(
        target_year = yr, position = pos, component = nm, target_column = target_col,
        n = sum(keep), MAE = mean(abs(pred[keep] - actual[keep])),
        RMSE = sqrt(mean((pred[keep] - actual[keep])^2)),
        correlation = suppressWarnings(stats::cor(pred[keep], actual[keep], use = "complete.obs")),
        component_to_fppg_correlation = suppressWarnings(stats::cor(actual[keep], te$target_fppg[keep], use = "complete.obs")),
        predicted_component_to_fppg_correlation = suppressWarnings(stats::cor(pred[keep], te$target_fppg[keep], use = "complete.obs"))
      )
    }
  }
}
comp_oof <- dplyr::bind_rows(rows)
if (nrow(comp_oof) == 0) stop("1.3 component validation created no predictions.")
component_metrics_by_year <- dplyr::bind_rows(component_metric_rows)
if (nrow(component_metrics_by_year) > 0) {
  readr::write_csv(component_metrics_by_year, "output/validation_component_targets_by_year.csv")
  component_signal <- component_metrics_by_year |>
    dplyr::group_by(position, component, target_column) |>
    dplyr::summarise(
      n = sum(n), MAE = weighted.mean(MAE, n, na.rm = TRUE), RMSE = weighted.mean(RMSE, n, na.rm = TRUE),
      correlation = weighted.mean(correlation, n, na.rm = TRUE),
      component_to_fppg_correlation = weighted.mean(component_to_fppg_correlation, n, na.rm = TRUE),
      predicted_component_to_fppg_correlation = weighted.mean(predicted_component_to_fppg_correlation, n, na.rm = TRUE),
      .groups = "drop"
    )
  readr::write_csv(component_signal, "output/component_signal_correlations.csv")
}

# ------------------------------------------------------------
# Honest rolling calibration of the component-derived FPPG.
# ------------------------------------------------------------
rolling_cal_rows <- list()
for (yr in targets) {
  for (pos in POSITIONS) {
    prior <- comp_oof |> dplyr::filter(position == pos, target_year < yr)
    if (nrow(prior) >= CALIBRATION_MIN_ROWS) {
      params <- fit_linear_calibration(prior$component_fppg_raw, prior$target_fppg)
      basis <- paste(sort(unique(prior$target_year)), collapse = ",")
    } else {
      residual_sd <- if (nrow(prior) >= 2) stats::sd(prior$target_fppg - prior$component_fppg_raw, na.rm = TRUE) else 0
      if (!is.finite(residual_sd)) residual_sd <- 0
      params <- c(intercept = 0, slope = 1, residual_sd = residual_sd, n = nrow(prior))
      basis <- "identity/no-prior-holdout"
    }
    rolling_cal_rows[[length(rolling_cal_rows) + 1]] <- data.frame(
      target_year = yr, position = pos,
      component_cal_intercept = as.numeric(params["intercept"]),
      component_cal_slope = as.numeric(params["slope"]),
      component_cal_residual_sd = as.numeric(params["residual_sd"]),
      component_cal_basis = basis
    )
  }
}
rolling_cal <- dplyr::bind_rows(rolling_cal_rows)
comp_oof <- comp_oof |>
  dplyr::left_join(rolling_cal, by = c("target_year", "position")) |>
  dplyr::mutate(
    component_fppg = apply_linear_calibration(component_fppg_raw, component_cal_intercept, component_cal_slope)
  )
readr::write_csv(rolling_cal, "output/validation_component_rolling_calibration.csv")

# Merge the already leakage-safe 1.2 direct-FPPG predictions.
direct_keep <- direct |>
  dplyr::transmute(
    player_id = as.character(player_id), position = as.character(position), target_year = as.integer(target_year),
    direct_1_2_fppg = as.numeric(predicted_fppg), direct_1_2_raw_fppg = as.numeric(raw_predicted_fppg),
    baseline_fppg = as.numeric(baseline_fppg)
  ) |>
  dplyr::distinct(player_id, position, target_year, .keep_all = TRUE)
comp_oof <- comp_oof |>
  dplyr::mutate(player_id = as.character(player_id), position = as.character(position), target_year = as.integer(target_year)) |>
  dplyr::left_join(direct_keep, by = c("player_id", "position", "target_year")) |>
  dplyr::filter(is.finite(direct_1_2_fppg), is.finite(target_fppg))

# ------------------------------------------------------------
# Honest rolling choice of component weight. First holdout defaults
# to the validated 1.2 direct model; later years may only use earlier
# holdouts to choose how much component architecture to trust.
# ------------------------------------------------------------
rolling_weight_rows <- list()
for (yr in targets) {
  for (pos in POSITIONS) {
    prior <- comp_oof |> dplyr::filter(position == pos, target_year < yr)
    if (nrow(prior) < 20) {
      best_w <- 0
      basis <- "1.2-safe-default"
    } else {
      cand <- lapply(COMPONENT_BLEND_CANDIDATES, function(w) {
        pred <- w * prior$component_fppg + (1 - w) * prior$direct_1_2_fppg
        data.frame(weight = w, MAE = mean(abs(pred - prior$target_fppg), na.rm = TRUE), RMSE = sqrt(mean((pred - prior$target_fppg)^2, na.rm = TRUE)))
      }) |> dplyr::bind_rows() |> dplyr::arrange(MAE, RMSE)
      best_w <- cand$weight[1]
      basis <- paste(sort(unique(prior$target_year)), collapse = ",")
    }
    rolling_weight_rows[[length(rolling_weight_rows) + 1]] <- data.frame(
      target_year = yr, position = pos, rolling_component_weight = best_w,
      rolling_direct_weight = 1 - best_w, basis_years = basis
    )
  }
}
rolling_weights <- dplyr::bind_rows(rolling_weight_rows)
comp_oof <- comp_oof |>
  dplyr::left_join(rolling_weights, by = c("target_year", "position")) |>
  dplyr::mutate(
    predicted_fppg_1_3 = rolling_component_weight * component_fppg + (1 - rolling_component_weight) * direct_1_2_fppg,
    error_1_3 = predicted_fppg_1_3 - target_fppg,
    error_1_2 = direct_1_2_fppg - target_fppg,
    baseline_error = baseline_fppg - target_fppg
  ) |>
  dplyr::group_by(position, target_year) |>
  dplyr::mutate(
    actual_position_rank_1_3 = rank(-target_fppg, ties.method = "min"),
    predicted_position_rank_1_3 = rank(-predicted_fppg_1_3, ties.method = "min"),
    abs_rank_error_1_3 = abs(predicted_position_rank_1_3 - actual_position_rank_1_3)
  ) |>
  dplyr::ungroup()
readr::write_csv(rolling_weights, "output/validation_component_rolling_weights.csv")

# Final 2026 component calibration uses all OOF holdouts.
final_cal_rows <- list()
for (pos in POSITIONS) {
  d <- comp_oof |> dplyr::filter(position == pos)
  params <- fit_linear_calibration(d$component_fppg_raw, d$target_fppg)
  final_cal_rows[[length(final_cal_rows) + 1]] <- data.frame(
    position = pos, intercept = as.numeric(params["intercept"]), slope = as.numeric(params["slope"]),
    residual_sd = as.numeric(params["residual_sd"]), n = as.numeric(params["n"])
  )
}
selected_component_cal <- dplyr::bind_rows(final_cal_rows)
readr::write_csv(selected_component_cal, "output/selected_component_calibration.csv")

# Final 2026 component weights are selected on all honest OOF rows, using the
# future calibration fit that will be available at projection time.
final_weight_rows <- list(); candidate_rows <- list()
for (pos in POSITIONS) {
  d <- comp_oof |> dplyr::filter(position == pos)
  cal <- selected_component_cal |> dplyr::filter(position == pos) |> dplyr::slice(1)
  comp_future <- apply_linear_calibration(d$component_fppg_raw, cal$intercept, cal$slope)
  cand <- lapply(COMPONENT_BLEND_CANDIDATES, function(w) {
    pred <- w * comp_future + (1 - w) * d$direct_1_2_fppg
    data.frame(position = pos, component_weight = w, direct_weight = 1 - w,
               MAE = mean(abs(pred - d$target_fppg), na.rm = TRUE),
               RMSE = sqrt(mean((pred - d$target_fppg)^2, na.rm = TRUE)),
               correlation = suppressWarnings(stats::cor(pred, d$target_fppg, use = "complete.obs")))
  }) |> dplyr::bind_rows() |> dplyr::arrange(MAE, RMSE)
  candidate_rows[[length(candidate_rows) + 1]] <- cand
  best <- cand[1, , drop = FALSE]
  final_weight_rows[[length(final_weight_rows) + 1]] <- data.frame(
    position = pos, selected_component_weight = best$component_weight,
    selected_direct_weight = best$direct_weight, validation_MAE = best$MAE,
    validation_RMSE = best$RMSE, validation_correlation = best$correlation
  )
}
selected_weights <- dplyr::bind_rows(final_weight_rows)
readr::write_csv(dplyr::bind_rows(candidate_rows), "output/validation_component_blend_candidates.csv")
readr::write_csv(selected_weights, "output/selected_component_blend_weights.csv")

# Main honest 1.3 metrics.
metrics <- comp_oof |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    seasons_tested = dplyr::n_distinct(target_year), n = dplyr::n(),
    model_MAE = mean(abs(error_1_3), na.rm = TRUE),
    baseline_MAE = mean(abs(baseline_error), na.rm = TRUE),
    MAE_improvement_pct = 100 * (baseline_MAE - model_MAE) / pmax(0.001, baseline_MAE),
    model_RMSE = sqrt(mean(error_1_3^2, na.rm = TRUE)),
    baseline_RMSE = sqrt(mean(baseline_error^2, na.rm = TRUE)),
    RMSE_improvement_pct = 100 * (baseline_RMSE - model_RMSE) / pmax(0.001, baseline_RMSE),
    correlation = suppressWarnings(stats::cor(predicted_fppg_1_3, target_fppg, use = "complete.obs")),
    spearman_rank_correlation = suppressWarnings(stats::cor(predicted_fppg_1_3, target_fppg, method = "spearman", use = "complete.obs")),
    bias = mean(error_1_3, na.rm = TRUE),
    mean_abs_rank_error = mean(abs_rank_error_1_3, na.rm = TRUE),
    direct_1_2_MAE = mean(abs(error_1_2), na.rm = TRUE),
    direct_1_2_RMSE = sqrt(mean(error_1_2^2, na.rm = TRUE)),
    direct_1_2_correlation = suppressWarnings(stats::cor(direct_1_2_fppg, target_fppg, use = "complete.obs")),
    MAE_change_vs_1_2_pct = 100 * (direct_1_2_MAE - model_MAE) / pmax(0.001, direct_1_2_MAE),
    RMSE_change_vs_1_2_pct = 100 * (direct_1_2_RMSE - model_RMSE) / pmax(0.001, direct_1_2_RMSE),
    correlation_change_vs_1_2 = correlation - direct_1_2_correlation,
    .groups = "drop"
  )
readr::write_csv(metrics, "output/validation_metrics.csv")
readr::write_csv(metrics, "output/validation_metrics_1_3.csv")
readr::write_csv(comp_oof, "output/validation_component_predictions.csv")

# Component-only vs direct vs final hybrid comparison.
comparison <- dplyr::bind_rows(
  comp_oof |> dplyr::transmute(position, target_year, target_fppg, method = "1.2 direct FPPG", prediction = direct_1_2_fppg),
  comp_oof |> dplyr::transmute(position, target_year, target_fppg, method = "1.3 component only (rolling calibrated)", prediction = component_fppg),
  comp_oof |> dplyr::transmute(position, target_year, target_fppg, method = "1.3 hybrid (honest rolling weight)", prediction = predicted_fppg_1_3)
) |>
  dplyr::mutate(error = prediction - target_fppg) |>
  dplyr::group_by(position, method) |>
  dplyr::summarise(n = dplyr::n(), MAE = mean(abs(error), na.rm = TRUE), RMSE = sqrt(mean(error^2, na.rm = TRUE)),
                   correlation = suppressWarnings(stats::cor(prediction, target_fppg, use = "complete.obs")), bias = mean(error, na.rm = TRUE), .groups = "drop")
readr::write_csv(comparison, "output/validation_1_3_model_comparison.csv")

# Fantasy-specific top-N recall/precision using the 1.3 hybrid ranking.
tier_rows <- list()
for (pos in POSITIONS) {
  d <- comp_oof |> dplyr::filter(position == pos)
  cutoffs <- FANTASY_TIER_CUTOFFS[[pos]]
  if (nrow(d) == 0 || is.null(cutoffs)) next
  for (nm in names(cutoffs)) {
    k <- as.integer(cutoffs[[nm]])
    actual_hit <- d$actual_position_rank_1_3 <= k
    pred_hit <- d$predicted_position_rank_1_3 <= k
    tp <- sum(actual_hit & pred_hit, na.rm = TRUE)
    tier_rows[[length(tier_rows) + 1]] <- data.frame(
      position = pos, fantasy_tier = nm, cutoff = k,
      recall_pct = 100 * tp / pmax(1, sum(actual_hit, na.rm = TRUE)),
      precision_pct = 100 * tp / pmax(1, sum(pred_hit, na.rm = TRUE))
    )
  }
}
tier_recall_13 <- dplyr::bind_rows(tier_rows)
if (nrow(tier_recall_13) > 0) readr::write_csv(tier_recall_13, "output/validation_fantasy_tier_recall_1_3.csv")

# Version comparison file for the dashboard.
version_comparison <- metrics |>
  dplyr::transmute(
    position,
    v1_3_MAE = model_MAE, v1_3_RMSE = model_RMSE, v1_3_correlation = correlation,
    v1_2_MAE = direct_1_2_MAE, v1_2_RMSE = direct_1_2_RMSE, v1_2_correlation = direct_1_2_correlation,
    MAE_change_vs_1_2_pct, RMSE_change_vs_1_2_pct, correlation_change_vs_1_2
  )
readr::write_csv(version_comparison, "output/model_version_comparison.csv")

# Human-readable report. Decision-model metrics remain in the preserved 1.2
# report because 1.3 does not replace those classifiers yet.
lines <- c(
  paste0("FANTASY MODEL 1.3 QUALITY REPORT - ", CURRENT_SEASON),
  paste0("Walk-forward seasons: ", paste(targets, collapse = ", ")),
  "Architecture: team volume -> opportunity -> efficiency/TD -> component FPPG -> validated hybrid with 1.2 direct model",
  "",
  "POSITION SUMMARY"
)
for (pos in POSITIONS) {
  m <- metrics |> dplyr::filter(position == pos)
  w <- selected_weights |> dplyr::filter(position == pos)
  if (nrow(m) == 0) next
  lines <- c(lines, sprintf(
    "%s | n=%d | MAE %.2f vs %.2f baseline (%+.1f%%) | RMSE %.2f vs %.2f (%+.1f%%) | corr %.3f | rank corr %.3f | bias %+.2f | 2026 component weight %.2f",
    pos, m$n, m$model_MAE, m$baseline_MAE, m$MAE_improvement_pct,
    m$model_RMSE, m$baseline_RMSE, m$RMSE_improvement_pct,
    m$correlation, m$spearman_rank_correlation, m$bias,
    ifelse(nrow(w) > 0, w$selected_component_weight, DEFAULT_COMPONENT_WEIGHT)
  ))
}
lines <- c(lines, "", "VS MODEL 1.2")
for (pos in POSITIONS) {
  m <- metrics |> dplyr::filter(position == pos)
  if (nrow(m) == 0) next
  lines <- c(lines, sprintf(
    "%s | MAE change %+.1f%% | RMSE change %+.1f%% | corr %+.3f",
    pos, m$MAE_change_vs_1_2_pct, m$RMSE_change_vs_1_2_pct, m$correlation_change_vs_1_2
  ))
}
lines <- c(lines, "", "SELECTED 2026 COMPONENT WEIGHTS")
for (pos in POSITIONS) {
  w <- selected_weights |> dplyr::filter(position == pos)
  if (nrow(w) == 0) next
  lines <- c(lines, sprintf("%s | component %.2f | direct 1.2 %.2f", pos, w$selected_component_weight, w$selected_direct_weight))
}
if (file.exists("output/validation_probability_metrics.csv")) {
  prob <- readr::read_csv("output/validation_probability_metrics.csv", show_col_types = FALSE)
  lines <- c(lines, "", "FANTASY DECISION MODELS (preserved 1.2 classifiers)")
  for (pos in POSITIONS) {
    r <- prob |> dplyr::filter(position == pos)
    if (nrow(r) == 0) next
    lines <- c(lines, sprintf("%s | elite AUC %.3f | starter AUC %.3f | breakout/upside AUC %.3f", pos, r$elite_AUC[1], r$starter_AUC[1], r$breakout_AUC[1]))
  }
}
if (exists("tier_recall_13") && nrow(tier_recall_13) > 0) {
  lines <- c(lines, "", "FANTASY TIER RECALL (1.3 hybrid ranks)")
  for (i in seq_len(nrow(tier_recall_13))) {
    r <- tier_recall_13[i, ]
    lines <- c(lines, sprintf("%s %s | recall %.1f%% | precision %.1f%%", r$position, r$fantasy_tier, r$recall_pct, r$precision_pct))
  }
}
lines <- c(lines, "", "See component_signal_correlations.csv for component-to-fantasy relationships.",
           "See validation_1_3_model_comparison.csv for component-only vs direct vs hybrid performance.")
writeLines(lines, "output/model_quality_report.txt")

cat("\n[1.3 VALIDATE] Selected 2026 component weights:\n")
print(selected_weights)
cat("[1.3 VALIDATE] Quality report written to output/model_quality_report.txt\n")
