# ============================================================
# FANTASY MODEL 2.3.2 - SIGNAL DISCOVERY + ERROR FEEDBACK
# ============================================================
# Runs on top of the completed 2.3 honest OOF store. It does NOT rebuild PBP,
# weekly features, or the 2.1/2.2/2.3 base models.
source("config.R")
ensure_packages(c("dplyr", "readr", "rpart"))
source("R/weekly_engine.R")
source("R/weekly_engine_22.R")
source("R/weekly_engine_23.R")
source("R/weekly_engine_232.R")

dir.create("models", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

oof_path <- "output/weekly_2_3_validation_predictions.csv"
feature_path <- if (file.exists("data/processed/weekly_model_table_2_3.csv")) "data/processed/weekly_model_table_2_3.csv" else "data/processed/weekly_model_table_2_2.csv"
if (!file.exists(oof_path)) stop("Missing ", oof_path, ". Run Fantasy Model 2.3 validation first.")
if (!file.exists(feature_path)) stop("Missing weekly feature table. Run 09_build_weekly_data.R first.")

oof <- readr::read_csv(oof_path, show_col_types = FALSE, progress = FALSE)
weekly <- readr::read_csv(feature_path, show_col_types = FALSE, progress = FALSE)
if (nrow(oof) == 0 || nrow(weekly) == 0) stop("2.3.2 prerequisite data is empty.")

# Attach only PRE-KICKOFF model features that are not already present in the OOF
# file. Current-week stat outcomes are deliberately excluded.
feature_union <- unique(c(
  unlist(lapply(POSITIONS, function(p) get_weekly_features21(p, names(weekly)))),
  unlist(lapply(POSITIONS, function(p) get_features22(p, "full", names(weekly))))
))
keys <- c("season", "week", "player_id")
extra <- setdiff(intersect(feature_union, names(weekly)), names(oof))
context <- weekly |> dplyr::select(dplyr::all_of(unique(c(keys, extra)))) |> dplyr::distinct(season, week, player_id, .keep_all = TRUE)
oof <- oof |> dplyr::left_join(context, by = keys)
oof$season <- as.integer(oof$season); oof$week <- as.integer(oof$week)

validation_years <- sort(unique(oof$season))
cat("[2.3.2] Historical OOF seasons: ", paste(validation_years, collapse = ", "), "\n", sep = "")
cat("[2.3.2] Learning rule: current holdout can use prior-week observed errors, but never current/future outcomes.\n")

rows <- list(); controller_rows <- list(); base_rows <- list(); signal_lab_rolling <- list()
for (yr in validation_years) {
  for (pos in POSITIONS) {
    prior <- oof |> dplyr::filter(position == pos, season < yr)
    cur <- oof |> dplyr::filter(position == pos, season == yr)
    if (nrow(cur) < 15) next

    base_col <- select_base_method232(prior)
    if (!base_col %in% names(cur)) base_col <- "honest_final_fppg"
    cur$base232_fppg <- wk232_num(cur[[base_col]])
    if (nrow(prior) > 0) prior$base232_fppg <- wk232_num(prior[[base_col]])

    cat("[2.3.2] ", yr, " ", pos, " | prior OOF ", nrow(prior), " | base ", base_col, "\n", sep = "")
    base_rows[[length(base_rows) + 1]] <- data.frame(target_year = yr, position = pos, prior_oof_n = nrow(prior), base_column = base_col, stringsAsFactors = FALSE)

    sig <- fit_signal_residual232(prior, pos, "base232_fppg")
    pid <- fit_pid232(prior, pos, "base232_fppg", sig)
    sim <- apply_controller232(cur, pos, sig, pid, "base232_fppg")
    sim$base_method_232 <- base_col
    sim$signal_model_enabled_232 <- isTRUE(sig$enabled)
    sim$pid_enabled_232 <- isTRUE(pid$enabled)
    sim$projected_weekly_fppg_232 <- sim$prediction_232
    sim$error_232 <- wk232_num(sim$actual_fppg) - wk232_num(sim$projected_weekly_fppg_232)
    sim$abs_error_232 <- abs(sim$error_232)
    sim$risk_score_232 <- wk232_num(sim$model_disagreement_23) + 0.75 * abs(wk232_num(sim$feedback_total_correction_232)) + 0.35 * wk232_num(sim$roll3_fppg_sd)

    controller_rows[[length(controller_rows) + 1]] <- data.frame(
      target_year = yr, position = pos, base_column = base_col,
      signal_enabled = isTRUE(sig$enabled), signal_lambda = ifelse(is.null(sig$lambda), NA_real_, sig$lambda),
      signal_inner_gain = ifelse(is.null(sig$inner_gain), 0, sig$inner_gain),
      pid_enabled = isTRUE(pid$enabled), kp = ifelse(is.null(pid$kp), 0, pid$kp),
      ki = ifelse(is.null(pid$ki), 0, pid$ki), kd = ifelse(is.null(pid$kd), 0, pid$kd),
      pid_inner_gain = ifelse(is.null(pid$inner_gain), 0, pid$inner_gain), stringsAsFactors = FALSE
    )
    if (nrow(prior) >= 100) {
      aud <- signal_audit232(prior, pos, "base232_fppg")
      if (nrow(aud)) signal_lab_rolling[[length(signal_lab_rolling) + 1]] <- aud |> dplyr::mutate(target_year = yr)
    }
    rows[[length(rows) + 1]] <- sim
  }
}

val <- dplyr::bind_rows(rows)
if (nrow(val) == 0) stop("2.3.2 produced no validation predictions.")
readr::write_csv(val, "output/weekly_2_3_2_validation_predictions.csv")
readr::write_csv(dplyr::bind_rows(controller_rows), "output/weekly_2_3_2_rolling_controller_parameters.csv")
readr::write_csv(dplyr::bind_rows(base_rows), "output/weekly_2_3_2_rolling_base_selection.csv")
if (length(signal_lab_rolling)) readr::write_csv(dplyr::bind_rows(signal_lab_rolling), "output/weekly_2_3_2_rolling_signal_lab.csv")

# ------------------------------------------------------------
# Honest metrics vs locked 2.3
# ------------------------------------------------------------
metric_rows <- list(); cohort_rows <- list()
for (pos in POSITIONS) {
  d <- val |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next
  starter <- d$starter_cohort %in% TRUE
  m23 <- metric_bundle232(d$actual_fppg, d$honest_final_fppg, starter)
  mb <- metric_bundle232(d$actual_fppg, d$base232_fppg, starter)
  m232 <- metric_bundle232(d$actual_fppg, d$projected_weekly_fppg_232, starter)
  promote <- is.finite(m232$MAE) && m232$MAE < m23$MAE * (1 - WEEKLY_232_PROMOTION_MIN_MAE_GAIN) &&
    m232$RMSE <= m23$RMSE * 1.0025 &&
    (!is.finite(m23$correlation) || !is.finite(m232$correlation) || m232$correlation >= m23$correlation - 0.005)
  metric_rows[[length(metric_rows) + 1]] <- data.frame(
    position = pos, seasons_tested = length(unique(d$season)), n = nrow(d),
    model23_MAE = m23$MAE, selected_base_MAE = mb$MAE, model232_MAE = m232$MAE,
    model23_RMSE = m23$RMSE, model232_RMSE = m232$RMSE,
    model23_correlation = m23$correlation, model232_correlation = m232$correlation,
    model23_rank_correlation = m23$rank_correlation, model232_rank_correlation = m232$rank_correlation,
    model23_starter_MAE = m23$starter_MAE, model232_starter_MAE = m232$starter_MAE,
    MAE_improvement_vs_23_pct = 100 * (m23$MAE - m232$MAE) / m23$MAE,
    RMSE_improvement_vs_23_pct = 100 * (m23$RMSE - m232$RMSE) / m23$RMSE,
    promoted_for_2026 = promote, stringsAsFactors = FALSE
  )
  for (coh in c("All", "Relevant", "Starter")) {
    dd <- d
    if (coh == "Relevant") dd <- dd[dd$relevant_cohort %in% TRUE, , drop = FALSE]
    if (coh == "Starter") dd <- dd[dd$starter_cohort %in% TRUE, , drop = FALSE]
    if (nrow(dd) < 10) next
    mm23 <- metric_bundle232(dd$actual_fppg, dd$honest_final_fppg)
    mm232 <- metric_bundle232(dd$actual_fppg, dd$projected_weekly_fppg_232)
    cohort_rows[[length(cohort_rows) + 1]] <- data.frame(position = pos, cohort = coh, n = nrow(dd),
      model23_MAE = mm23$MAE, model232_MAE = mm232$MAE, model23_RMSE = mm23$RMSE, model232_RMSE = mm232$RMSE,
      model23_correlation = mm23$correlation, model232_correlation = mm232$correlation,
      model23_rank_correlation = mm23$rank_correlation, model232_rank_correlation = mm232$rank_correlation)
  }
}
metrics <- dplyr::bind_rows(metric_rows)
cohorts <- dplyr::bind_rows(cohort_rows)
readr::write_csv(metrics, "output/weekly_2_3_2_validation_metrics.csv")
readr::write_csv(cohorts, "output/weekly_2_3_2_cohort_metrics.csv")

# ------------------------------------------------------------
# Final signal lab + 2026 controller objects using ALL honest OOF history.
# This is future training, not substituted into historical validation.
# ------------------------------------------------------------
final_base_rows <- list(); final_controller_rows <- list(); final_signal_rows <- list(); reason_rows <- list(); conf_rows <- list()
for (pos in POSITIONS) {
  d <- oof |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next
  base_col <- select_base_method232(d)
  if (!base_col %in% names(d)) base_col <- "honest_final_fppg"
  d$base232_fppg <- wk232_num(d[[base_col]])
  d <- engineer_signal_features232(d, "base232_fppg")
  d$residual_target_232 <- wk232_num(d$actual_fppg) - wk232_num(d$base232_fppg)

  audit <- signal_audit232(d, pos, "base232_fppg")
  if (nrow(audit)) {
    final_signal_rows[[length(final_signal_rows) + 1]] <- audit
    top <- utils::head(audit$feature, 12)
    for (nm in top) {
      x <- wk232_num(d[[nm]]); med <- stats::median(x, na.rm = TRUE)
      hi <- x >= med; lo <- x < med
      reason_rows[[length(reason_rows) + 1]] <- data.frame(
        position = pos, feature = nm, median_feature = med,
        mean_residual_high = mean(d$residual_target_232[hi], na.rm = TRUE),
        mean_residual_low = mean(d$residual_target_232[lo], na.rm = TRUE),
        interpretation_high = ifelse(mean(d$residual_target_232[hi], na.rm = TRUE) > 0, "historical undershoot", "historical overshoot"),
        interpretation_low = ifelse(mean(d$residual_target_232[lo], na.rm = TRUE) > 0, "historical undershoot", "historical overshoot"),
        stringsAsFactors = FALSE)
    }
  }

  sig <- fit_signal_residual232(d, pos, "base232_fppg")
  pid <- fit_pid232(d, pos, "base232_fppg", sig)
  saveRDS(sig, paste0("models/weekly_2_3_2_signal_controller_", pos, ".rds"))
  saveRDS(pid, paste0("models/weekly_2_3_2_pid_controller_", pos, ".rds"))
  final_base_rows[[length(final_base_rows) + 1]] <- data.frame(position = pos, base_column = base_col, stringsAsFactors = FALSE)
  final_controller_rows[[length(final_controller_rows) + 1]] <- data.frame(
    position = pos, base_column = base_col, signal_enabled = isTRUE(sig$enabled),
    signal_lambda = ifelse(is.null(sig$lambda), NA_real_, sig$lambda),
    signal_inner_gain = ifelse(is.null(sig$inner_gain), 0, sig$inner_gain),
    signal_features = paste(if (is.null(sig$features)) character() else sig$features, collapse = ";"),
    pid_enabled = isTRUE(pid$enabled), kp = ifelse(is.null(pid$kp), 0, pid$kp),
    ki = ifelse(is.null(pid$ki), 0, pid$ki), kd = ifelse(is.null(pid$kd), 0, pid$kd),
    pid_inner_gain = ifelse(is.null(pid$inner_gain), 0, pid$inner_gain), stringsAsFactors = FALSE)

  # Confidence calibration uses honest 2.3.2 historical error and a risk score
  # combining model disagreement, correction magnitude, and recent volatility.
  vd <- val |> dplyr::filter(position == pos)
  if (nrow(vd) >= 60) {
    q <- stats::quantile(vd$risk_score_232, probs = c(1/3, 2/3), na.rm = TRUE, names = FALSE)
    vd$confidence_232 <- ifelse(vd$risk_score_232 <= q[1], "High", ifelse(vd$risk_score_232 <= q[2], "Medium", "Low"))
    cc <- vd |> dplyr::group_by(confidence_232) |> dplyr::summarise(n = dplyr::n(), expected_abs_error = mean(abs_error_232, na.rm = TRUE), .groups = "drop")
    cc$position <- pos; cc$q33_risk <- q[1]; cc$q67_risk <- q[2]
    conf_rows[[length(conf_rows) + 1]] <- cc |> dplyr::select(position, confidence = confidence_232, n, expected_abs_error, q33_risk, q67_risk)
  }
}

final_base <- dplyr::bind_rows(final_base_rows)
final_ctrl <- dplyr::bind_rows(final_controller_rows)
final_signal <- dplyr::bind_rows(final_signal_rows)
reasons <- dplyr::bind_rows(reason_rows)
confidence <- dplyr::bind_rows(conf_rows)
readr::write_csv(final_base, "output/weekly_2_3_2_selected_base.csv")
readr::write_csv(final_ctrl, "output/weekly_2_3_2_selected_controller.csv")
readr::write_csv(final_signal, "output/weekly_2_3_2_signal_lab.csv")
readr::write_csv(reasons, "output/weekly_2_3_2_error_reason_audit.csv")
readr::write_csv(confidence, "output/weekly_2_3_2_confidence_calibration.csv")

# Promotion table is a hard safety gate for live 2026 application.
promotion <- metrics |> dplyr::select(position, promoted_for_2026, model23_MAE, model232_MAE, model23_RMSE, model232_RMSE, model23_correlation, model232_correlation)
readr::write_csv(promotion, "output/weekly_2_3_2_promotion.csv")

# Honest 2.3.2 residual pool for empirical intervals/probabilities when promoted.
res_pool <- val |> dplyr::transmute(position, season, week, player_id, projected_weekly_fppg_232,
                                    residual = actual_fppg - projected_weekly_fppg_232,
                                    risk_score_232, signal_correction_232, pid_correction_232)
readr::write_csv(res_pool, "output/weekly_2_3_2_residual_pool.csv")

lines <- c(
  paste0("FANTASY MODEL 2.3.2 SIGNAL + FEEDBACK QUALITY REPORT - ", CURRENT_SEASON),
  "", "DESIGN",
  "Feed-forward: qualified pre-kickoff signals predict remaining OOF residual error.",
  "Feedback: bounded decayed PID-like controller uses only each player's PRIOR observed forecast errors.",
  "Safety: integral windup caps, correction caps, inner-season enable gates, and per-position 2.3 promotion guardrail.",
  "Expected fantasy production: structured_neutral_fppg is exposed as expected_fppg_232 and compared with the selected base.",
  "", "POSITION SUMMARY"
)
for (i in seq_len(nrow(metrics))) {
  r <- metrics[i, ]
  lines <- c(lines, sprintf("%s | n=%d | 2.3 MAE %.3f -> 2.3.2 %.3f (%+.2f%%) | RMSE %.3f -> %.3f (%+.2f%%) | corr %.3f -> %.3f | starter MAE %.3f -> %.3f | promote=%s",
    r$position, r$n, r$model23_MAE, r$model232_MAE, r$MAE_improvement_vs_23_pct,
    r$model23_RMSE, r$model232_RMSE, r$RMSE_improvement_vs_23_pct,
    r$model23_correlation, r$model232_correlation, r$model23_starter_MAE, r$model232_starter_MAE,
    ifelse(r$promoted_for_2026, "YES", "NO")))
}
lines <- c(lines, "", "FILES",
  "weekly_2_3_2_signal_lab.csv = strongest stable pre-kickoff signals and residual relationships",
  "weekly_2_3_2_error_reason_audit.csv = where high/low signal states historically led to under/overshoots",
  "weekly_2_3_2_rolling_controller_parameters.csv = honest year-by-year controller decisions",
  "weekly_2_3_2_validation_metrics.csv = 2.3 vs 2.3.2 apples-to-apples metrics",
  "weekly_2_3_2_cohort_metrics.csv = All/Relevant/Starter results",
  "weekly_2_3_2_confidence_calibration.csv = expected error by 2.3.2 risk/confidence bucket",
  "weekly_2_3_2_promotion.csv = hard production safety gate")
writeLines(lines, "output/weekly_2_3_2_model_quality_report.txt")

cat("[2.3.2] Validation complete. Honest rows: ", nrow(val), "\n", sep = "")
print(metrics)
