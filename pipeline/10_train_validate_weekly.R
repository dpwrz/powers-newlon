# ============================================================
# FANTASY MODEL 2.3 - WEEKLY HISTORICAL META-CALIBRATION
# ============================================================
source("config.R")
ensure_packages(c("dplyr", "readr", "rpart"))
source("R/weekly_engine.R")
source("R/weekly_engine_22.R")
source("R/weekly_engine_23.R")

dir.create("models", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

path <- if (file.exists("data/processed/weekly_model_table_2_3.csv")) "data/processed/weekly_model_table_2_3.csv" else "data/processed/weekly_model_table_2_2.csv"
if (!file.exists(path)) stop("Missing weekly feature table. Run 09_build_weekly_data.R first.")
df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
if (nrow(df) == 0) stop("Weekly feature table is empty.")

available_years <- sort(unique(as.integer(df$season)))
validation_years <- utils::tail(available_years[available_years <= TRAIN_END], WEEKLY_23_VALIDATION_YEARS)
if (length(validation_years) < 2) stop("Not enough seasons for 2.3 weekly walk-forward validation.")
cat("[2.3 VALIDATE] Holdout seasons: ", paste(validation_years, collapse = ", "), "\n", sep = "")
cat("[2.3 VALIDATE] Rule: every calibration/weight for a holdout uses prior OOF seasons only.\n")

rows <- list(); rolling_meta_rows <- list(); rolling_version_rows <- list(); rolling_legacy_rows <- list(); rolling_matchup_rows <- list(); rolling_residual_rows <- list(); opportunity_rows <- list()

for (yr in validation_years) {
  for (i in seq_along(POSITIONS)) {
    pos <- POSITIONS[i]
    tr <- df |> dplyr::filter(position == pos, season < yr)
    te <- df |> dplyr::filter(position == pos, season == yr)
    if (nrow(tr) < WEEKLY_22_MIN_TRAIN_ROWS || nrow(te) < 15) next

    cat("[2.3 VALIDATE] ", yr, " ", pos, " | train ", nrow(tr), " | test ", nrow(te), "\n", sep = "")
    te$baseline_weekly_fppg <- weekly_baseline21(te$preseason_prior_fppg, te$roll3_fppg, te$games_played_prior)
    te$week_phase_23 <- wk23_phase(te$week)
    te <- add_pregame_cohorts22(te)

    history <- dplyr::bind_rows(rows)
    prior_pos <- if (nrow(history) > 0) history |> dplyr::filter(position == pos, season < yr) else data.frame()

    # ----------------------------------------------------------
    # A. Reconstruct the 2.1 monolithic weekly challenger.
    # Its blend with the prior is selected phase-by-phase from prior OOF only.
    # ----------------------------------------------------------
    legacy_features <- get_weekly_features21(pos, names(tr))
    if (length(legacy_features) < 8) stop("Too few 2.1 legacy features for ", pos)
    legacy_fit <- fit_fantasy_model(tr, legacy_features, "weekly_fppg", seed = SEED + yr * 50 + i, n_trees = WEEKLY_VALIDATION_TREES)
    te$legacy21_model_fppg <- pmax(0, predict_fantasy_model(legacy_fit, te))
    te$legacy21_honest_fppg <- te$baseline_weekly_fppg
    for (ph in unique(te$week_phase_23)) {
      idx <- te$week_phase_23 == ph
      lw <- select_legacy21_weight23(prior_pos, ph)
      te$legacy21_honest_fppg[idx] <- apply_legacy21_weight23(te[idx, , drop = FALSE], lw)
      rolling_legacy_rows[[length(rolling_legacy_rows) + 1]] <- data.frame(target_year = yr, position = pos, phase = ph, prior_oof_n = nrow(prior_pos), lw, stringsAsFactors = FALSE)
    }

    # ----------------------------------------------------------
    # B. 2.2 neutral / structured / raw matchup / full direct challengers.
    # ----------------------------------------------------------
    neutral_features <- get_features22(pos, "neutral", names(tr))
    neutral_fit <- fit_fantasy_model(tr, neutral_features, "weekly_fppg", seed = SEED + yr * 100 + i, n_trees = WEEKLY_22_VALIDATION_TREES)
    te$neutral_direct_fppg <- pmax(0, predict_fantasy_model(neutral_fit, te))

    structured_fit <- fit_structured22(tr, pos, validation = TRUE, seed = SEED + yr * 200 + i)
    st <- predict_structured22(structured_fit, te)
    for (nm in names(st)) te[[nm]] <- st[[nm]]

    matchup_fit <- fit_matchup_model22(tr, pos, seed = SEED + yr * 300 + i, validation = TRUE)
    te$matchup_delta_22_raw <- predict_matchup_delta22(matchup_fit, te, pos)
    te$structured_matchup_fppg <- pmax(0, te$structured_neutral_fppg + te$matchup_delta_22_raw)

    matchup_cal <- fit_matchup_calibration23(prior_pos, pos)
    te$calibrated_matchup_delta_23 <- apply_matchup_calibration23(te$matchup_delta_22_raw, matchup_cal)
    te$structured_matchup_calibrated_fppg <- pmax(0, te$structured_neutral_fppg + te$calibrated_matchup_delta_23)
    rolling_matchup_rows[[length(rolling_matchup_rows) + 1]] <- data.frame(target_year = yr, matchup_cal, stringsAsFactors = FALSE)

    full_features <- get_features22(pos, "full", names(tr))
    direct_fit <- fit_fantasy_model(tr, full_features, "weekly_fppg", seed = SEED + yr * 400 + i, n_trees = WEEKLY_22_VALIDATION_TREES)
    te$direct_full_fppg <- pmax(0, predict_fantasy_model(direct_fit, te))

    # Reconstruct the raw 2.2 chronological guardrail as an explicit challenger.
    if (nrow(prior_pos) >= 40) {
      w22 <- score_stack22(prior_pos) |> dplyr::slice(1) |> dplyr::select(w_prior, w_neutral, w_structured_matchup, w_direct)
    } else w22 <- safe_start_stack22()
    te$honest22_fppg <- pmax(0, apply_stack22(te, w22))

    # Opportunity diagnostics.
    specs <- opportunity_specs22(pos)
    for (nm in names(specs)) {
      actual_col <- unname(specs[[nm]]); pred_col <- paste0("projected_", nm)
      if (actual_col %in% names(te) && pred_col %in% names(te)) {
        aa <- wk_num(te[[actual_col]]); pp <- wk_num(te[[pred_col]])
        opportunity_rows[[length(opportunity_rows) + 1]] <- data.frame(
          season = yr, position = pos, component = nm, n = sum(is.finite(aa) & is.finite(pp)),
          MAE = mean(abs(pp - aa), na.rm = TRUE), RMSE = sqrt(mean((pp - aa)^2, na.rm = TRUE)),
          correlation = safe_cor21(pp, aa), stringsAsFactors = FALSE
        )
      }
    }

    # ----------------------------------------------------------
    # C. Phase-aware historical meta stack.
    # ----------------------------------------------------------
    te$meta_precal_fppg <- te$baseline_weekly_fppg
    for (ph in unique(te$week_phase_23)) {
      idx <- te$week_phase_23 == ph
      mw <- select_meta_weights23(prior_pos, ph)
      te$meta_precal_fppg[idx] <- pmax(0, apply_meta_stack23(te[idx, , drop = FALSE], mw))
      rolling_meta_rows[[length(rolling_meta_rows) + 1]] <- data.frame(target_year = yr, position = pos, phase = ph, prior_oof_n = nrow(prior_pos), mw, stringsAsFactors = FALSE)
    }
    te$model_disagreement_23 <- model_disagreement23(te)

    # ----------------------------------------------------------
    # D. Guarded residual calibration. It is disabled unless a prior-season
    # inner validation proves that it reduces error.
    # ----------------------------------------------------------
    residual_cal <- fit_residual_calibrator23(prior_pos, pos)
    te$historical_residual_correction_23 <- predict_residual_calibrator23(residual_cal, te, pos)
    te$candidate23_fppg <- pmax(0, te$meta_precal_fppg + te$historical_residual_correction_23)
    rolling_residual_rows[[length(rolling_residual_rows) + 1]] <- data.frame(
      target_year = yr, position = pos, prior_oof_n = nrow(prior_pos), enabled = isTRUE(residual_cal$enabled),
      lambda = ifelse(is.null(residual_cal$lambda), NA_real_, residual_cal$lambda), validation_gain = ifelse(is.null(residual_cal$validation_gain), 0, residual_cal$validation_gain), stringsAsFactors = FALSE
    )

    # ----------------------------------------------------------
    # E. Cross-version guardrail: reconstructed 2.1 vs raw 2.2 vs new 2.3.
    # Again, only prior OOF history can choose the current holdout blend.
    # ----------------------------------------------------------
    te$honest_final_fppg <- te$candidate23_fppg
    for (ph in unique(te$week_phase_23)) {
      idx <- te$week_phase_23 == ph
      vw <- select_version_weights23(prior_pos, ph)
      te$honest_final_fppg[idx] <- pmax(0, apply_version_guard23(te[idx, , drop = FALSE], vw))
      rolling_version_rows[[length(rolling_version_rows) + 1]] <- data.frame(target_year = yr, position = pos, phase = ph, prior_oof_n = nrow(prior_pos), vw, stringsAsFactors = FALSE)
    }

    keep_cols <- c(
      "season", "week", "player_id", "player_display_name", "position", "team", "opponent", "weekly_fppg",
      "week_phase_23", "baseline_rank", "starter_cohort", "relevant_cohort",
      "baseline_weekly_fppg", "legacy21_model_fppg", "legacy21_honest_fppg",
      "neutral_direct_fppg", "structured_neutral_fppg", "matchup_delta_22_raw", "structured_matchup_fppg",
      "calibrated_matchup_delta_23", "structured_matchup_calibrated_fppg", "direct_full_fppg", "honest22_fppg",
      "meta_precal_fppg", "historical_residual_correction_23", "candidate23_fppg", "honest_final_fppg", "model_disagreement_23",
      "preseason_prior_fppg", "roll3_fppg", "roll5_fppg", "games_played_prior", "role_fppg_trend", "season_week",
      "opp_pos_residual_roll4", "implied_team_total", "team_spread_line", "injury_risk",
      "projected_pass_attempts", "projected_pass_yards", "projected_pass_tds", "projected_interceptions",
      "projected_carries", "projected_rush_yards", "projected_rush_tds", "projected_targets",
      "projected_receptions", "projected_rec_yards", "projected_rec_tds"
    )
    keep_cols <- intersect(keep_cols, names(te))
    out <- te[, keep_cols, drop = FALSE]
    names(out)[names(out) == "weekly_fppg"] <- "actual_fppg"
    rows[[length(rows) + 1]] <- out
  }
}

oof <- dplyr::bind_rows(rows)
if (nrow(oof) == 0) stop("2.3 weekly validation produced no OOF predictions.")
readr::write_csv(oof, "output/weekly_2_3_validation_predictions.csv")
readr::write_csv(dplyr::bind_rows(rolling_legacy_rows), "output/weekly_2_3_rolling_legacy_weights.csv")
readr::write_csv(dplyr::bind_rows(rolling_meta_rows), "output/weekly_2_3_rolling_meta_weights.csv")
readr::write_csv(dplyr::bind_rows(rolling_version_rows), "output/weekly_2_3_rolling_version_weights.csv")
readr::write_csv(dplyr::bind_rows(rolling_matchup_rows), "output/weekly_2_3_rolling_matchup_calibration.csv")
readr::write_csv(dplyr::bind_rows(rolling_residual_rows), "output/weekly_2_3_rolling_residual_calibration.csv")
if (length(opportunity_rows) > 0) readr::write_csv(dplyr::bind_rows(opportunity_rows), "output/weekly_2_3_opportunity_metrics_by_year.csv")

# ------------------------------------------------------------
# Final 2026 calibration/weights from the completed honest OOF store.
# These are future-selection parameters, not substituted into honest history.
# ------------------------------------------------------------
phases <- c("ALL", "W1-3", "W4-7", "W8-12", "W13-18")
final_legacy_rows <- list(); final_meta_rows <- list(); final_version_rows <- list(); final_matchup_rows <- list(); residual_final_rows <- list()
selected22_rows <- list()
for (pos in POSITIONS) {
  d <- oof |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next
  for (ph in phases) {
    lw <- select_legacy21_weight23(d, ph); final_legacy_rows[[length(final_legacy_rows) + 1]] <- data.frame(position = pos, phase = ph, lw)
    mw <- select_meta_weights23(d, ph); sc <- if (ph == "ALL") score_meta_stack23(d) |> dplyr::slice(1) else {
      dd <- d[d$week_phase_23 == ph, , drop = FALSE]; if (nrow(dd) >= WEEKLY_23_PHASE_MIN_ROWS) score_meta_stack23(dd) |> dplyr::slice(1) else score_meta_stack23(d) |> dplyr::slice(1)
    }
    final_meta_rows[[length(final_meta_rows) + 1]] <- data.frame(position = pos, phase = ph, mw,
      selection_MAE = if (nrow(sc)) sc$MAE[1] else NA_real_, selection_starter_MAE = if (nrow(sc)) sc$starter_MAE[1] else NA_real_)
    vw <- select_version_weights23(d, ph); sv <- if (ph == "ALL") score_version_guard23(d) |> dplyr::slice(1) else {
      dd <- d[d$week_phase_23 == ph, , drop = FALSE]; if (nrow(dd) >= WEEKLY_23_PHASE_MIN_ROWS) score_version_guard23(dd) |> dplyr::slice(1) else score_version_guard23(d) |> dplyr::slice(1)
    }
    final_version_rows[[length(final_version_rows) + 1]] <- data.frame(position = pos, phase = ph, vw,
      selection_MAE = if (nrow(sv)) sv$MAE[1] else NA_real_, selection_starter_MAE = if (nrow(sv)) sv$starter_MAE[1] else NA_real_)
  }
  final_matchup_rows[[length(final_matchup_rows) + 1]] <- fit_matchup_calibration23(d, pos)
  rcal <- fit_residual_calibrator23(d, pos)
  saveRDS(rcal, paste0("models/weekly_2_3_residual_calibrator_", pos, ".rds"))
  residual_final_rows[[length(residual_final_rows) + 1]] <- data.frame(position = pos, enabled = isTRUE(rcal$enabled), lambda = rcal$lambda, validation_gain = rcal$validation_gain)
  selected22_rows[[length(selected22_rows) + 1]] <- score_stack22(d) |> dplyr::slice(1) |>
    dplyr::mutate(position = pos) |> dplyr::select(position, w_prior, w_neutral, w_structured_matchup, w_direct, MAE, RMSE, correlation, rank_correlation, bias)
}
final_legacy <- dplyr::bind_rows(final_legacy_rows)
final_meta <- dplyr::bind_rows(final_meta_rows)
final_version <- dplyr::bind_rows(final_version_rows)
final_matchup <- dplyr::bind_rows(final_matchup_rows)
selected22 <- dplyr::bind_rows(selected22_rows)
readr::write_csv(final_legacy, "output/weekly_2_3_selected_legacy_weights.csv")
readr::write_csv(final_meta, "output/weekly_2_3_selected_meta_weights.csv")
readr::write_csv(final_version, "output/weekly_2_3_selected_version_weights.csv")
readr::write_csv(final_matchup, "output/weekly_2_3_matchup_calibration_parameters.csv")
readr::write_csv(dplyr::bind_rows(residual_final_rows), "output/weekly_2_3_residual_calibration_summary.csv")
readr::write_csv(selected22, "output/weekly_2_3_selected_22_stack.csv")
# Compatibility for app/research scripts expecting a 2.2 selected stack.
readr::write_csv(selected22, "output/weekly_2_2_selected_stack.csv")

# ------------------------------------------------------------
# Honest evaluation and version comparison.
# ------------------------------------------------------------
methods <- c(
  "2.0/role prior" = "baseline_weekly_fppg",
  "2.1 reconstructed chronological" = "legacy21_honest_fppg",
  "2.2 raw chronological stack" = "honest22_fppg",
  "2.3 calibrated structured" = "structured_matchup_calibrated_fppg",
  "2.3 meta before residual" = "meta_precal_fppg",
  "2.3 meta + guarded residual" = "candidate23_fppg",
  "2.3 final version guardrail" = "honest_final_fppg"
)
comparison_rows <- list()
for (pos in POSITIONS) {
  d <- oof |> dplyr::filter(position == pos)
  for (lab in names(methods)) {
    col <- methods[[lab]]; if (!col %in% names(d)) next
    p <- wk_num(d[[col]])
    comparison_rows[[length(comparison_rows) + 1]] <- data.frame(
      position = pos, method = lab, n = nrow(d), MAE = mean(abs(p - d$actual_fppg), na.rm = TRUE),
      RMSE = sqrt(mean((p - d$actual_fppg)^2, na.rm = TRUE)), correlation = safe_cor21(p, d$actual_fppg),
      rank_correlation = safe_cor21(p, d$actual_fppg, "spearman"), bias = mean(p - d$actual_fppg, na.rm = TRUE), stringsAsFactors = FALSE
    )
  }
}
version_comparison <- dplyr::bind_rows(comparison_rows)
readr::write_csv(version_comparison, "output/weekly_2_3_version_comparison.csv")
readr::write_csv(version_comparison, "output/weekly_2_3_architecture_comparison.csv")

metrics <- oof |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    seasons_tested = dplyr::n_distinct(season), n = dplyr::n(),
    baseline_MAE = mean(abs(baseline_weekly_fppg - actual_fppg), na.rm = TRUE),
    legacy21_MAE = mean(abs(legacy21_honest_fppg - actual_fppg), na.rm = TRUE),
    model22_MAE = mean(abs(honest22_fppg - actual_fppg), na.rm = TRUE),
    final_MAE = mean(abs(honest_final_fppg - actual_fppg), na.rm = TRUE),
    baseline_RMSE = sqrt(mean((baseline_weekly_fppg - actual_fppg)^2, na.rm = TRUE)),
    final_RMSE = sqrt(mean((honest_final_fppg - actual_fppg)^2, na.rm = TRUE)),
    correlation = safe_cor21(honest_final_fppg, actual_fppg), rank_correlation = safe_cor21(honest_final_fppg, actual_fppg, "spearman"),
    bias = mean(honest_final_fppg - actual_fppg, na.rm = TRUE), .groups = "drop"
  ) |>
  dplyr::mutate(
    MAE_improvement_vs_prior_pct = 100 * (baseline_MAE - final_MAE) / pmax(.001, baseline_MAE),
    MAE_improvement_vs_21_pct = 100 * (legacy21_MAE - final_MAE) / pmax(.001, legacy21_MAE),
    MAE_improvement_vs_22_pct = 100 * (model22_MAE - final_MAE) / pmax(.001, model22_MAE),
    RMSE_improvement_vs_prior_pct = 100 * (baseline_RMSE - final_RMSE) / pmax(.001, baseline_RMSE)
  )
readr::write_csv(metrics, "output/weekly_2_3_validation_metrics.csv")
readr::write_csv(metrics, "output/weekly_validation_metrics.csv")

by_year <- oof |> dplyr::group_by(position, season) |> dplyr::summarise(
  n = dplyr::n(), MAE = mean(abs(honest_final_fppg - actual_fppg), na.rm = TRUE),
  legacy21_MAE = mean(abs(legacy21_honest_fppg - actual_fppg), na.rm = TRUE), model22_MAE = mean(abs(honest22_fppg - actual_fppg), na.rm = TRUE),
  RMSE = sqrt(mean((honest_final_fppg - actual_fppg)^2, na.rm = TRUE)), correlation = safe_cor21(honest_final_fppg, actual_fppg), .groups = "drop")
readr::write_csv(by_year, "output/weekly_2_3_validation_by_year.csv")

# Cohorts and phase diagnostics.
cohort_rows <- list()
for (pos in POSITIONS) {
  d <- oof |> dplyr::filter(position == pos)
  for (cohort in c("All", "Relevant", "Starter")) {
    x <- if (cohort == "Relevant") d |> dplyr::filter(relevant_cohort) else if (cohort == "Starter") d |> dplyr::filter(starter_cohort) else d
    if (nrow(x) == 0) next
    cohort_rows[[length(cohort_rows) + 1]] <- data.frame(
      position = pos, cohort = cohort, n = nrow(x), MAE = mean(abs(x$honest_final_fppg - x$actual_fppg), na.rm = TRUE),
      baseline_MAE = mean(abs(x$baseline_weekly_fppg - x$actual_fppg), na.rm = TRUE),
      legacy21_MAE = mean(abs(x$legacy21_honest_fppg - x$actual_fppg), na.rm = TRUE), model22_MAE = mean(abs(x$honest22_fppg - x$actual_fppg), na.rm = TRUE),
      RMSE = sqrt(mean((x$honest_final_fppg - x$actual_fppg)^2, na.rm = TRUE)), correlation = safe_cor21(x$honest_final_fppg, x$actual_fppg),
      rank_correlation = safe_cor21(x$honest_final_fppg, x$actual_fppg, "spearman"), stringsAsFactors = FALSE)
  }
}
cohort_metrics <- dplyr::bind_rows(cohort_rows) |> dplyr::mutate(MAE_improvement_vs_prior_pct = 100 * (baseline_MAE - MAE) / pmax(.001, baseline_MAE))
readr::write_csv(cohort_metrics, "output/weekly_2_3_cohort_metrics.csv")
phase_metrics <- oof |> dplyr::group_by(position, week_phase_23) |> dplyr::summarise(
  n = dplyr::n(), MAE = mean(abs(honest_final_fppg - actual_fppg), na.rm = TRUE), legacy21_MAE = mean(abs(legacy21_honest_fppg - actual_fppg), na.rm = TRUE),
  model22_MAE = mean(abs(honest22_fppg - actual_fppg), na.rm = TRUE), correlation = safe_cor21(honest_final_fppg, actual_fppg), .groups = "drop")
readr::write_csv(phase_metrics, "output/weekly_2_3_validation_by_phase.csv")

# Matchup calibration audit: raw 2.2 delta vs calibrated 2.3 delta.
oof$matchup_bucket_23 <- dplyr::case_when(oof$matchup_delta_22_raw >= 1.5 ~ "Favorable", oof$matchup_delta_22_raw <= -1.5 ~ "Difficult", TRUE ~ "Neutral")
matchup <- oof |> dplyr::mutate(actual_matchup_delta = actual_fppg - structured_neutral_fppg) |> dplyr::group_by(position, matchup_bucket_23) |>
  dplyr::summarise(n = dplyr::n(), raw_predicted_delta = mean(matchup_delta_22_raw, na.rm = TRUE), calibrated_predicted_delta = mean(calibrated_matchup_delta_23, na.rm = TRUE),
    actual_matchup_delta = mean(actual_matchup_delta, na.rm = TRUE), final_MAE = mean(abs(honest_final_fppg - actual_fppg), na.rm = TRUE), .groups = "drop")
readr::write_csv(matchup, "output/weekly_2_3_matchup_calibration.csv")
readr::write_csv(matchup, "output/weekly_matchup_validation.csv")

# Projection-tier audit and model-disagreement confidence calibration.
oof <- oof |> dplyr::group_by(season, week, position) |> dplyr::arrange(dplyr::desc(honest_final_fppg), .by_group = TRUE) |> dplyr::mutate(predicted_rank_23 = dplyr::row_number()) |> dplyr::ungroup()
oof$projection_tier_23 <- dplyr::case_when(oof$starter_cohort ~ "Starter", oof$relevant_cohort ~ "Relevant", TRUE ~ "Fringe")
tier_metrics <- oof |> dplyr::group_by(position, projection_tier_23) |> dplyr::summarise(n = dplyr::n(), MAE = mean(abs(honest_final_fppg - actual_fppg), na.rm = TRUE), correlation = safe_cor21(honest_final_fppg, actual_fppg), .groups = "drop")
readr::write_csv(tier_metrics, "output/weekly_2_3_error_by_projection_tier.csv")
confidence <- build_confidence_calibration23(oof)
readr::write_csv(confidence, "output/weekly_2_3_confidence_calibration.csv")

# Residual pool for live empirical ranges; this is honest final 2.3 error only.
residual_pool <- oof |> dplyr::mutate(residual = actual_fppg - honest_final_fppg) |>
  dplyr::select(position, season, week, week_phase_23, projection_tier_23, model_disagreement_23, honest_final_fppg, residual)
readr::write_csv(residual_pool, "output/weekly_2_3_residual_pool.csv")

# Top-N identification.
ranked <- oof |> dplyr::group_by(season, week, position) |> dplyr::mutate(
  predicted_rank = rank(-honest_final_fppg, ties.method = "min"), actual_rank = rank(-actual_fppg, ties.method = "min"),
  cutoff = as.numeric(WEEKLY_22_TOP_PROB_CUTOFF[position]), pred_top = predicted_rank <= cutoff, actual_top = actual_rank <= cutoff) |> dplyr::ungroup()
rank_metrics <- ranked |> dplyr::group_by(position) |> dplyr::summarise(n = dplyr::n(), topN_recall = sum(pred_top & actual_top, na.rm = TRUE) / pmax(1, sum(actual_top, na.rm = TRUE)),
  topN_precision = sum(pred_top & actual_top, na.rm = TRUE) / pmax(1, sum(pred_top, na.rm = TRUE)), .groups = "drop")
readr::write_csv(rank_metrics, "output/weekly_2_3_topn_accuracy.csv")

# ------------------------------------------------------------
# Train final base models on all completed historical weeks.
# ------------------------------------------------------------
importance_rows <- list()
for (i in seq_along(POSITIONS)) {
  pos <- POSITIONS[i]
  d <- df |> dplyr::filter(position == pos, season <= TRAIN_END)
  if (nrow(d) < WEEKLY_22_MIN_TRAIN_ROWS) next
  d$baseline_weekly_fppg <- weekly_baseline21(d$preseason_prior_fppg, d$roll3_fppg, d$games_played_prior)

  lf <- get_weekly_features21(pos, names(d))
  legacy_fit <- fit_fantasy_model(d, lf, "weekly_fppg", seed = SEED + i * 9000, n_trees = WEEKLY_FINAL_TREES)
  saveRDS(legacy_fit, paste0("models/weekly_model_2_1_", pos, ".rds"))

  nf <- get_features22(pos, "neutral", names(d))
  neutral_fit <- fit_fantasy_model(d, nf, "weekly_fppg", seed = SEED + i * 10000, n_trees = WEEKLY_22_FINAL_TREES)
  saveRDS(neutral_fit, paste0("models/weekly_2_2_neutral_", pos, ".rds"))
  structured_fit <- fit_structured22(d, pos, validation = FALSE, seed = SEED + i * 11000)
  saveRDS(structured_fit, paste0("models/weekly_2_2_structured_", pos, ".rds"))
  matchup_fit <- fit_matchup_model22(d, pos, seed = SEED + i * 12000, validation = FALSE)
  saveRDS(matchup_fit, paste0("models/weekly_2_2_matchup_", pos, ".rds"))
  ff <- get_features22(pos, "full", names(d))
  direct_fit <- fit_fantasy_model(d, ff, "weekly_fppg", seed = SEED + i * 13000, n_trees = WEEKLY_22_FINAL_TREES)
  saveRDS(direct_fit, paste0("models/weekly_2_2_direct_", pos, ".rds"))

  families <- list(legacy21 = legacy_fit, neutral = neutral_fit, direct = direct_fit, matchup = matchup_fit)
  for (family in names(families)) {
    fit <- families[[family]]; if (is.null(fit)) next
    imp <- get_feature_importance(fit)
    if (nrow(imp) > 0) { imp$position <- pos; imp$family <- family; importance_rows[[length(importance_rows) + 1]] <- imp }
  }
  for (nm in names(structured_fit$models)) {
    imp <- get_feature_importance(structured_fit$models[[nm]])
    if (nrow(imp) > 0) { imp$position <- pos; imp$family <- paste0("opportunity_", nm); importance_rows[[length(importance_rows) + 1]] <- imp }
  }
}
if (length(importance_rows) > 0) {
  imp <- dplyr::bind_rows(importance_rows)
  readr::write_csv(imp, "output/weekly_2_3_feature_importance.csv")
  readr::write_csv(imp, "output/weekly_feature_importance.csv")
}

# Quality report.
lines <- c(
  paste0("FANTASY MODEL 2.3 WEEKLY QUALITY REPORT - ", CURRENT_SEASON),
  paste0("Historical weekly seasons: ", min(available_years), "-", max(available_years)),
  paste0("Walk-forward holdouts: ", paste(validation_years, collapse = ", ")),
  "Architecture: 2.0 prior + reconstructed 2.1 challenger + 2.2 decomposed models -> OOF matchup calibration -> phase-aware meta stack -> guarded ridge residual -> cross-version guardrail",
  "Critical rule: every holdout's tuning uses only prior out-of-sample predictions. First holdout safely defaults to the prior.",
  "Development note: 2.3 was designed after inspecting 2.2's 2022-2025 errors; the strongest future evidence will be prospective 2026 weekly performance.",
  "", "POSITION SUMMARY"
)
for (ii in seq_len(nrow(metrics))) {
  r <- metrics[ii, ]
  lines <- c(lines, sprintf("%s | n=%d | 2.3 MAE %.2f | prior %.2f | 2.1 challenger %.2f | 2.2 challenger %.2f | vs prior %+.1f%% | vs 2.1 %+.1f%% | vs 2.2 %+.1f%% | corr %.3f | rank %.3f | bias %+.2f",
    r$position, r$n, r$final_MAE, r$baseline_MAE, r$legacy21_MAE, r$model22_MAE, r$MAE_improvement_vs_prior_pct, r$MAE_improvement_vs_21_pct, r$MAE_improvement_vs_22_pct, r$correlation, r$rank_correlation, r$bias))
}
lines <- c(lines, "", "FINAL 2026 MATCHUP SHRINKAGE")
for (ii in seq_len(nrow(final_matchup))) {
  r <- final_matchup[ii, ]
  lines <- c(lines, sprintf("%s | favorable slope %.2f | difficult slope %.2f | OOF n=%d", r$position, r$positive_slope, r$negative_slope, r$n))
}
lines <- c(lines, "", "STARTER COHORT")
st <- cohort_metrics |> dplyr::filter(cohort == "Starter")
for (ii in seq_len(nrow(st))) {
  r <- st[ii, ]
  lines <- c(lines, sprintf("%s | n=%d | 2.3 MAE %.2f | prior %.2f | 2.1 %.2f | 2.2 %.2f", r$position, r$n, r$MAE, r$baseline_MAE, r$legacy21_MAE, r$model22_MAE))
}
lines <- c(lines, "", "FILES", "weekly_2_3_version_comparison.csv = 2.0/2.1/2.2/2.3 apples-to-apples OOF comparison",
  "weekly_2_3_selected_meta_weights.csv = phase-aware future meta weights", "weekly_2_3_selected_version_weights.csv = cross-version guardrail",
  "weekly_2_3_matchup_calibration.csv = raw vs calibrated matchup delta", "weekly_2_3_confidence_calibration.csv = disagreement-to-error mapping")
writeLines(lines, "output/weekly_2_3_model_quality_report.txt")
writeLines(lines, "output/weekly_model_quality_report.txt")
cat(paste(lines, collapse = "\n"), "\n")
cat("[2.3 VALIDATE] Complete. Honest OOF player-weeks: ", nrow(oof), "\n", sep = "")
