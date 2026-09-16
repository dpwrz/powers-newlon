# ============================================================
# FANTASY MODEL 2.2 - WEEKLY ACCURACY VALIDATION + TRAINING
# ============================================================
source("config.R")
ensure_packages(c("dplyr", "readr", "rpart"))
source("R/weekly_engine.R")
source("R/weekly_engine_22.R")

dir.create("models", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

path <- "data/processed/weekly_model_table_2_2.csv"
if (!file.exists(path)) stop("Missing 2.2 weekly feature table. Run 09_build_weekly_data.R first.")
df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
if (nrow(df) == 0) stop("2.2 weekly feature table is empty.")

# Preserve a completed 2.1 report before 2.2 publishes the compatibility paths.
if (file.exists("output/weekly_validation_metrics.csv") && !file.exists("output/weekly_validation_metrics_2_1.csv")) {
  old <- tryCatch(readr::read_csv("output/weekly_validation_metrics.csv", show_col_types = FALSE), error = function(e) data.frame())
  if (nrow(old) > 0 && "weekly_model_weight" %in% names(old)) readr::write_csv(old, "output/weekly_validation_metrics_2_1.csv")
}
if (file.exists("output/weekly_model_quality_report.txt") && !file.exists("output/weekly_model_quality_report_2_1.txt")) {
  file.copy("output/weekly_model_quality_report.txt", "output/weekly_model_quality_report_2_1.txt", overwrite = FALSE)
}

available_years <- sort(unique(as.integer(df$season)))
validation_years <- utils::tail(available_years[available_years <= TRAIN_END], WEEKLY_22_VALIDATION_YEARS)
if (length(validation_years) < 2) stop("Not enough seasons for 2.2 weekly walk-forward validation.")
cat("[2.2 VALIDATE] Holdout seasons: ", paste(validation_years, collapse = ", "), "\n", sep = "")

rows <- list(); rolling_stack_rows <- list(); opportunity_rows <- list()

for (yr in validation_years) {
  for (i in seq_along(POSITIONS)) {
    pos <- POSITIONS[i]
    tr <- df |> dplyr::filter(position == pos, season < yr)
    te <- df |> dplyr::filter(position == pos, season == yr)
    if (nrow(tr) < WEEKLY_22_MIN_TRAIN_ROWS || nrow(te) < 15) next

    cat("[2.2 VALIDATE] ", yr, " ", pos, " | train ", nrow(tr), " | test ", nrow(te), "\n", sep = "")

    # Pregame prior baseline.
    te$baseline_weekly_fppg <- weekly_baseline21(te$preseason_prior_fppg, te$roll3_fppg, te$games_played_prior)

    # A. Neutral role-only direct model. No opponent or betting environment.
    neutral_features <- get_features22(pos, "neutral", names(tr))
    if (length(neutral_features) < 8) stop("Too few 2.2 neutral features for ", pos)
    neutral_fit <- fit_fantasy_model(
      tr, neutral_features, "weekly_fppg",
      seed = SEED + yr * 100 + i,
      n_trees = WEEKLY_22_VALIDATION_TREES
    )
    te$neutral_direct_fppg <- pmax(0, predict_fantasy_model(neutral_fit, te))

    # B. Opportunity-first structured stat line. Game environment may affect
    # opportunity, while efficiency is shrunk toward the preseason prior.
    structured_fit <- fit_structured22(tr, pos, validation = TRUE, seed = SEED + yr * 200 + i)
    st <- predict_structured22(structured_fit, te)
    for (nm in names(st)) te[[nm]] <- st[[nm]]

    # C. Matchup delta is learned only from cross-fitted historical residuals.
    matchup_fit <- fit_matchup_model22(tr, pos, seed = SEED + yr * 300 + i, validation = TRUE)
    te$matchup_delta_22 <- predict_matchup_delta22(matchup_fit, te, pos)
    te$structured_matchup_fppg <- pmax(0, te$structured_neutral_fppg + te$matchup_delta_22)

    # D. Full direct model lets the data test whether a monolithic weekly model
    # still contains signal the decomposed architecture misses.
    full_features <- get_features22(pos, "full", names(tr))
    if (length(full_features) < 10) stop("Too few 2.2 full features for ", pos)
    direct_fit <- fit_fantasy_model(
      tr, full_features, "weekly_fppg",
      seed = SEED + yr * 400 + i,
      n_trees = WEEKLY_22_VALIDATION_TREES
    )
    te$direct_full_fppg <- pmax(0, predict_fantasy_model(direct_fit, te))

    # Honest architecture selection. The current holdout season cannot select
    # its own weights. The first holdout therefore begins at 100% prior.
    history <- dplyr::bind_rows(rows)
    prior_pos <- if (nrow(history) > 0) history |> dplyr::filter(position == pos, season < yr) else data.frame()
    if (nrow(prior_pos) >= 40) {
      scored <- score_stack22(prior_pos)
      w <- scored |> dplyr::slice(1) |> dplyr::select(w_prior, w_neutral, w_structured_matchup, w_direct)
    } else {
      w <- safe_start_stack22()
    }
    te$honest_final_fppg <- pmax(0, apply_stack22(te, w))
    rolling_stack_rows[[length(rolling_stack_rows) + 1]] <- data.frame(
      target_year = yr, position = pos, prior_oof_n = nrow(prior_pos), w,
      stringsAsFactors = FALSE
    )

    # Opportunity-target diagnostics are valuable even if a component does not
    # ultimately receive production stack weight.
    specs <- opportunity_specs22(pos)
    for (nm in names(specs)) {
      actual_col <- unname(specs[[nm]]); pred_col <- paste0("projected_", nm)
      if (actual_col %in% names(te) && pred_col %in% names(te)) {
        aa <- wk_num(te[[actual_col]]); pp <- wk_num(te[[pred_col]])
        opportunity_rows[[length(opportunity_rows) + 1]] <- data.frame(
          season = yr, position = pos, component = nm, n = sum(is.finite(aa) & is.finite(pp)),
          MAE = mean(abs(pp - aa), na.rm = TRUE),
          RMSE = sqrt(mean((pp - aa)^2, na.rm = TRUE)),
          correlation = safe_cor21(pp, aa),
          stringsAsFactors = FALSE
        )
      }
    }

    keep_cols <- c(
      "season", "week", "player_id", "player_display_name", "position", "team", "opponent", "weekly_fppg",
      "baseline_weekly_fppg", "neutral_direct_fppg", "structured_neutral_fppg", "matchup_delta_22",
      "structured_matchup_fppg", "direct_full_fppg", "honest_final_fppg",
      "preseason_prior_fppg", "roll3_fppg", "roll5_fppg", "games_played_prior", "role_fppg_trend",
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
if (nrow(oof) == 0) stop("2.2 weekly validation produced no OOF predictions.")
readr::write_csv(oof, "output/weekly_2_2_validation_predictions.csv")
readr::write_csv(dplyr::bind_rows(rolling_stack_rows), "output/weekly_2_2_rolling_stack_weights.csv")
if (length(opportunity_rows) > 0) readr::write_csv(dplyr::bind_rows(opportunity_rows), "output/weekly_2_2_opportunity_metrics_by_year.csv")

# Final 2026 stack weights are selected from all honest OOF architecture
# predictions. These weights are for the future projection only; they are not
# substituted into the historical honest score.
selected_rows <- list(); stack_candidate_rows <- list()
for (pos in POSITIONS) {
  d <- oof |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next
  scored <- score_stack22(d)
  scored$position <- pos
  stack_candidate_rows[[length(stack_candidate_rows) + 1]] <- scored
  selected_rows[[length(selected_rows) + 1]] <- scored |> dplyr::slice(1) |>
    dplyr::select(position, w_prior, w_neutral, w_structured_matchup, w_direct, MAE, RMSE, correlation, rank_correlation, bias)
}
selected <- dplyr::bind_rows(selected_rows)
readr::write_csv(dplyr::bind_rows(stack_candidate_rows), "output/weekly_2_2_stack_candidates.csv")
readr::write_csv(selected, "output/weekly_2_2_selected_stack.csv")

# Architecture ablation on the same OOF sample.
arch_long <- list()
for (pos in POSITIONS) {
  d <- oof |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next
  methods <- c(
    "2.0/role prior" = "baseline_weekly_fppg",
    "2.2 neutral direct" = "neutral_direct_fppg",
    "2.2 structured neutral" = "structured_neutral_fppg",
    "2.2 structured + matchup" = "structured_matchup_fppg",
    "2.2 full direct" = "direct_full_fppg",
    "2.2 honest stack" = "honest_final_fppg"
  )
  for (lab in names(methods)) {
    p <- wk_num(d[[methods[[lab]]]])
    arch_long[[length(arch_long) + 1]] <- data.frame(
      position = pos, method = lab, n = nrow(d),
      MAE = mean(abs(p - d$actual_fppg), na.rm = TRUE),
      RMSE = sqrt(mean((p - d$actual_fppg)^2, na.rm = TRUE)),
      correlation = safe_cor21(p, d$actual_fppg),
      rank_correlation = safe_cor21(p, d$actual_fppg, "spearman"),
      bias = mean(p - d$actual_fppg, na.rm = TRUE), stringsAsFactors = FALSE
    )
  }
}
architecture <- dplyr::bind_rows(arch_long)
readr::write_csv(architecture, "output/weekly_2_2_architecture_comparison.csv")
readr::write_csv(architecture, "output/weekly_2_2_ablation.csv")

# Primary honest metrics use the rolling guardrail, never the hindsight-selected
# final 2026 stack.
metrics <- oof |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    seasons_tested = dplyr::n_distinct(season), n = dplyr::n(),
    baseline_MAE = mean(abs(baseline_weekly_fppg - actual_fppg), na.rm = TRUE),
    final_MAE = mean(abs(honest_final_fppg - actual_fppg), na.rm = TRUE),
    baseline_RMSE = sqrt(mean((baseline_weekly_fppg - actual_fppg)^2, na.rm = TRUE)),
    final_RMSE = sqrt(mean((honest_final_fppg - actual_fppg)^2, na.rm = TRUE)),
    correlation = safe_cor21(honest_final_fppg, actual_fppg),
    rank_correlation = safe_cor21(honest_final_fppg, actual_fppg, "spearman"),
    bias = mean(honest_final_fppg - actual_fppg, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    MAE_improvement_vs_prior_pct = 100 * (baseline_MAE - final_MAE) / pmax(.001, baseline_MAE),
    RMSE_improvement_vs_prior_pct = 100 * (baseline_RMSE - final_RMSE) / pmax(.001, baseline_RMSE)
  )
readr::write_csv(metrics, "output/weekly_2_2_validation_metrics.csv")
# Compatibility path used by the app.
readr::write_csv(metrics, "output/weekly_validation_metrics.csv")

by_year <- oof |>
  dplyr::group_by(position, season) |>
  dplyr::summarise(
    n = dplyr::n(), MAE = mean(abs(honest_final_fppg - actual_fppg), na.rm = TRUE),
    baseline_MAE = mean(abs(baseline_weekly_fppg - actual_fppg), na.rm = TRUE),
    RMSE = sqrt(mean((honest_final_fppg - actual_fppg)^2, na.rm = TRUE)),
    correlation = safe_cor21(honest_final_fppg, actual_fppg), .groups = "drop"
  )
readr::write_csv(by_year, "output/weekly_2_2_validation_by_year.csv")

# Matchup calibration: 2.1 under-reacted to difficult/favorable games, so 2.2
# explicitly audits predicted matchup deltas against actual residual outcomes.
oof$matchup_bucket_22 <- dplyr::case_when(
  oof$matchup_delta_22 >= 1.5 ~ "Favorable",
  oof$matchup_delta_22 <= -1.5 ~ "Difficult",
  TRUE ~ "Neutral"
)
matchup <- oof |>
  dplyr::mutate(actual_delta_vs_structured = actual_fppg - structured_neutral_fppg) |>
  dplyr::group_by(position, matchup_bucket_22) |>
  dplyr::summarise(
    n = dplyr::n(), predicted_matchup_delta = mean(matchup_delta_22, na.rm = TRUE),
    actual_matchup_delta = mean(actual_delta_vs_structured, na.rm = TRUE),
    actual_mean = mean(actual_fppg, na.rm = TRUE), predicted_mean = mean(honest_final_fppg, na.rm = TRUE),
    MAE = mean(abs(honest_final_fppg - actual_fppg), na.rm = TRUE), .groups = "drop"
  )
readr::write_csv(matchup, "output/weekly_2_2_matchup_calibration.csv")
# Compatibility path.
readr::write_csv(matchup, "output/weekly_matchup_validation.csv")

# Fantasy-relevant cohorts are selected only from pregame baseline ranks.
cohort_oof <- add_pregame_cohorts22(oof)
cohort_rows <- list()
for (pos in POSITIONS) {
  d <- cohort_oof |> dplyr::filter(position == pos)
  for (cohort in c("All", "Relevant", "Starter")) {
    x <- if (cohort == "Relevant") d |> dplyr::filter(relevant_cohort) else if (cohort == "Starter") d |> dplyr::filter(starter_cohort) else d
    if (nrow(x) == 0) next
    cohort_rows[[length(cohort_rows) + 1]] <- data.frame(
      position = pos, cohort = cohort, n = nrow(x),
      MAE = mean(abs(x$honest_final_fppg - x$actual_fppg), na.rm = TRUE),
      baseline_MAE = mean(abs(x$baseline_weekly_fppg - x$actual_fppg), na.rm = TRUE),
      RMSE = sqrt(mean((x$honest_final_fppg - x$actual_fppg)^2, na.rm = TRUE)),
      correlation = safe_cor21(x$honest_final_fppg, x$actual_fppg),
      rank_correlation = safe_cor21(x$honest_final_fppg, x$actual_fppg, "spearman"),
      stringsAsFactors = FALSE
    )
  }
}
cohort_metrics <- dplyr::bind_rows(cohort_rows) |>
  dplyr::mutate(MAE_improvement_vs_prior_pct = 100 * (baseline_MAE - MAE) / pmax(.001, baseline_MAE))
readr::write_csv(cohort_metrics, "output/weekly_2_2_cohort_metrics.csv")

# Weekly top-N rank recall: the prediction must identify the actual top fantasy
# finishers among players who were forecast before kickoff.
ranked <- oof |>
  dplyr::group_by(season, week, position) |>
  dplyr::mutate(
    predicted_rank = rank(-honest_final_fppg, ties.method = "min"),
    actual_rank = rank(-actual_fppg, ties.method = "min"),
    cutoff = as.numeric(WEEKLY_22_TOP_PROB_CUTOFF[position]),
    pred_top = predicted_rank <= cutoff, actual_top = actual_rank <= cutoff
  ) |> dplyr::ungroup()
rank_metrics <- ranked |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    n = dplyr::n(),
    topN_recall = sum(pred_top & actual_top, na.rm = TRUE) / pmax(1, sum(actual_top, na.rm = TRUE)),
    topN_precision = sum(pred_top & actual_top, na.rm = TRUE) / pmax(1, sum(pred_top, na.rm = TRUE)),
    .groups = "drop"
  )
readr::write_csv(rank_metrics, "output/weekly_2_2_topn_accuracy.csv")

# Residual pools power the empirical projection distributions used in the app.
residual_pool <- oof |>
  dplyr::mutate(residual = actual_fppg - honest_final_fppg) |>
  dplyr::select(position, season, week, residual)
readr::write_csv(residual_pool, "output/weekly_2_2_residual_pool.csv")

# ------------------------------------------------------------
# Train final 2026 models on all completed historical weeks.
# ------------------------------------------------------------
importance_rows <- list()
for (i in seq_along(POSITIONS)) {
  pos <- POSITIONS[i]
  d <- df |> dplyr::filter(position == pos, season <= TRAIN_END)
  if (nrow(d) < WEEKLY_22_MIN_TRAIN_ROWS) next
  d$baseline_weekly_fppg <- weekly_baseline21(d$preseason_prior_fppg, d$roll3_fppg, d$games_played_prior)

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

  model_families <- list(neutral = neutral_fit, direct = direct_fit, matchup = matchup_fit)
  for (family in names(model_families)) {
    fit <- model_families[[family]]
    if (is.null(fit)) next
    imp <- get_feature_importance(fit)
    if (nrow(imp) > 0) {
      imp$position <- pos; imp$family <- family
      importance_rows[[length(importance_rows) + 1]] <- imp
    }
  }
  # Opportunity model importance is stored per component tree ensemble.
  for (nm in names(structured_fit$models)) {
    imp <- get_feature_importance(structured_fit$models[[nm]])
    if (nrow(imp) > 0) {
      imp$position <- pos; imp$family <- paste0("opportunity_", nm)
      importance_rows[[length(importance_rows) + 1]] <- imp
    }
  }
}
if (length(importance_rows) > 0) {
  imp_all <- dplyr::bind_rows(importance_rows)
  readr::write_csv(imp_all, "output/weekly_2_2_feature_importance.csv")
  readr::write_csv(imp_all, "output/weekly_feature_importance.csv")
}

# Human-readable quality report.
lines <- c(
  paste0("FANTASY MODEL 2.2 WEEKLY QUALITY REPORT - ", CURRENT_SEASON),
  paste0("Historical weekly seasons: ", min(available_years), "-", max(available_years)),
  paste0("Walk-forward holdouts: ", paste(validation_years, collapse = ", ")),
  "Architecture: 2.0 prior -> neutral role -> opportunity/shrunk stat line -> matchup delta -> full direct challenger -> chronological stack",
  "Validation rule: each holdout season is predicted only from earlier seasons; stack weights for that holdout use earlier OOF seasons only.",
  "",
  "POSITION SUMMARY"
)
for (i in seq_len(nrow(metrics))) {
  r <- metrics[i, ]
  lines <- c(lines, sprintf(
    "%s | n=%d | MAE %.2f vs prior %.2f (%+.1f%%) | RMSE %.2f vs %.2f (%+.1f%%) | corr %.3f | rank corr %.3f | bias %+.2f",
    r$position, r$n, r$final_MAE, r$baseline_MAE, r$MAE_improvement_vs_prior_pct,
    r$final_RMSE, r$baseline_RMSE, r$RMSE_improvement_vs_prior_pct,
    r$correlation, r$rank_correlation, r$bias
  ))
}
lines <- c(lines, "", "SELECTED 2026 STACK WEIGHTS")
for (i in seq_len(nrow(selected))) {
  r <- selected[i, ]
  lines <- c(lines, sprintf(
    "%s | prior %.2f | neutral %.2f | structured+matchup %.2f | full direct %.2f | selection MAE %.2f",
    r$position, r$w_prior, r$w_neutral, r$w_structured_matchup, r$w_direct, r$MAE
  ))
}
lines <- c(lines, "", "FANTASY-RELEVANT STARTER COHORT")
starter_rows <- cohort_metrics |> dplyr::filter(cohort == "Starter")
for (i in seq_len(nrow(starter_rows))) {
  r <- starter_rows[i, ]
  lines <- c(lines, sprintf("%s | n=%d | starter MAE %.2f vs prior %.2f (%+.1f%%)", r$position, r$n, r$MAE, r$baseline_MAE, r$MAE_improvement_vs_prior_pct))
}
lines <- c(lines, "", "TOP-N WEEKLY IDENTIFICATION")
for (i in seq_len(nrow(rank_metrics))) {
  r <- rank_metrics[i, ]
  lines <- c(lines, sprintf("%s | recall %.1f%% | precision %.1f%%", r$position, 100 * r$topN_recall, 100 * r$topN_precision))
}
lines <- c(lines, "", "See weekly_2_2_architecture_comparison.csv for ablation.", "See weekly_2_2_matchup_calibration.csv for matchup response.", "See weekly_2_2_cohort_metrics.csv for starter/relevant-player accuracy.", "See weekly_2_2_feature_importance.csv for feature-family importance.")
writeLines(lines, "output/weekly_2_2_model_quality_report.txt")
writeLines(lines, "output/weekly_model_quality_report.txt")

cat("[2.2 VALIDATE] Complete. Honest OOF player-weeks: ", nrow(oof), "\n", sep = "")
cat("[2.2 VALIDATE] Final 2026 stack weights saved to output/weekly_2_2_selected_stack.csv\n")
