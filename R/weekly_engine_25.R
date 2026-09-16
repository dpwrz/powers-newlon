# ============================================================
# FANTASY MODEL 2.5 - ACCURACY TOURNAMENT ENGINE
# ============================================================
# A modern direct boosted challenger layered behind the current production
# champion. Everything in MODEL25_SAFE_FEATURES must be available before
# kickoff and must also exist in the live projection grid.

MODEL25_SAFE_FEATURES <- c(
  "is_home", "rest_days", "total_line", "team_spread_line", "implied_team_total",
  "preseason_prior_fppg", "preseason_deep_target_rate", "preseason_middle_target_rate",
  "preseason_redzone_target_rate", "prior_catch_rate", "prior_yards_per_target",
  "prior_yards_per_carry", "prior_yards_per_attempt", "prior_pass_td_rate",
  "prior_rush_td_rate", "prior_rec_td_rate", "prior_interception_rate",
  "games_played_prior", "prior_game_fppg", "roll3_fppg", "roll5_fppg",
  "roll3_fppg_sd", "season_to_date_fppg", "roll3_targets", "roll5_targets",
  "roll3_carries", "roll5_carries", "roll3_pass_attempts", "roll5_pass_attempts",
  "roll3_target_share", "roll3_carry_share", "roll3_pass_attempt_share",
  "roll3_offense_pct", "roll5_offense_pct", "roll3_offense_snaps", "snap_trend",
  "role_fppg_trend", "target_trend", "carry_trend", "opportunity_per_snap",
  "season_targets_prior", "season_carries_prior", "season_pass_attempts_prior",
  "roll3_catch_rate", "roll3_ypt", "roll3_rush_ypc", "roll3_pass_ypa",
  "roll3_pass_td_rate", "roll3_interception_rate", "roll3_rush_td_rate",
  "roll3_rec_td_rate", "season_to_date_catch_rate", "season_to_date_ypt",
  "season_to_date_rush_ypc", "season_to_date_pass_ypa",
  "season_to_date_pass_td_rate", "season_to_date_interception_rate",
  "season_to_date_rush_td_rate", "season_to_date_rec_td_rate",
  "roll3_ngs_cpoe", "roll3_ngs_air_yards", "roll3_ngs_time_to_throw",
  "roll3_ngs_aggressiveness", "roll3_ngs_separation", "roll3_ngs_cushion",
  "roll3_ngs_yac_oe", "roll3_ngs_air_yard_share", "roll3_ngs_rush_yoe_pa",
  "roll3_ngs_box_rate", "roll3_ngs_time_to_los", "ngs_available",
  "roll3_team_pass_attempts", "roll3_team_carries", "expected_team_plays",
  "expected_team_pass_rate", "opp_pos_residual_roll4", "opp_pos_residual_roll8",
  "opp_pos_fppg_allowed_roll4", "def_pass_epa_allowed_roll4",
  "def_pass_epa_allowed_roll8", "def_rush_epa_allowed_roll4",
  "def_rush_epa_allowed_roll8", "def_pass_success_allowed_roll4",
  "def_rush_success_allowed_roll4", "def_explosive_pass_rate_roll4",
  "def_deep_pass_rate_roll4", "def_middle_pass_rate_roll4",
  "def_deep_epa_allowed_roll4", "def_middle_epa_allowed_roll4",
  "def_sack_rate_roll4", "def_qb_hit_rate_roll4",
  "def_redzone_pass_td_rate_roll4", "def_redzone_rush_td_rate_roll4",
  "def_wr_residual_roll4", "def_te_residual_roll4",
  "qb_pass_matchup", "qb_pressure_matchup", "rb_rush_matchup", "rb_receiving_matchup",
  "wr_volume_matchup", "wr_deep_matchup", "wr_redzone_matchup",
  "te_middle_matchup", "te_redzone_matchup", "favorite_points", "underdog_points",
  "favorite_rush_interaction", "underdog_target_interaction",
  "matchup_role_interaction", "injury_risk", "practice_risk",
  "team_skill_out_count", "team_skill_questionable_count", "season_week"
)

# Explicitly document known leakage/current-game outcomes that must never enter
# this challenger. The validator checks for these names before fitting.
MODEL25_FORBIDDEN_FEATURES <- c(
  "weekly_fppg", "targets", "receptions", "carries", "pass_attempts",
  "receiving_yards", "rushing_yards", "passing_yards", "receiving_tds",
  "rushing_tds", "passing_tds", "offense_snaps", "offense_pct",
  "target_share_week", "carry_share_week", "pass_attempt_share_week",
  "ngs_cpoe", "ngs_air_yards", "ngs_time_to_throw", "ngs_aggressiveness",
  "ngs_separation", "ngs_cushion", "ngs_yac_oe", "ngs_air_yard_share",
  "ngs_rush_yoe_pa", "ngs_box_rate", "ngs_time_to_los",
  "defense_residual_allowed", "ngs_available_week"
)

MODEL25_ALPHA_GRID <- c(0, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.50)
MODEL25_BOOTSTRAP_ALPHA <- 0.15
MODEL25_EARLY_MAX_ALPHA <- 0.25
MODEL25_MATURE_MAX_ALPHA <- 0.40
# Position-specific final caps are shrinkage guardrails, not fitted outcomes.
MODEL25_FINAL_ALPHA_CAP <- c(QB = 0.20, RB = 0.30, WR = 0.40, TE = 0.25)
MODEL25_ALPHA_SCORE_TOL <- 0.001
MODEL25_BOOTSTRAP_REPS <- 1000L

wk25_num <- function(x, default = 0) {
  z <- suppressWarnings(as.numeric(x))
  z[!is.finite(z)] <- default
  z
}

wk25_bool <- function(x) {
  if (is.logical(x)) return(replace(x, is.na(x), FALSE))
  z <- toupper(trimws(as.character(x)))
  z %in% c("TRUE", "T", "1", "YES", "Y")
}

wk25_safe_cor <- function(x, y, method = "pearson") {
  x <- wk25_num(x, NA_real_); y <- wk25_num(y, NA_real_)
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 5 || stats::sd(x[keep]) == 0 || stats::sd(y[keep]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x[keep], y[keep], method = method))
}

wk25_core_metrics <- function(actual, pred) {
  a <- wk25_num(actual, NA_real_); p <- wk25_num(pred, NA_real_)
  keep <- is.finite(a) & is.finite(p)
  a <- a[keep]; p <- p[keep]
  if (!length(a)) return(data.frame(n = 0L, MAE = NA_real_, RMSE = NA_real_, correlation = NA_real_, rank_correlation = NA_real_))
  data.frame(
    n = length(a),
    MAE = mean(abs(a - p)),
    RMSE = sqrt(mean((a - p)^2)),
    correlation = wk25_safe_cor(a, p, "pearson"),
    rank_correlation = wk25_safe_cor(a, p, "spearman")
  )
}

wk25_cohort_metrics <- function(d, pred_col, actual_col = "actual_fppg") {
  if (!nrow(d)) return(data.frame())
  masks <- list(
    All = rep(TRUE, nrow(d)),
    Relevant = if ("relevant_cohort" %in% names(d)) wk25_bool(d$relevant_cohort) else rep(TRUE, nrow(d)),
    Starter = if ("starter_cohort" %in% names(d)) wk25_bool(d$starter_cohort) else rep(FALSE, nrow(d))
  )
  rows <- lapply(names(masks), function(lbl) {
    z <- d[masks[[lbl]], , drop = FALSE]
    m <- wk25_core_metrics(z[[actual_col]], z[[pred_col]])
    cbind(data.frame(cohort = lbl, stringsAsFactors = FALSE), m)
  })
  dplyr::bind_rows(rows)
}

wk25_compare <- function(d, base_col = "production_base_25", cand_col = "candidate_fppg_25") {
  b <- wk25_cohort_metrics(d, base_col)
  c <- wk25_cohort_metrics(d, cand_col)
  names(b)[names(b) %in% c("n", "MAE", "RMSE", "correlation", "rank_correlation")] <- paste0("base_", names(b)[names(b) %in% c("n", "MAE", "RMSE", "correlation", "rank_correlation")])
  names(c)[names(c) %in% c("n", "MAE", "RMSE", "correlation", "rank_correlation")] <- paste0("candidate_", names(c)[names(c) %in% c("n", "MAE", "RMSE", "correlation", "rank_correlation")])
  out <- dplyr::left_join(b, c, by = "cohort")
  out$MAE_improvement_pct <- 100 * (out$base_MAE - out$candidate_MAE) / pmax(out$base_MAE, 1e-9)
  out$RMSE_improvement_pct <- 100 * (out$base_RMSE - out$candidate_RMSE) / pmax(out$base_RMSE, 1e-9)
  out
}

wk25_prepare_matrix <- function(d, features = MODEL25_SAFE_FEATURES) {
  n <- nrow(d)
  out <- vector("list", length(features)); names(out) <- features
  for (nm in features) {
    if (nm %in% names(d)) out[[nm]] <- wk25_num(d[[nm]], 0)
    else out[[nm]] <- rep(0, n)
  }
  as.matrix(as.data.frame(out, check.names = FALSE))
}

wk25_assert_feature_contract <- function(features = MODEL25_SAFE_FEATURES) {
  bad <- intersect(features, MODEL25_FORBIDDEN_FEATURES)
  if (length(bad)) stop("2.5 leakage guard failed. Forbidden feature(s): ", paste(bad, collapse = ", "))
  invisible(TRUE)
}

wk25_xgb_params <- function(seed = 42L, objective = "reg:pseudohubererror") {
  list(
    objective = objective,
    eval_metric = "mae",
    max_depth = 3L,
    eta = 0.025,
    min_child_weight = 30,
    subsample = 0.85,
    colsample_bytree = 0.65,
    lambda = 20,
    alpha = 1,
    gamma = 0.05,
    tree_method = "hist",
    seed = as.integer(seed)
  )
}

wk25_fit_direct <- function(d, target_col = "weekly_fppg", features = MODEL25_SAFE_FEATURES,
                            seed = 42L, nrounds = 300L) {
  wk25_assert_feature_contract(features)
  if (!requireNamespace("xgboost", quietly = TRUE)) stop("xgboost is required for Model 2.5.")
  y <- wk25_num(d[[target_col]], NA_real_)
  keep <- is.finite(y)
  if (sum(keep) < 100) stop("Not enough chronological training rows for 2.5 direct model.")
  X <- wk25_prepare_matrix(d[keep, , drop = FALSE], features)
  dm <- xgboost::xgb.DMatrix(data = X, label = y[keep])
  primary <- wk25_xgb_params(seed, "reg:pseudohubererror")
  fit <- tryCatch(
    xgboost::xgb.train(params = primary, data = dm, nrounds = as.integer(nrounds), verbose = 0),
    error = function(e) {
      message("[2.5] Pseudo-Huber objective unavailable; falling back to squared error: ", conditionMessage(e))
      fallback <- wk25_xgb_params(seed, "reg:squarederror")
      xgboost::xgb.train(params = fallback, data = dm, nrounds = as.integer(nrounds), verbose = 0)
    }
  )
  list(model = fit, features = features, nrounds = as.integer(nrounds), n_train = sum(keep))
}

wk25_predict_direct <- function(obj, d) {
  if (!nrow(d)) return(numeric())
  X <- wk25_prepare_matrix(d, obj$features)
  pmax(0, as.numeric(stats::predict(obj$model, xgboost::xgb.DMatrix(X))))
}

wk25_decision_score <- function(d, alpha) {
  if (!nrow(d)) return(Inf)
  d$.candidate25 <- pmax(0, wk25_num(d$production_base_25) + alpha * (wk25_num(d$direct_fppg_25) - wk25_num(d$production_base_25)))
  cmp <- wk25_compare(d, "production_base_25", ".candidate25")
  get_ratio <- function(cohort, metric) {
    z <- cmp[cmp$cohort == cohort, , drop = FALSE]
    if (!nrow(z)) return(1)
    b <- z[[paste0("base_", metric)]][1]; c <- z[[paste0("candidate_", metric)]][1]
    if (!is.finite(b) || b <= 0 || !is.finite(c)) 1 else c / b
  }
  0.45 * get_ratio("Starter", "MAE") +
    0.30 * get_ratio("Relevant", "MAE") +
    0.15 * get_ratio("All", "MAE") +
    0.05 * get_ratio("Starter", "RMSE") +
    0.05 * get_ratio("Relevant", "RMSE")
}

wk25_select_alpha <- function(prior_oof, pos, final = FALSE) {
  if (!nrow(prior_oof)) return(MODEL25_BOOTSTRAP_ALPHA)
  n_years <- length(unique(as.integer(prior_oof$season)))
  max_alpha <- if (final) as.numeric(MODEL25_FINAL_ALPHA_CAP[[pos]]) else if (n_years < 2) MODEL25_EARLY_MAX_ALPHA else MODEL25_MATURE_MAX_ALPHA
  grid <- MODEL25_ALPHA_GRID[MODEL25_ALPHA_GRID <= max_alpha + 1e-9]
  scores <- vapply(grid, function(a) wk25_decision_score(prior_oof, a), numeric(1))
  best <- min(scores, na.rm = TRUE)
  near <- grid[is.finite(scores) & scores <= best + MODEL25_ALPHA_SCORE_TOL]
  if (!length(near)) return(0)
  # One-standard-error style shrinkage: among essentially tied blends, choose
  # the smallest correction away from the proven incumbent.
  min(near)
}

wk25_year_metrics <- function(d, base_col = "production_base_25", cand_col = "candidate_fppg_25") {
  if (!nrow(d)) return(data.frame())
  out <- list()
  for (yy in sort(unique(as.integer(d$season)))) {
    z <- d[as.integer(d$season) == yy, , drop = FALSE]
    cmp <- wk25_compare(z, base_col, cand_col)
    cmp$season <- yy
    out[[length(out) + 1]] <- cmp
  }
  dplyr::bind_rows(out)
}

wk25_cluster_bootstrap <- function(d, base_col = "production_base_25", cand_col = "candidate_fppg_25",
                                   cohort = "Starter", reps = MODEL25_BOOTSTRAP_REPS, seed = 42L) {
  if (identical(cohort, "Starter") && "starter_cohort" %in% names(d)) d <- d[wk25_bool(d$starter_cohort), , drop = FALSE]
  if (identical(cohort, "Relevant") && "relevant_cohort" %in% names(d)) d <- d[wk25_bool(d$relevant_cohort), , drop = FALSE]
  if (!nrow(d)) return(data.frame(cohort = cohort, mean_MAE_gain = NA_real_, p_improves = NA_real_, lower_90 = NA_real_, upper_90 = NA_real_))
  d$.delta <- abs(wk25_num(d$actual_fppg) - wk25_num(d[[base_col]])) - abs(wk25_num(d$actual_fppg) - wk25_num(d[[cand_col]]))
  d$.cluster <- paste(as.integer(d$season), as.integer(d$week), sep = "-")
  cl <- stats::aggregate(.delta ~ .cluster, data = d, FUN = mean)
  vals <- wk25_num(cl$.delta, NA_real_); vals <- vals[is.finite(vals)]
  if (!length(vals)) return(data.frame(cohort = cohort, mean_MAE_gain = NA_real_, p_improves = NA_real_, lower_90 = NA_real_, upper_90 = NA_real_))
  set.seed(seed + length(vals))
  sims <- replicate(as.integer(reps), mean(sample(vals, length(vals), replace = TRUE)))
  data.frame(
    cohort = cohort,
    mean_MAE_gain = mean(d$.delta, na.rm = TRUE),
    p_improves = mean(sims > 0, na.rm = TRUE),
    lower_90 = as.numeric(stats::quantile(sims, 0.05, na.rm = TRUE, names = FALSE)),
    upper_90 = as.numeric(stats::quantile(sims, 0.95, na.rm = TRUE, names = FALSE))
  )
}

wk25_promotion_gate <- function(d, pos) {
  cmp <- wk25_compare(d, "production_base_25", "candidate_fppg_25")
  ym <- wk25_year_metrics(d, "production_base_25", "candidate_fppg_25")
  row_for <- function(cohort) cmp[cmp$cohort == cohort, , drop = FALSE]
  allm <- row_for("All"); rel <- row_for("Relevant"); sta <- row_for("Starter")
  ys <- ym[ym$cohort == "Starter", , drop = FALSE]
  mae_year_wins <- sum(ys$MAE_improvement_pct > 0, na.rm = TRUE)
  rmse_year_wins <- sum(ys$RMSE_improvement_pct > 0, na.rm = TRUE)
  n_years <- nrow(ys)
  boot <- wk25_cluster_bootstrap(d, cohort = "Starter")
  checks <- c(
    all_MAE = nrow(allm) && allm$MAE_improvement_pct[1] > 0,
    all_RMSE = nrow(allm) && allm$RMSE_improvement_pct[1] >= 0,
    relevant_MAE = nrow(rel) && rel$MAE_improvement_pct[1] >= 0,
    relevant_RMSE = nrow(rel) && rel$RMSE_improvement_pct[1] >= 0,
    starter_MAE = nrow(sta) && sta$MAE_improvement_pct[1] > 0,
    starter_RMSE = nrow(sta) && sta$RMSE_improvement_pct[1] > 0,
    correlation = nrow(allm) && (is.na(allm$candidate_correlation[1]) || is.na(allm$base_correlation[1]) || allm$candidate_correlation[1] >= allm$base_correlation[1] - 0.002),
    rank_correlation = nrow(allm) && (is.na(allm$candidate_rank_correlation[1]) || is.na(allm$base_rank_correlation[1]) || allm$candidate_rank_correlation[1] >= allm$base_rank_correlation[1] - 0.002),
    year_MAE_stability = n_years >= 3 && mae_year_wins >= max(3, n_years - 1),
    year_RMSE_stability = n_years >= 3 && rmse_year_wins >= max(3, n_years - 1),
    paired_confidence = is.finite(boot$p_improves[1]) && boot$p_improves[1] >= 0.95
  )
  data.frame(
    position = pos,
    promoted_for_2026 = all(checks),
    failed_checks = paste(names(checks)[!checks], collapse = ";"),
    starter_MAE_gain_pct = if (nrow(sta)) sta$MAE_improvement_pct[1] else NA_real_,
    starter_RMSE_gain_pct = if (nrow(sta)) sta$RMSE_improvement_pct[1] else NA_real_,
    relevant_MAE_gain_pct = if (nrow(rel)) rel$MAE_improvement_pct[1] else NA_real_,
    relevant_RMSE_gain_pct = if (nrow(rel)) rel$RMSE_improvement_pct[1] else NA_real_,
    all_MAE_gain_pct = if (nrow(allm)) allm$MAE_improvement_pct[1] else NA_real_,
    all_RMSE_gain_pct = if (nrow(allm)) allm$RMSE_improvement_pct[1] else NA_real_,
    starter_MAE_year_wins = mae_year_wins,
    starter_RMSE_year_wins = rmse_year_wins,
    years_tested = n_years,
    starter_bootstrap_p_improves = boot$p_improves[1],
    stringsAsFactors = FALSE
  )
}
