# ============================================================
# FANTASY MODEL 2.3.2 - SIGNAL DISCOVERY + ERROR FEEDBACK ENGINE
# ============================================================
# Requires config.R, weekly_engine.R, weekly_engine_22.R and weekly_engine_23.R.
#
# 2.3.2 adds two deliberately separate ideas:
#   1) FEED-FORWARD signal correction: learn which pre-kickoff signals explain
#      the remaining error after the existing 2.3 forecast.
#   2) FEEDBACK error correction: a bounded, decayed PID-like controller that
#      learns from a player's PRIOR observed forecast errors only.
#
# Critical leakage rule: a week can use only information that existed before
# that week's kickoff. The controller updates its error state only AFTER the
# prior game has been observed.

wk232_num <- function(x) {
  z <- suppressWarnings(as.numeric(x)); z[!is.finite(z)] <- 0; z
}

wk232_clip <- function(x, lo, hi) pmin(hi, pmax(lo, wk232_num(x)))

safe_ratio232 <- function(a, b, default = 0) {
  a <- wk232_num(a); b <- suppressWarnings(as.numeric(b))
  if (length(b) == 1L) b <- rep(b, length(a)) else b <- rep(b, length.out = length(a))
  out <- rep(default, length(a))
  ok <- is.finite(b) & abs(b) > 1e-9
  out[ok] <- a[ok] / b[ok]
  out[!is.finite(out)] <- default
  out
}

# ------------------------------------------------------------
# Production base selection
# ------------------------------------------------------------
W232_BASE_METHODS <- c(
  legacy21 = "legacy21_honest_fppg",
  model22 = "honest22_fppg",
  meta23 = "meta_precal_fppg",
  residual23 = "candidate23_fppg",
  final23 = "honest_final_fppg"
)

metric_bundle232 <- function(actual, pred, starter = NULL) {
  a <- wk232_num(actual); p <- wk232_num(pred)
  keep <- is.finite(a) & is.finite(p)
  a <- a[keep]; p <- p[keep]
  if (is.null(starter)) starter <- rep(FALSE, length(keep))
  starter <- as.logical(starter)[keep]
  mae <- if (length(a)) mean(abs(p - a)) else Inf
  rmse <- if (length(a)) sqrt(mean((p - a)^2)) else Inf
  corr <- if (length(a) >= 5) safe_cor21(p, a) else NA_real_
  rank <- if (length(a) >= 5) safe_cor21(p, a, "spearman") else NA_real_
  smae <- if (sum(starter, na.rm = TRUE) >= 10) mean(abs(p[starter] - a[starter])) else mae
  list(MAE = mae, RMSE = rmse, correlation = corr, rank_correlation = rank, starter_MAE = smae)
}

relative_utility232 <- function(base_metrics, candidate_metrics) {
  if (!is.finite(base_metrics$MAE) || !is.finite(candidate_metrics$MAE)) return(-Inf)
  g_mae <- (base_metrics$MAE - candidate_metrics$MAE) / max(base_metrics$MAE, 1e-6)
  g_rmse <- (base_metrics$RMSE - candidate_metrics$RMSE) / max(base_metrics$RMSE, 1e-6)
  g_starter <- (base_metrics$starter_MAE - candidate_metrics$starter_MAE) / max(base_metrics$starter_MAE, 1e-6)
  bc <- base_metrics$correlation; cc <- candidate_metrics$correlation
  g_corr <- if (is.finite(bc) && is.finite(cc)) cc - bc else 0
  br <- base_metrics$rank_correlation; cr <- candidate_metrics$rank_correlation
  g_rank <- if (is.finite(br) && is.finite(cr)) cr - br else 0
  0.40 * g_mae + 0.25 * g_rmse + 0.20 * g_starter + 0.10 * g_corr + 0.05 * g_rank
}

select_base_method232 <- function(history) {
  if (nrow(history) < WEEKLY_232_BASE_MIN_ROWS) return("honest_final_fppg")
  starter <- if ("starter_cohort" %in% names(history)) history$starter_cohort %in% TRUE else rep(FALSE, nrow(history))
  rows <- lapply(names(W232_BASE_METHODS), function(nm) {
    col <- W232_BASE_METHODS[[nm]]
    if (!col %in% names(history)) return(NULL)
    m <- metric_bundle232(history$actual_fppg, history[[col]], starter)
    data.frame(method = nm, column = col, MAE = m$MAE, RMSE = m$RMSE,
               starter_MAE = m$starter_MAE, correlation = m$correlation,
               rank_correlation = m$rank_correlation, stringsAsFactors = FALSE)
  })
  sc <- dplyr::bind_rows(rows)
  if (nrow(sc) == 0) return("honest_final_fppg")
  # Normalize within-position so MAE/RMSE/starter error dominate but a model
  # cannot win by a tiny MAE gain while materially worsening correlation.
  min_mae <- min(sc$MAE, na.rm = TRUE); min_rmse <- min(sc$RMSE, na.rm = TRUE); min_s <- min(sc$starter_MAE, na.rm = TRUE)
  sc$objective <- 0.45 * safe_ratio232(sc$MAE, min_mae, 1) +
    0.25 * safe_ratio232(sc$RMSE, min_rmse, 1) +
    0.20 * safe_ratio232(sc$starter_MAE, min_s, 1) -
    0.07 * dplyr::coalesce(sc$correlation, 0) - 0.03 * dplyr::coalesce(sc$rank_correlation, 0)
  sc <- sc[order(sc$objective, sc$MAE, sc$RMSE), , drop = FALSE]
  sc$column[1]
}

# ------------------------------------------------------------
# Safe feed-forward / expected-production features
# ------------------------------------------------------------
engineer_signal_features232 <- function(d, base_col = "base232_fppg") {
  out <- d
  need <- function(nm) if (nm %in% names(out)) wk232_num(out[[nm]]) else rep(0, nrow(out))
  base <- if (base_col %in% names(out)) need(base_col) else need("honest_final_fppg")
  xfp <- need("structured_neutral_fppg")
  out$expected_fppg_232 <- xfp
  out$xfp_gap_vs_base_232 <- xfp - base
  out$direct_gap_vs_base_232 <- need("direct_full_fppg") - base
  out$model22_gap_vs_base_232 <- need("honest22_fppg") - base
  out$legacy_gap_vs_base_232 <- need("legacy21_honest_fppg") - base
  out$role_vs_prior_232 <- need("roll3_fppg") - need("preseason_prior_fppg")
  out$role_5_vs_prior_232 <- need("roll5_fppg") - need("preseason_prior_fppg")
  out$matchup_abs_232 <- abs(need("calibrated_matchup_delta_23"))
  out$projected_opportunities_232 <- need("projected_pass_attempts") + need("projected_carries") + need("projected_targets")
  out$projected_touch_opportunities_232 <- need("projected_carries") + need("projected_targets")
  out$projected_td_points_232 <- 4 * need("projected_pass_tds") + 6 * (need("projected_rush_tds") + need("projected_rec_tds")) - 2 * need("projected_interceptions")
  out$projection_spread_232 <- pmax(need("direct_full_fppg"), need("structured_matchup_calibrated_fppg"), need("legacy21_honest_fppg")) -
    pmin(need("direct_full_fppg"), need("structured_matchup_calibrated_fppg"), need("legacy21_honest_fppg"))
  out
}

signal_candidate_names232 <- function(pos, available_names) {
  raw <- unique(c(
    get_weekly_features21(pos, available_names),
    get_features22(pos, "full", available_names),
    c("expected_fppg_232", "xfp_gap_vs_base_232", "direct_gap_vs_base_232",
      "model22_gap_vs_base_232", "legacy_gap_vs_base_232", "role_vs_prior_232",
      "role_5_vs_prior_232", "matchup_abs_232", "projected_opportunities_232",
      "projected_touch_opportunities_232", "projected_td_points_232", "projection_spread_232",
      "model_disagreement_23", "calibrated_matchup_delta_23", "implied_team_total",
      "team_spread_line", "games_played_prior", "season_week", "roll3_fppg_sd")
  ))
  raw <- intersect(raw, available_names)
  # Current-week outcomes or identifiers are never candidates.
  banned <- c("weekly_fppg", "actual_fppg", "season", "week", "player_id", "player_display_name",
              "position", "team", "opponent", "baseline_rank", "starter_cohort", "relevant_cohort")
  setdiff(raw, banned)
}

signal_audit232 <- function(d, pos, base_col = "base232_fppg") {
  if (nrow(d) < 30) return(data.frame())
  d <- engineer_signal_features232(d, base_col)
  if (!"actual_fppg" %in% names(d)) return(data.frame())
  d$residual_target_232 <- wk232_num(d$actual_fppg) - wk232_num(d[[base_col]])
  feats <- signal_candidate_names232(pos, names(d))
  rows <- lapply(feats, function(nm) {
    x <- wk232_num(d[[nm]]); y <- wk232_num(d$actual_fppg); r <- wk232_num(d$residual_target_232)
    keep <- is.finite(x) & is.finite(y) & is.finite(r)
    if (sum(keep) < 30 || stats::sd(x[keep]) < 1e-9) return(NULL)
    pear <- safe_cor21(x[keep], y[keep])
    spear <- safe_cor21(x[keep], y[keep], "spearman")
    rc <- safe_cor21(x[keep], r[keep])
    years <- sort(unique(d$season[keep]))
    yc <- vapply(years, function(yy) {
      ii <- keep & d$season == yy
      if (sum(ii) < 20 || stats::sd(x[ii]) < 1e-9) return(NA_real_)
      safe_cor21(x[ii], r[ii])
    }, numeric(1))
    yc <- yc[is.finite(yc)]
    global_sign <- sign(ifelse(is.finite(rc), rc, 0))
    stability <- if (length(yc) >= 2 && global_sign != 0) mean(sign(yc) == global_sign) else 0.5
    median_year_abs <- if (length(yc)) stats::median(abs(yc)) else 0
    score <- 0.45 * abs(ifelse(is.finite(rc), rc, 0)) +
      0.18 * abs(ifelse(is.finite(pear), pear, 0)) +
      0.12 * abs(ifelse(is.finite(spear), spear, 0)) +
      0.15 * stability + 0.10 * median_year_abs
    data.frame(position = pos, feature = nm, n = sum(keep), coverage = mean(keep),
               outcome_pearson = pear, outcome_spearman = spear, residual_correlation = rc,
               year_sign_stability = stability, median_year_abs_residual_cor = median_year_abs,
               signal_score = score, stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows) |> dplyr::arrange(dplyr::desc(signal_score))
}

prune_redundant_signals232 <- function(d, ranked_features, max_features = WEEKLY_232_SIGNAL_TOP_N,
                                       cor_threshold = WEEKLY_232_REDUNDANCY_COR) {
  if (length(ranked_features) == 0) return(character())
  keep <- character()
  for (nm in ranked_features) {
    if (!nm %in% names(d)) next
    x <- wk232_num(d[[nm]])
    if (stats::sd(x, na.rm = TRUE) < 1e-9) next
    redundant <- FALSE
    for (kk in keep) {
      cc <- suppressWarnings(stats::cor(x, wk232_num(d[[kk]]), use = "pairwise.complete.obs"))
      if (is.finite(cc) && abs(cc) >= cor_threshold) { redundant <- TRUE; break }
    }
    if (!redundant) keep <- c(keep, nm)
    if (length(keep) >= max_features) break
  }
  keep
}

# ------------------------------------------------------------
# Contextual residual model: explains WHY the base missed.
# ------------------------------------------------------------
fit_signal_residual232 <- function(history, pos, base_col = "base232_fppg") {
  default <- list(enabled = FALSE, model = NULL, features = character(), lambda = NA_real_, inner_gain = 0)
  if (nrow(history) < WEEKLY_232_SIGNAL_MIN_ROWS || length(unique(history$season)) < 2) return(default)
  d <- engineer_signal_features232(history, base_col)
  d$residual_target_232 <- wk232_num(d$actual_fppg) - wk232_num(d[[base_col]])
  inner_year <- max(d$season, na.rm = TRUE)
  tr <- d[d$season < inner_year, , drop = FALSE]
  va <- d[d$season == inner_year, , drop = FALSE]
  if (nrow(tr) < WEEKLY_232_SIGNAL_INNER_MIN_ROWS || nrow(va) < 30) return(default)

  audit <- signal_audit232(tr, pos, base_col)
  if (nrow(audit) == 0) return(default)
  ranked <- audit$feature[audit$signal_score >= WEEKLY_232_SIGNAL_MIN_SCORE]
  if (length(ranked) < 3) ranked <- utils::head(audit$feature, min(8, nrow(audit)))
  feats <- prune_redundant_signals232(tr, ranked)
  if (length(feats) < 3) return(default)

  starter <- if ("starter_cohort" %in% names(tr)) tr$starter_cohort %in% TRUE else rep(FALSE, nrow(tr))
  relevant <- if ("relevant_cohort" %in% names(tr)) tr$relevant_cohort %in% TRUE else rep(FALSE, nrow(tr))
  weights <- 1 + WEEKLY_232_SIGNAL_RELEVANT_WEIGHT * relevant + WEEKLY_232_SIGNAL_STARTER_WEIGHT * starter

  base_m <- metric_bundle232(va$actual_fppg, va[[base_col]], if ("starter_cohort" %in% names(va)) va$starter_cohort else NULL)
  best <- NULL
  for (lam in WEEKLY_232_SIGNAL_RIDGE_LAMBDAS) {
    fit <- fit_ridge23(tr, feats, "residual_target_232", lam, weights)
    if (is.null(fit)) next
    corr <- wk232_clip(predict_ridge23(fit, va), -WEEKLY_232_SIGNAL_CAP[[pos]], WEEKLY_232_SIGNAL_CAP[[pos]])
    pred <- pmax(0, wk232_num(va[[base_col]]) + corr)
    m <- metric_bundle232(va$actual_fppg, pred, if ("starter_cohort" %in% names(va)) va$starter_cohort else NULL)
    util <- relative_utility232(base_m, m)
    row <- list(model = fit, features = feats, lambda = lam, utility = util, metrics = m)
    if (is.null(best) || util > best$utility) best <- row
  }
  if (is.null(best) || !is.finite(best$utility) || best$utility < WEEKLY_232_SIGNAL_MIN_UTILITY || best$metrics$MAE >= base_m$MAE) return(default)

  # Refit chosen schema/lambda on all prior OOF history.
  starter_all <- if ("starter_cohort" %in% names(d)) d$starter_cohort %in% TRUE else rep(FALSE, nrow(d))
  relevant_all <- if ("relevant_cohort" %in% names(d)) d$relevant_cohort %in% TRUE else rep(FALSE, nrow(d))
  weights_all <- 1 + WEEKLY_232_SIGNAL_RELEVANT_WEIGHT * relevant_all + WEEKLY_232_SIGNAL_STARTER_WEIGHT * starter_all
  fit_all <- fit_ridge23(d, best$features, "residual_target_232", best$lambda, weights_all)
  list(enabled = !is.null(fit_all), model = fit_all, features = best$features, lambda = best$lambda,
       inner_gain = best$utility)
}

predict_signal_residual232 <- function(obj, d, pos, base_col = "base232_fppg") {
  if (is.null(obj) || !isTRUE(obj$enabled) || is.null(obj$model)) return(rep(0, nrow(d)))
  z <- engineer_signal_features232(d, base_col)
  cap <- as.numeric(WEEKLY_232_SIGNAL_CAP[[pos]]); if (!is.finite(cap)) cap <- 1.5
  wk232_clip(predict_ridge23(obj$model, z), -cap, cap)
}

# ------------------------------------------------------------
# Bounded PID-like forecast-error controller
# ------------------------------------------------------------
# This is intentionally a decayed controller, not a literal industrial PID.
# Weekly fantasy outcomes are noisy and the 'plant' changes as roles/injuries
# change. Decay + caps prevent integral windup and chasing TD randomness.

pid_grid232 <- function() {
  expand.grid(
    kp = WEEKLY_232_PID_KP,
    ki = WEEKLY_232_PID_KI,
    kd = WEEKLY_232_PID_KD,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
}

simulate_pid232 <- function(d, pos, base_col = "base232_fppg", signal_correction = NULL,
                            kp = 0, ki = 0, kd = 0, actual_col = "actual_fppg") {
  if (nrow(d) == 0) return(d)
  x <- d
  if (is.null(signal_correction)) signal_correction <- rep(0, nrow(x))
  x$.signal232 <- wk232_num(signal_correction)
  x$.row232 <- seq_len(nrow(x))
  x$pid_p_232 <- 0; x$pid_i_232 <- 0; x$pid_d_232 <- 0
  x$pid_correction_232 <- 0; x$signal_correction_232 <- x$.signal232
  x$feedback_total_correction_232 <- 0; x$prediction_232 <- wk232_num(x[[base_col]])
  cap <- as.numeric(WEEKLY_232_TOTAL_CORRECTION_CAP[[pos]]); if (!is.finite(cap)) cap <- 2
  int_cap <- as.numeric(WEEKLY_232_PID_INTEGRAL_CAP[[pos]]); if (!is.finite(int_cap)) int_cap <- 6
  deriv_cap <- as.numeric(WEEKLY_232_PID_DERIVATIVE_CAP[[pos]]); if (!is.finite(deriv_cap)) deriv_cap <- 8

  groups <- split(seq_len(nrow(x)), interaction(x$season, x$player_id, drop = TRUE))
  for (idx0 in groups) {
    idx <- idx0[order(wk232_num(x$week[idx0]))]
    prev_err <- 0; prev_prev_err <- 0; integral <- 0
    for (j in idx) {
      pterm <- prev_err
      iterm <- integral
      dterm <- wk232_clip(prev_err - prev_prev_err, -deriv_cap, deriv_cap)
      pid <- kp * pterm + ki * iterm + kd * dterm
      total <- wk232_clip(x$.signal232[j] + pid, -cap, cap)
      pred <- max(0, wk232_num(x[[base_col]][j]) + total)
      x$pid_p_232[j] <- pterm; x$pid_i_232[j] <- iterm; x$pid_d_232[j] <- dterm
      x$pid_correction_232[j] <- pid; x$feedback_total_correction_232[j] <- total; x$prediction_232[j] <- pred
      actual <- wk232_num(x[[actual_col]][j])
      err <- actual - pred
      prev_prev_err <- prev_err
      prev_err <- err
      integral <- WEEKLY_232_PID_DECAY * integral + err
      integral <- wk232_clip(integral, -int_cap, int_cap)
    }
  }
  x <- x[order(x$.row232), , drop = FALSE]
  x$.signal232 <- NULL; x$.row232 <- NULL
  x
}

fit_pid232 <- function(history, pos, base_col = "base232_fppg", signal_obj = NULL) {
  default <- list(enabled = FALSE, kp = 0, ki = 0, kd = 0, inner_gain = 0)
  if (nrow(history) < WEEKLY_232_PID_MIN_ROWS || length(unique(history$season)) < 2) return(default)
  inner_year <- max(history$season, na.rm = TRUE)
  tr <- history[history$season < inner_year, , drop = FALSE]
  va <- history[history$season == inner_year, , drop = FALSE]
  if (nrow(tr) < WEEKLY_232_PID_INNER_MIN_ROWS || nrow(va) < 30) return(default)

  # The signal model is fitted only on older seasons when tuning PID gains.
  sig_inner <- fit_signal_residual232(tr, pos, base_col)
  sig_va <- predict_signal_residual232(sig_inner, va, pos, base_col)
  base_m <- metric_bundle232(va$actual_fppg, va[[base_col]], if ("starter_cohort" %in% names(va)) va$starter_cohort else NULL)
  best <- list(utility = -Inf)
  grid <- pid_grid232()
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, , drop = FALSE]
    sim <- simulate_pid232(va, pos, base_col, sig_va, g$kp, g$ki, g$kd)
    m <- metric_bundle232(sim$actual_fppg, sim$prediction_232, if ("starter_cohort" %in% names(sim)) sim$starter_cohort else NULL)
    util <- relative_utility232(base_m, m)
    if (is.finite(util) && util > best$utility) best <- list(utility = util, kp = g$kp, ki = g$ki, kd = g$kd, metrics = m)
  }
  if (!is.finite(best$utility) || best$utility < WEEKLY_232_PID_MIN_UTILITY || best$metrics$MAE >= base_m$MAE) return(default)
  list(enabled = TRUE, kp = best$kp, ki = best$ki, kd = best$kd, inner_gain = best$utility)
}

apply_controller232 <- function(d, pos, signal_obj, pid_obj, base_col = "base232_fppg") {
  sig <- predict_signal_residual232(signal_obj, d, pos, base_col)
  kp <- if (!is.null(pid_obj) && isTRUE(pid_obj$enabled)) pid_obj$kp else 0
  ki <- if (!is.null(pid_obj) && isTRUE(pid_obj$enabled)) pid_obj$ki else 0
  kd <- if (!is.null(pid_obj) && isTRUE(pid_obj$enabled)) pid_obj$kd else 0
  simulate_pid232(d, pos, base_col, sig, kp, ki, kd)
}

# ------------------------------------------------------------
# Live PID state from saved pregame snapshots + completed 2026 games.
# ------------------------------------------------------------
# Returns one row per player with P/I/D state ready for the upcoming week.
build_live_pid_state232 <- function(history_snapshots, actual_weekly, pos, prediction_col = "projected_weekly_fppg_232") {
  empty <- data.frame(player_id = character(), pid_p_232 = numeric(), pid_i_232 = numeric(), pid_d_232 = numeric(), stringsAsFactors = FALSE)
  if (nrow(history_snapshots) == 0 || nrow(actual_weekly) == 0) return(empty)
  h <- history_snapshots
  fallback <- intersect(c("projected_weekly_fppg", "controller_base_fppg_232"), names(h))
  if (!prediction_col %in% names(h) && length(fallback) == 0) return(empty)
  preferred <- if (prediction_col %in% names(h)) suppressWarnings(as.numeric(h[[prediction_col]])) else rep(NA_real_, nrow(h))
  if (length(fallback) > 0) {
    fb <- suppressWarnings(as.numeric(h[[fallback[1]]]))
    bad <- !is.finite(preferred); preferred[bad] <- fb[bad]
  }
  h$.prediction232_live <- preferred
  if (!all(c("player_id", "week") %in% names(h))) return(empty)
  # Multiple snapshots can exist for a week; use the last stored pregame view.
  if ("generated_at" %in% names(h)) h <- h[order(h$generated_at), , drop = FALSE]
  h <- h |> dplyr::group_by(player_id, week) |> dplyr::slice_tail(n = 1) |> dplyr::ungroup()
  a <- actual_weekly
  if (!"actual_fppg" %in% names(a)) {
    if ("weekly_fppg" %in% names(a)) a$actual_fppg <- a$weekly_fppg else return(empty)
  }
  z <- h |> dplyr::inner_join(a |> dplyr::select(player_id, week, actual_fppg), by = c("player_id", "week"))
  if (nrow(z) == 0) return(empty)
  z <- z[is.finite(suppressWarnings(as.numeric(z$.prediction232_live))) & is.finite(suppressWarnings(as.numeric(z$actual_fppg))), , drop = FALSE]
  if (nrow(z) == 0) return(empty)
  z$error232 <- suppressWarnings(as.numeric(z$actual_fppg)) - suppressWarnings(as.numeric(z$.prediction232_live))
  groups <- split(seq_len(nrow(z)), z$player_id)
  rows <- lapply(groups, function(idx0) {
    idx <- idx0[order(wk232_num(z$week[idx0]))]
    errs <- wk232_num(z$error232[idx])
    prev <- if (length(errs)) utils::tail(errs, 1) else 0
    prev2 <- if (length(errs) >= 2) errs[length(errs) - 1] else 0
    integral <- 0
    for (e in errs) integral <- WEEKLY_232_PID_DECAY * integral + e
    int_cap <- as.numeric(WEEKLY_232_PID_INTEGRAL_CAP[[pos]]); if (!is.finite(int_cap)) int_cap <- 6
    deriv_cap <- as.numeric(WEEKLY_232_PID_DERIVATIVE_CAP[[pos]]); if (!is.finite(deriv_cap)) deriv_cap <- 8
    data.frame(player_id = as.character(z$player_id[idx[1]]), pid_p_232 = prev,
               pid_i_232 = wk232_clip(integral, -int_cap, int_cap),
               pid_d_232 = wk232_clip(prev - prev2, -deriv_cap, deriv_cap), stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

predict_live_controller232 <- function(d, pos, signal_obj, pid_obj, live_state, base_col = "base232_fppg") {
  out <- engineer_signal_features232(d, base_col)
  out$signal_correction_232 <- predict_signal_residual232(signal_obj, out, pos, base_col)
  out <- out |> dplyr::left_join(live_state, by = "player_id")
  for (nm in c("pid_p_232", "pid_i_232", "pid_d_232")) {
    if (!nm %in% names(out)) out[[nm]] <- 0
    out[[nm]] <- dplyr::coalesce(wk232_num(out[[nm]]), 0)
  }
  kp <- if (!is.null(pid_obj) && isTRUE(pid_obj$enabled)) pid_obj$kp else 0
  ki <- if (!is.null(pid_obj) && isTRUE(pid_obj$enabled)) pid_obj$ki else 0
  kd <- if (!is.null(pid_obj) && isTRUE(pid_obj$enabled)) pid_obj$kd else 0
  out$pid_correction_232 <- kp * out$pid_p_232 + ki * out$pid_i_232 + kd * out$pid_d_232
  cap <- as.numeric(WEEKLY_232_TOTAL_CORRECTION_CAP[[pos]]); if (!is.finite(cap)) cap <- 2
  out$feedback_total_correction_232 <- wk232_clip(out$signal_correction_232 + out$pid_correction_232, -cap, cap)
  out$projected_weekly_fppg_pre_availability_232 <- pmax(0, wk232_num(out[[base_col]]) + out$feedback_total_correction_232)
  out
}
