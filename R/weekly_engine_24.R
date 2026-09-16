# ============================================================
# FANTASY MODEL 2.4 - SIGNAL ATTRIBUTION + PLAYER TALENT ENGINE
# ============================================================
# Requires config.R plus weekly_engine.R, weekly_engine_22.R,
# weekly_engine_23.R and weekly_engine_232.R.
#
# 2.4 is deliberately signal-first. It does NOT weight a variable merely by
# its raw correlation with fantasy points. Candidate signals must be pre-kickoff,
# stable through time, add information after the locked forecast, and improve
# chronological OOF prediction. Standardized ridge coefficients and OOF
# permutation loss provide conditional/unique signal weights.

wk24_num <- function(x, default = 0) {
  z <- suppressWarnings(as.numeric(x))
  z[!is.finite(z)] <- default
  z
}

wk24_clip <- function(x, lo, hi) pmin(hi, pmax(lo, wk24_num(x)))

wk24_norm_text <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  gsub("[^a-z0-9]", "", x)
}

wk24_pick <- function(d, candidates, default = NA) {
  nm <- intersect(candidates, names(d))
  if (length(nm)) d[[nm[1]]] else rep(default, nrow(d))
}

wk24_bool <- function(x) {
  if (is.logical(x)) return(ifelse(is.na(x), FALSE, x))
  if (is.numeric(x)) return(is.finite(x) & x != 0)
  z <- tolower(trimws(as.character(x)))
  z %in% c("1", "true", "t", "yes", "y", "pressure", "man", "zone")
}

wk24_safe_cor <- function(x, y, method = "pearson") {
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  k <- is.finite(x) & is.finite(y)
  if (sum(k) < 5 || stats::sd(x[k]) < 1e-10 || stats::sd(y[k]) < 1e-10) return(NA_real_)
  suppressWarnings(stats::cor(x[k], y[k], method = method))
}

wk24_metrics <- function(actual, pred, starter = NULL) {
  a <- suppressWarnings(as.numeric(actual)); p <- suppressWarnings(as.numeric(pred))
  keep <- is.finite(a) & is.finite(p)
  a <- a[keep]; p <- p[keep]
  if (is.null(starter)) starter <- rep(FALSE, length(keep))
  starter <- as.logical(starter)[keep]; starter[is.na(starter)] <- FALSE
  if (!length(a)) return(list(MAE = Inf, RMSE = Inf, correlation = NA_real_, rank_correlation = NA_real_, starter_MAE = Inf))
  list(
    MAE = mean(abs(p - a)),
    RMSE = sqrt(mean((p - a)^2)),
    correlation = wk24_safe_cor(p, a),
    rank_correlation = wk24_safe_cor(p, a, "spearman"),
    starter_MAE = if (sum(starter) >= 10) mean(abs(p[starter] - a[starter])) else mean(abs(p - a))
  )
}

wk24_utility <- function(base, cand) {
  if (!is.finite(base$MAE) || !is.finite(cand$MAE)) return(-Inf)
  g_mae <- (base$MAE - cand$MAE) / max(base$MAE, 1e-8)
  g_rmse <- (base$RMSE - cand$RMSE) / max(base$RMSE, 1e-8)
  g_starter <- (base$starter_MAE - cand$starter_MAE) / max(base$starter_MAE, 1e-8)
  g_corr <- if (is.finite(base$correlation) && is.finite(cand$correlation)) cand$correlation - base$correlation else 0
  g_rank <- if (is.finite(base$rank_correlation) && is.finite(cand$rank_correlation)) cand$rank_correlation - base$rank_correlation else 0
  0.38 * g_mae + 0.27 * g_rmse + 0.20 * g_starter + 0.10 * g_corr + 0.05 * g_rank
}

wk24_strict_promote <- function(base, cand) {
  if (!is.finite(base$MAE) || !is.finite(cand$MAE)) return(FALSE)
  mae_ok <- cand$MAE <= base$MAE * (1 - MODEL24_PROMOTION_MIN_MAE_GAIN)
  rmse_ok <- cand$RMSE <= base$RMSE
  starter_ok <- cand$starter_MAE <= base$starter_MAE * MODEL24_PROMOTION_MAX_STARTER_MAE_RATIO
  corr_ok <- !is.finite(base$correlation) || !is.finite(cand$correlation) || cand$correlation >= base$correlation - MODEL24_PROMOTION_MAX_CORR_LOSS
  rank_ok <- !is.finite(base$rank_correlation) || !is.finite(cand$rank_correlation) || cand$rank_correlation >= base$rank_correlation - MODEL24_PROMOTION_MAX_RANK_LOSS
  isTRUE(mae_ok && rmse_ok && starter_ok && corr_ok && rank_ok)
}

# ------------------------------------------------------------
# Locked 2.3 / 2.3.2 benchmark
# ------------------------------------------------------------
# 2.3.2 showed that a tiny population-MAE gain can hide worse RMSE, ranking or
# starter performance. 2.4 therefore chooses the benchmark itself with the
# same strict multi-metric rule before attempting to beat it.
wk24_choose_locked_base <- function(d) {
  out <- d
  if (!"honest_final_fppg" %in% names(out)) out$honest_final_fppg <- 0
  p23 <- wk24_num(out$honest_final_fppg)
  p232 <- if ("projected_weekly_fppg_232" %in% names(out)) wk24_num(out$projected_weekly_fppg_232) else p23
  starter <- if ("starter_cohort" %in% names(out)) out$starter_cohort %in% TRUE else rep(FALSE, nrow(out))
  m23 <- wk24_metrics(out$actual_fppg, p23, starter)
  m232 <- wk24_metrics(out$actual_fppg, p232, starter)
  use232 <- wk24_strict_promote(m23, m232)
  list(column = if (use232) "projected_weekly_fppg_232" else "honest_final_fppg",
       method = if (use232) "2.3.2" else "2.3", metrics23 = m23, metrics232 = m232)
}

# ------------------------------------------------------------
# Player talent prior / experience gate
# ------------------------------------------------------------
wk24_talent_weight <- function(games_prior, pos) {
  hl <- suppressWarnings(as.numeric(MODEL24_TALENT_HALF_LIFE_GAMES[[pos]]))
  if (!is.finite(hl) || hl <= 0) hl <- 12
  g <- pmax(0, wk24_num(games_prior))
  exp(-log(2) * g / hl)
}

wk24_add_talent_features <- function(d, talent, live = FALSE) {
  out <- d
  if (is.null(talent) || nrow(talent) == 0 || !"player_id" %in% names(out)) {
    out$talent_prior_fppg_24 <- 0
    out$talent_weight_24 <- 0
    out$talent_gap_weighted_24 <- 0
    return(out)
  }
  keep <- intersect(c("player_id", "talent_prior_fppg_24", "talent_prior_source_24", "talent_profile_available_24",
                      MODEL24_TALENT_FEATURES), names(talent))
  tt <- talent[, keep, drop = FALSE]
  tt <- tt[!duplicated(tt$player_id), , drop = FALSE]
  out <- dplyr::left_join(out, tt, by = "player_id")
  if (!"talent_prior_fppg_24" %in% names(out)) out$talent_prior_fppg_24 <- 0
  gp <- if ("games_played_prior" %in% names(out)) out$games_played_prior else rep(0, nrow(out))
  out$talent_weight_24 <- vapply(seq_len(nrow(out)), function(i) {
    p <- as.character(out$position[i]); if (!p %in% names(MODEL24_TALENT_HALF_LIFE_GAMES)) p <- "WR"
    wk24_talent_weight(gp[i], p)
  }, numeric(1))
  base_prior <- if ("preseason_prior_fppg" %in% names(out)) wk24_num(out$preseason_prior_fppg) else if ("locked_base_fppg_24" %in% names(out)) wk24_num(out$locked_base_fppg_24) else rep(0, nrow(out))
  out$talent_prior_fppg_24 <- wk24_num(out$talent_prior_fppg_24)
  out$talent_gap_weighted_24 <- out$talent_weight_24 * (out$talent_prior_fppg_24 - base_prior)
  # Pre-NFL measurements are gated by NFL experience before entering the weekly
  # model. A 40 time can matter for a rookie but cannot retain the same direct
  # weight after years of NFL evidence have accumulated.
  for (nm in intersect(MODEL24_TALENT_FEATURES, names(out))) {
    x <- suppressWarnings(as.numeric(out[[nm]])); x[!is.finite(x)] <- 0
    out[[paste0("tw_", nm)]] <- out$talent_weight_24 * x
  }
  out
}

# ------------------------------------------------------------
# Deterministic, pre-kickoff player archetypes
# ------------------------------------------------------------
wk24_archetype <- function(d) {
  n <- nrow(d); if (!n) return(character())
  pos <- as.character(d$position)
  getv <- function(nm) if (nm %in% names(d)) wk24_num(d[[nm]]) else rep(0, n)
  pc <- pmax(getv("projected_carries"), getv("roll3_carries"))
  pt <- pmax(getv("projected_targets"), getv("roll3_targets"))
  pa <- pmax(getv("projected_pass_attempts"), getv("roll3_pass_attempts"))
  ts <- getv("roll3_target_share")
  cs <- getv("roll3_carry_share")
  snap <- pmax(getv("roll3_offense_pct"), getv("roll5_offense_pct"))
  air <- pmax(getv("roll3_ngs_air_yard_share"), getv("preseason_deep_target_rate"))
  out <- rep("General", n)
  q <- pos == "QB"
  out[q & pc >= 4.5] <- "DualThreat"
  out[q & pc < 4.5 & pa >= 35] <- "VolumePocket"
  out[q & pc < 4.5 & pa < 35] <- "Pocket"
  r <- pos == "RB"
  out[r & (pt >= 4 | ts >= 0.10)] <- "ReceivingBack"
  out[r & !(pt >= 4 | ts >= 0.10) & (pc >= 14 | cs >= 0.55) & snap >= 0.55] <- "Workhorse"
  out[r & !(out %in% c("ReceivingBack", "Workhorse"))] <- "Committee"
  w <- pos == "WR"
  out[w & ts >= 0.24] <- "Alpha"
  out[w & ts < 0.24 & air >= 0.22] <- "Vertical"
  out[w & !(out %in% c("Alpha", "Vertical")) & pt >= 6] <- "VolumeReceiver"
  out[w & !(out %in% c("Alpha", "Vertical", "VolumeReceiver"))] <- "Secondary"
  t <- pos == "TE"
  out[t & (ts >= 0.16 | pt >= 5)] <- "ReceivingTE"
  out[t & !(out %in% c("ReceivingTE")) & snap >= 0.70] <- "InlineTE"
  out[t & !(out %in% c("ReceivingTE", "InlineTE"))] <- "HybridTE"
  out
}

# ------------------------------------------------------------
# Hierarchical player response to opponent style
# ------------------------------------------------------------
wk24_slope <- function(x, y) {
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  k <- is.finite(x) & is.finite(y)
  x <- x[k]; y <- y[k]
  if (length(x) < 12 || stats::var(x) < 1e-10) return(NA_real_)
  as.numeric(stats::cov(x, y) / stats::var(x))
}

wk24_fit_response_map <- function(history, pos, base_col = "locked_base_fppg_24") {
  d <- history[history$position == pos, , drop = FALSE]
  feats <- intersect(MODEL24_RESPONSE_FEATURES, names(d))
  if (nrow(d) < MODEL24_SIGNAL_MIN_ROWS || !length(feats)) return(list(position = pos, maps = list(), features = character()))
  if (!"archetype_24" %in% names(d)) d$archetype_24 <- wk24_archetype(d)
  d$residual_target_24 <- wk24_num(d$actual_fppg) - wk24_num(d[[base_col]])
  maps <- list()
  for (f in feats) {
    x <- suppressWarnings(as.numeric(d[[f]])); y <- d$residual_target_24
    k <- is.finite(x) & is.finite(y)
    if (sum(k) < MODEL24_SIGNAL_MIN_ROWS || stats::sd(x[k]) < 1e-8) next
    mu <- mean(x[k]); ps <- wk24_slope(x[k] - mu, y[k]); if (!is.finite(ps)) ps <- 0
    arch_rows <- list()
    for (a in unique(d$archetype_24[k])) {
      ii <- k & d$archetype_24 == a
      n_a <- sum(ii); raw <- if (n_a >= 20) wk24_slope(x[ii] - mu, y[ii]) else NA_real_
      if (!is.finite(raw)) raw <- ps
      shr <- (n_a * raw + MODEL24_RESPONSE_ARCHETYPE_K * ps) / (n_a + MODEL24_RESPONSE_ARCHETYPE_K)
      arch_rows[[length(arch_rows) + 1]] <- data.frame(key = as.character(a), n = n_a, slope = shr, stringsAsFactors = FALSE)
    }
    arch_df <- dplyr::bind_rows(arch_rows)
    player_rows <- list()
    ids <- unique(as.character(d$player_id[k]))
    for (id in ids) {
      ii <- k & as.character(d$player_id) == id
      n_p <- sum(ii)
      if (n_p < MODEL24_RESPONSE_MIN_PLAYER_GAMES) next
      raw <- wk24_slope(x[ii] - mu, y[ii]); if (!is.finite(raw)) next
      a <- as.character(d$archetype_24[which(ii)[1]])
      parent <- ps
      if (nrow(arch_df) && a %in% arch_df$key) parent <- arch_df$slope[match(a, arch_df$key)]
      shr <- (n_p * raw + MODEL24_RESPONSE_PLAYER_K * parent) / (n_p + MODEL24_RESPONSE_PLAYER_K)
      player_rows[[length(player_rows) + 1]] <- data.frame(key = id, n = n_p, slope = shr, stringsAsFactors = FALSE)
    }
    maps[[f]] <- list(mean = mu, position_slope = ps, archetype = arch_df,
                      player = dplyr::bind_rows(player_rows))
  }
  list(position = pos, maps = maps, features = names(maps))
}

wk24_response_col <- function(feature) paste0("response_", gsub("^opp_|_roll4_24$", "", feature), "_24")

wk24_apply_response_map <- function(d, obj, pos) {
  out <- d
  if (!"archetype_24" %in% names(out)) out$archetype_24 <- wk24_archetype(out)
  total <- rep(0, nrow(out))
  if (is.null(obj) || !length(obj$maps)) { out$response_adjustment_raw_24 <- total; return(out) }
  cap <- as.numeric(MODEL24_RESPONSE_CAP[[pos]]); if (!is.finite(cap)) cap <- 2
  for (f in names(obj$maps)) {
    if (!f %in% names(out)) next
    mp <- obj$maps[[f]]; x <- suppressWarnings(as.numeric(out[[f]])); x[!is.finite(x)] <- mp$mean
    eff <- rep(0, nrow(out))
    for (i in seq_len(nrow(out))) {
      sl <- mp$position_slope
      a <- as.character(out$archetype_24[i]); id <- as.character(out$player_id[i])
      if (nrow(mp$archetype) && a %in% mp$archetype$key) sl <- mp$archetype$slope[match(a, mp$archetype$key)]
      if (nrow(mp$player) && id %in% mp$player$key) sl <- mp$player$slope[match(id, mp$player$key)]
      eff[i] <- (x[i] - mp$mean) * sl
    }
    eff <- wk24_clip(eff, -cap, cap)
    out[[wk24_response_col(f)]] <- eff
    total <- total + eff
  }
  out$response_adjustment_raw_24 <- wk24_clip(total, -cap, cap)
  out
}

# ------------------------------------------------------------
# Signal discovery / conditional attribution
# ------------------------------------------------------------
wk24_partial_cor_base <- function(x, y, base) {
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y)); b <- suppressWarnings(as.numeric(base))
  k <- is.finite(x) & is.finite(y) & is.finite(b)
  if (sum(k) < 30 || stats::sd(x[k]) < 1e-10) return(NA_real_)
  rx <- stats::residuals(stats::lm(x[k] ~ b[k]))
  ry <- stats::residuals(stats::lm(y[k] ~ b[k]))
  wk24_safe_cor(rx, ry)
}

wk24_signal_candidates <- function(pos, available_names) {
  raw <- unique(c(
    get_weekly_features21(pos, available_names),
    get_features22(pos, "full", available_names),
    MODEL24_DEFENSE_STYLE_FEATURES,
    grep("^tw_.*_24$", available_names, value = TRUE),
    c("weekly_age", "weekly_experience", "weekly_is_rookie", "talent_prior_fppg_24", "talent_weight_24",
      "talent_gap_weighted_24", "xfp_roll3_24", "xfp_roll5_24", "fpoe_roll3_24", "fpoe_roll5_24",
      "expected_fppg_232", "xfp_gap_vs_base_232", "direct_gap_vs_base_232", "model22_gap_vs_base_232",
      "legacy_gap_vs_base_232", "role_vs_prior_232", "role_5_vs_prior_232", "matchup_abs_232",
      "projected_opportunities_232", "projected_touch_opportunities_232", "projected_td_points_232",
      "projection_spread_232", "model_disagreement_23", "calibrated_matchup_delta_23", "implied_team_total",
      "team_spread_line", "games_played_prior", "season_week", "roll3_fppg_sd", "response_adjustment_raw_24"),
    grep("^response_.*_24$", available_names, value = TRUE)
  ))
  raw <- intersect(raw, available_names)
  # Current-week outcomes, identifiers and data-availability flags are prohibited.
  banned_exact <- c("weekly_fppg", "actual_fppg", "season", "week", "player_id", "player_display_name",
                    "position", "team", "opponent", "baseline_rank", "starter_cohort", "relevant_cohort",
                    "ngs_available", "talent_profile_available_24")
  raw <- setdiff(raw, banned_exact)
  raw[!grepl("(^|_)actual($|_)|outcome|result|current_week|ngs_available|data_available", raw, ignore.case = TRUE)]
}

wk24_signal_audit <- function(d, pos, base_col = "locked_base_fppg_24") {
  if (nrow(d) < 30 || !all(c("actual_fppg", base_col) %in% names(d))) return(data.frame())
  y <- wk24_num(d$actual_fppg); base <- wk24_num(d[[base_col]]); residual <- y - base
  feats <- wk24_signal_candidates(pos, names(d))
  rows <- list()
  for (nm in feats) {
    x <- suppressWarnings(as.numeric(d[[nm]]))
    k <- is.finite(x) & is.finite(y) & is.finite(residual)
    if (sum(k) < 30 || stats::sd(x[k]) < 1e-9) next
    pear <- wk24_safe_cor(x[k], y[k]); spear <- wk24_safe_cor(x[k], y[k], "spearman")
    rc <- wk24_safe_cor(x[k], residual[k]); pc <- wk24_partial_cor_base(x[k], y[k], base[k])
    years <- sort(unique(as.integer(d$season[k])))
    yr_corr <- vapply(years, function(yy) {
      ii <- k & as.integer(d$season) == yy
      if (sum(ii) < 20 || stats::sd(x[ii]) < 1e-9) return(NA_real_)
      wk24_safe_cor(x[ii], residual[ii])
    }, numeric(1))
    yr_corr <- yr_corr[is.finite(yr_corr)]
    gs <- sign(ifelse(is.finite(rc), rc, 0))
    stability <- if (length(yr_corr) >= 2 && gs != 0) mean(sign(yr_corr) == gs) else 0.5
    med_abs <- if (length(yr_corr)) stats::median(abs(yr_corr)) else 0
    # Ranking is dominated by remaining-error signal and stability; raw outcome
    # correlation is descriptive rather than a direct model weight.
    score <- 0.42 * abs(ifelse(is.finite(rc), rc, 0)) +
      0.22 * abs(ifelse(is.finite(pc), pc, 0)) + 0.16 * stability +
      0.10 * med_abs + 0.06 * abs(ifelse(is.finite(pear), pear, 0)) +
      0.04 * abs(ifelse(is.finite(spear), spear, 0))
    rows[[length(rows) + 1]] <- data.frame(
      position = pos, feature = nm, n = sum(k), coverage = mean(k),
      outcome_pearson = pear, outcome_spearman = spear, partial_correlation_after_base = pc,
      residual_correlation = rc, year_sign_stability = stability,
      median_year_abs_residual_cor = med_abs, signal_score = score, stringsAsFactors = FALSE)
  }
  dplyr::bind_rows(rows) |> dplyr::arrange(dplyr::desc(signal_score))
}

wk24_prune_signals <- function(d, audit, max_features = MODEL24_SIGNAL_TOP_N) {
  if (is.null(audit) || !nrow(audit)) return(character())
  cand <- audit |> dplyr::filter(n >= MODEL24_SIGNAL_MIN_ROWS,
                                 abs(residual_correlation) >= MODEL24_SIGNAL_MIN_ABS_RESID_COR,
                                 year_sign_stability >= MODEL24_SIGNAL_MIN_DIRECTION_STABILITY) |>
    dplyr::arrange(dplyr::desc(signal_score)) |> dplyr::pull(feature)
  keep <- character()
  for (nm in cand) {
    if (!nm %in% names(d)) next
    x <- suppressWarnings(as.numeric(d[[nm]])); x[!is.finite(x)] <- 0
    if (stats::sd(x) < 1e-9) next
    redundant <- FALSE
    for (kk in keep) {
      y <- suppressWarnings(as.numeric(d[[kk]])); y[!is.finite(y)] <- 0
      cc <- suppressWarnings(stats::cor(x, y))
      if (is.finite(cc) && abs(cc) >= MODEL24_SIGNAL_REDUNDANCY_COR) { redundant <- TRUE; break }
    }
    if (!redundant) keep <- c(keep, nm)
    if (length(keep) >= max_features) break
  }
  keep
}

wk24_fit_signal_model <- function(history, pos, base_col = "locked_base_fppg_24") {
  d <- history[history$position == pos, , drop = FALSE]
  default <- list(enabled = FALSE, model = NULL, features = character(), lambda = NA_real_, inner_utility = 0,
                  inner_metrics = NULL, audit = data.frame(), position = pos)
  if (nrow(d) < MODEL24_SIGNAL_MIN_ROWS || length(unique(d$season)) < 2) return(default)
  d$residual_target_24 <- wk24_num(d$actual_fppg) - wk24_num(d[[base_col]])
  audit <- wk24_signal_audit(d, pos, base_col)
  feats <- wk24_prune_signals(d, audit)
  if (length(feats) < 2) { default$audit <- audit; return(default) }
  years <- sort(unique(as.integer(d$season))); vy <- max(years)
  tr <- d[as.integer(d$season) < vy, , drop = FALSE]; va <- d[as.integer(d$season) == vy, , drop = FALSE]
  if (nrow(tr) < MODEL24_SIGNAL_INNER_MIN_ROWS || nrow(va) < 30) { default$audit <- audit; return(default) }
  starter_tr <- if ("starter_cohort" %in% names(tr)) tr$starter_cohort %in% TRUE else rep(FALSE, nrow(tr))
  relevant_tr <- if ("relevant_cohort" %in% names(tr)) tr$relevant_cohort %in% TRUE else rep(FALSE, nrow(tr))
  w <- 1 + MODEL24_SIGNAL_STARTER_WEIGHT * starter_tr + MODEL24_SIGNAL_RELEVANT_WEIGHT * relevant_tr
  starter_va <- if ("starter_cohort" %in% names(va)) va$starter_cohort %in% TRUE else rep(FALSE, nrow(va))
  base_m <- wk24_metrics(va$actual_fppg, va[[base_col]], starter_va)
  scored <- list()
  for (lam in MODEL24_SIGNAL_RIDGE_LAMBDAS) {
    fit <- fit_ridge23(tr, feats, "residual_target_24", lam, w)
    if (is.null(fit)) next
    corr <- wk24_clip(predict_ridge23(fit, va), -MODEL24_SIGNAL_CORRECTION_CAP[[pos]], MODEL24_SIGNAL_CORRECTION_CAP[[pos]])
    pred <- pmax(0, wk24_num(va[[base_col]]) + corr)
    met <- wk24_metrics(va$actual_fppg, pred, starter_va)
    scored[[length(scored) + 1]] <- data.frame(lambda = lam, utility = wk24_utility(base_m, met),
      MAE = met$MAE, RMSE = met$RMSE, starter_MAE = met$starter_MAE,
      correlation = met$correlation, rank_correlation = met$rank_correlation)
  }
  sc <- dplyr::bind_rows(scored)
  if (!nrow(sc)) { default$audit <- audit; return(default) }
  sc <- sc |> dplyr::arrange(dplyr::desc(utility), MAE, RMSE)
  best <- sc[1, , drop = FALSE]
  if (!is.finite(best$utility) || best$utility <= 0) { default$audit <- audit; default$inner_metrics <- sc; return(default) }
  starter_all <- if ("starter_cohort" %in% names(d)) d$starter_cohort %in% TRUE else rep(FALSE, nrow(d))
  relevant_all <- if ("relevant_cohort" %in% names(d)) d$relevant_cohort %in% TRUE else rep(FALSE, nrow(d))
  wa <- 1 + MODEL24_SIGNAL_STARTER_WEIGHT * starter_all + MODEL24_SIGNAL_RELEVANT_WEIGHT * relevant_all
  fit <- fit_ridge23(d, feats, "residual_target_24", best$lambda[1], wa)
  list(enabled = !is.null(fit), model = fit, features = feats, lambda = best$lambda[1], inner_utility = best$utility[1],
       inner_metrics = sc, audit = audit, position = pos)
}

wk24_predict_signal <- function(obj, d, pos) {
  if (is.null(obj) || !isTRUE(obj$enabled) || is.null(obj$model)) return(rep(0, nrow(d)))
  cap <- as.numeric(MODEL24_SIGNAL_CORRECTION_CAP[[pos]]); if (!is.finite(cap)) cap <- 3
  wk24_clip(predict_ridge23(obj$model, d), -cap, cap)
}

wk24_coefficient_table <- function(obj, pos) {
  if (is.null(obj) || !isTRUE(obj$enabled) || is.null(obj$model)) return(data.frame())
  m <- obj$model
  b <- m$beta[-1]
  # Standardized coefficient is directly comparable across features. Convert to
  # approximate raw-unit slope as a second, more football-interpretable view.
  raw <- b / m$sds
  data.frame(position = pos, feature = m$features,
             standardized_coefficient = as.numeric(b), raw_unit_slope = as.numeric(raw),
             lambda = m$lambda, stringsAsFactors = FALSE)
}

wk24_permutation_importance <- function(model_obj, d, pos, base_col = "locked_base_fppg_24", repeats = MODEL24_PERMUTATION_REPEATS) {
  if (is.null(model_obj) || !isTRUE(model_obj$enabled) || !length(model_obj$features) || nrow(d) < 30) return(data.frame())
  starter <- if ("starter_cohort" %in% names(d)) d$starter_cohort %in% TRUE else rep(FALSE, nrow(d))
  corr0 <- wk24_predict_signal(model_obj, d, pos)
  p0 <- pmax(0, wk24_num(d[[base_col]]) + corr0)
  m0 <- wk24_metrics(d$actual_fppg, p0, starter)
  rows <- list(); set.seed(2401)
  for (f in model_obj$features) {
    if (!f %in% names(d)) next
    vals <- list()
    for (r in seq_len(max(1, repeats))) {
      z <- d; z[[f]] <- sample(z[[f]], nrow(z), replace = FALSE)
      pp <- pmax(0, wk24_num(z[[base_col]]) + wk24_predict_signal(model_obj, z, pos))
      mm <- wk24_metrics(z$actual_fppg, pp, starter)
      vals[[length(vals) + 1]] <- data.frame(
        delta_MAE_if_permuted = mm$MAE - m0$MAE,
        delta_RMSE_if_permuted = mm$RMSE - m0$RMSE,
        delta_correlation_if_permuted = m0$correlation - mm$correlation,
        delta_rank_correlation_if_permuted = m0$rank_correlation - mm$rank_correlation)
    }
    vv <- dplyr::bind_rows(vals)
    rows[[length(rows) + 1]] <- data.frame(position = pos, feature = f,
      delta_MAE_if_permuted = mean(vv$delta_MAE_if_permuted, na.rm = TRUE),
      delta_RMSE_if_permuted = mean(vv$delta_RMSE_if_permuted, na.rm = TRUE),
      delta_correlation_if_permuted = mean(vv$delta_correlation_if_permuted, na.rm = TRUE),
      delta_rank_correlation_if_permuted = mean(vv$delta_rank_correlation_if_permuted, na.rm = TRUE), stringsAsFactors = FALSE)
  }
  dplyr::bind_rows(rows) |> dplyr::arrange(dplyr::desc(delta_MAE_if_permuted))
}

cat("[2.4] Signal-attribution + player-talent engine loaded.\n")
