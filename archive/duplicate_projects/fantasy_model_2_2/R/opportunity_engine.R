# ============================================================
# FANTASY MODEL 2.0 - OPPORTUNITY-FIRST STATISTICAL ENGINE
# ============================================================
# Architecture:
#   team volume -> player opportunity -> shrinkage efficiency
#   -> red-zone-adjusted TD expectation -> stat line -> FPPG
#   -> capped residual correction -> validated guardrail blend
#
# The direct 1.2 model remains available as a validation guardrail,
# but the structured 2.0 projection is built from football components.

num20 <- function(x) suppressWarnings(as.numeric(x))
clip20 <- function(x, lo = -Inf, hi = Inf) {
  x <- num20(x)
  x[!is.finite(x)] <- 0
  pmin(hi, pmax(lo, x))
}
safe_div20 <- function(a, b, default = 0) {
  a <- num20(a); b <- num20(b)
  out <- rep(default, max(length(a), length(b)))
  ok <- is.finite(a) & is.finite(b) & b > 0
  out[ok] <- a[ok] / b[ok]
  out
}
ensure20 <- function(df, nm, default = 0) {
  if (!nm %in% names(df)) df[[nm]] <- rep(default, nrow(df))
  df
}
weighted_mean_safe20 <- function(x, w) {
  x <- num20(x); w <- num20(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

cat("[2.0 HOTFIX 5] Opportunity engine loaded: position-specific residual schemas enabled.\n")

# ------------------------------------------------------------
# Opportunity models
# ------------------------------------------------------------
OPPORTUNITY_EXTRA_FEATURES_20 <- c(
  "team_projected_pass_attempts_pg", "team_projected_carries_pg",
  "prior_teammate_max_target_share", "prior_teammate_top2_target_share",
  "prior_teammate_target_hhi", "prior_teammate_max_carry_share",
  "prior_teammate_top2_carry_share", "prior_teammate_carry_hhi",
  "target_competition_index", "carry_competition_index",
  "team_pass_opportunity_index", "team_rush_opportunity_index"
)

opportunity_feature_bank20 <- function(position, component, available_names = NULL) {
  pos <- toupper(as.character(position)[1])
  common <- c(
    "prior_fppg", "two_year_fppg", "recent_weighted_fppg", "fppg_trend",
    "prior_games", "two_year_games", "career_games_before", "experience", "is_rookie",
    "age", "age_squared", "draft_round", "draft_pick", "draft_capital_score",
    "team_projected_pass_attempts_pg", "team_projected_carries_pg",
    "team_prior_plays_pg", "team_prior_pass_rate", "team_prior_neutral_pass_rate",
    "team_prior_redzone_pass_rate", "team_prior_redzone_rush_rate",
    "team_prior_points_pg", "team_prior_shotgun_rate", "team_prior_no_huddle_rate"
  )

  bank <- switch(
    paste(pos, component, sep = "_"),
    QB_pass_attempts_pg = c(
      common, "prior_pass_attempts_pg", "two_year_pass_attempts_pg", "prior_pass_yd_pg",
      "team_prior_pass_attempts_pg", "player_qb_prior_avg_air_yards",
      "player_qb_prior_deep_throw_rate", "player_qb_prior_redzone_throw_rate",
      "player_qb_prior_ngs_time_to_throw", "player_qb_prior_ngs_cpoe", "qb_team_continuity"
    ),
    QB_carries_pg = c(
      common, "prior_carries_pg", "two_year_carries_pg", "prior_rush_yd_pg",
      "prior_rush_td_rate", "player_qb_prior_ngs_time_to_throw", "qb_team_continuity"
    ),
    RB_carry_share = c(
      common, "prior_carries_pg", "two_year_carries_pg", "prior_targets_pg",
      "rb_prior_team_carry_share", "rb_prior_touch_opportunity",
      "prior_teammate_max_carry_share", "prior_teammate_top2_carry_share",
      "prior_teammate_carry_hhi", "carry_competition_index", "team_rush_opportunity_index"
    ),
    RB_target_share = c(
      common, "prior_target_share", "prior_targets_pg", "two_year_targets_pg", "prior_wopr",
      "qb_prior_rb_target_rate", "rec_prior_short_target_rate", "rec_prior_route_screen",
      "rb_receiving_fit", "prior_teammate_max_target_share", "prior_teammate_top2_target_share",
      "prior_teammate_target_hhi", "target_competition_index", "team_pass_opportunity_index"
    ),
    WR_target_share = c(
      common, "prior_target_share", "prior_targets_pg", "two_year_targets_pg", "prior_wopr",
      "prior_air_yards_share", "qb_prior_wr_target_rate", "rec_prior_avg_depth_target",
      "rec_prior_deep_target_rate", "rec_prior_redzone_target_rate",
      "qb_receiver_depth_fit", "qb_receiver_location_fit", "qb_receiver_route_fit",
      "wr_context_fit_score", "prior_teammate_max_target_share", "prior_teammate_top2_target_share",
      "prior_teammate_target_hhi", "target_competition_index", "team_pass_opportunity_index"
    ),
    WR_carries_pg = c(
      common, "prior_carries_pg", "two_year_carries_pg", "prior_rush_yd_pg",
      "rec_prior_route_screen", "team_prior_shotgun_rate"
    ),
    TE_target_share = c(
      common, "prior_target_share", "prior_targets_pg", "two_year_targets_pg", "prior_wopr",
      "qb_prior_te_target_rate", "rec_prior_middle_target_rate", "rec_prior_redzone_target_rate",
      "rec_prior_short_target_rate", "te_middle_fit", "te_redzone_fit", "te_route_fit",
      "te_context_fit_score", "prior_teammate_max_target_share", "prior_teammate_top2_target_share",
      "prior_teammate_target_hhi", "target_competition_index", "team_pass_opportunity_index"
    ),
    common
  )

  # Keep only position-specific context that already exists in the validated 1.2
  # feature architecture. This prevents cross-position context leakage/noise.
  bank <- unique(c(bank, intersect(POSITION_CONTEXT_FEATURES[[pos]], get_model_features(pos)), OPPORTUNITY_EXTRA_FEATURES_20))
  if (!is.null(available_names)) bank <- intersect(bank, available_names)
  bank
}

get_opportunity_specs20 <- function(position) {
  pos <- toupper(as.character(position)[1])
  switch(
    pos,
    QB = list(
      pass_attempts_pg = list(target = "target_pass_attempts_pg", lo = 10, hi = 50),
      carries_pg = list(target = "target_carries_pg", lo = 0, hi = 18)
    ),
    RB = list(
      carry_share = list(target = "target_carry_share", lo = 0, hi = 0.85),
      target_share = list(target = "target_target_share", lo = 0, hi = 0.35)
    ),
    WR = list(
      target_share = list(target = "target_target_share", lo = 0, hi = 0.42),
      carries_pg = list(target = "target_carries_pg", lo = 0, hi = 5)
    ),
    TE = list(
      target_share = list(target = "target_target_share", lo = 0, hi = 0.35)
    ),
    stop("Unsupported position: ", pos)
  )
}

fit_opportunity_models20 <- function(data, position, n_trees = OPPORTUNITY_FINAL_TREES, seed = SEED) {
  specs <- get_opportunity_specs20(position)
  out <- list(); i <- 0
  for (nm in names(specs)) {
    i <- i + 1
    sp <- specs[[nm]]
    if (!sp$target %in% names(data)) next
    target <- num20(data[[sp$target]])
    d <- data[is.finite(target), , drop = FALSE]
    feats <- opportunity_feature_bank20(position, nm, names(d))
    if (nrow(d) < 35 || length(feats) < 5) next
    obj <- fit_fantasy_model(
      d, feats, sp$target,
      seed = seed + i * 1000,
      n_trees = n_trees
    )
    obj$component <- nm
    obj$position <- toupper(position)
    obj$clip_lo <- sp$lo
    obj$clip_hi <- sp$hi
    out[[nm]] <- obj
  }
  out
}

predict_opportunity_models20 <- function(models, new_data) {
  out <- list(); sds <- list()
  for (nm in names(models)) {
    detail <- predict_fantasy_model_detail(models[[nm]], new_data)
    out[[nm]] <- clip20(detail$model_prediction, models[[nm]]$clip_lo, models[[nm]]$clip_hi)
    sds[[nm]] <- clip20(detail$model_disagreement_sd, 0, Inf)
  }
  list(predictions = as.data.frame(out), disagreement = as.data.frame(sds))
}

# ------------------------------------------------------------
# Shrinkage efficiency / TD rate models
# ------------------------------------------------------------
get_rate_specs20 <- function(position) {
  pos <- toupper(as.character(position)[1])
  B <- RATE_BOUNDS_20[[pos]]
  switch(
    pos,
    QB = list(
      pass_ypa = list(target = "target_yards_per_attempt", prior = "prior_yards_per_attempt", career = "career_yards_per_attempt", prior_n = "prior_attempts_n", career_n = "career_attempts_before", target_n = "target_attempts_n", kind = "efficiency", context = "none", bounds = B$pass_ypa),
      int_rate = list(target = "target_interception_rate", prior = "prior_interception_rate", career = "career_interception_rate", prior_n = "prior_attempts_n", career_n = "career_attempts_before", target_n = "target_attempts_n", kind = "efficiency", context = "none", bounds = B$int_rate),
      rush_ypc = list(target = "target_yards_per_carry", prior = "prior_yards_per_carry", career = "career_yards_per_carry", prior_n = "prior_carries_n", career_n = "career_carries_before", target_n = "target_carries_n", kind = "efficiency", context = "none", bounds = B$rush_ypc),
      pass_td_rate = list(target = "target_pass_td_rate", prior = "prior_pass_td_rate", career = "career_pass_td_rate", prior_n = "prior_attempts_n", career_n = "career_attempts_before", target_n = "target_attempts_n", kind = "td", context = "pass_td", bounds = B$pass_td_rate),
      rush_td_rate = list(target = "target_rush_td_rate", prior = "prior_rush_td_rate", career = "career_rush_td_rate", prior_n = "prior_carries_n", career_n = "career_carries_before", target_n = "target_carries_n", kind = "td", context = "rush_td", bounds = B$rush_td_rate)
    ),
    RB = list(
      catch_rate = list(target = "target_catch_rate", prior = "prior_catch_rate", career = "career_catch_rate", prior_n = "prior_targets_n", career_n = "career_targets_before", target_n = "target_targets_n", kind = "efficiency", context = "none", bounds = B$catch_rate),
      ypt = list(target = "target_yards_per_target", prior = "prior_yards_per_target", career = "career_yards_per_target", prior_n = "prior_targets_n", career_n = "career_targets_before", target_n = "target_targets_n", kind = "efficiency", context = "none", bounds = B$ypt),
      rush_ypc = list(target = "target_yards_per_carry", prior = "prior_yards_per_carry", career = "career_yards_per_carry", prior_n = "prior_carries_n", career_n = "career_carries_before", target_n = "target_carries_n", kind = "efficiency", context = "none", bounds = B$rush_ypc),
      rec_td_rate = list(target = "target_rec_td_rate", prior = "prior_rec_td_rate", career = "career_rec_td_rate", prior_n = "prior_targets_n", career_n = "career_targets_before", target_n = "target_targets_n", kind = "td", context = "rec_td", bounds = B$rec_td_rate),
      rush_td_rate = list(target = "target_rush_td_rate", prior = "prior_rush_td_rate", career = "career_rush_td_rate", prior_n = "prior_carries_n", career_n = "career_carries_before", target_n = "target_carries_n", kind = "td", context = "rush_td", bounds = B$rush_td_rate)
    ),
    WR = list(
      catch_rate = list(target = "target_catch_rate", prior = "prior_catch_rate", career = "career_catch_rate", prior_n = "prior_targets_n", career_n = "career_targets_before", target_n = "target_targets_n", kind = "efficiency", context = "none", bounds = B$catch_rate),
      ypt = list(target = "target_yards_per_target", prior = "prior_yards_per_target", career = "career_yards_per_target", prior_n = "prior_targets_n", career_n = "career_targets_before", target_n = "target_targets_n", kind = "efficiency", context = "none", bounds = B$ypt),
      rush_ypc = list(target = "target_yards_per_carry", prior = "prior_yards_per_carry", career = "career_yards_per_carry", prior_n = "prior_carries_n", career_n = "career_carries_before", target_n = "target_carries_n", kind = "efficiency", context = "none", bounds = B$rush_ypc),
      rec_td_rate = list(target = "target_rec_td_rate", prior = "prior_rec_td_rate", career = "career_rec_td_rate", prior_n = "prior_targets_n", career_n = "career_targets_before", target_n = "target_targets_n", kind = "td", context = "rec_td", bounds = B$rec_td_rate),
      rush_td_rate = list(target = "target_rush_td_rate", prior = "prior_rush_td_rate", career = "career_rush_td_rate", prior_n = "prior_carries_n", career_n = "career_carries_before", target_n = "target_carries_n", kind = "td", context = "rush_td", bounds = B$rush_td_rate)
    ),
    TE = list(
      catch_rate = list(target = "target_catch_rate", prior = "prior_catch_rate", career = "career_catch_rate", prior_n = "prior_targets_n", career_n = "career_targets_before", target_n = "target_targets_n", kind = "efficiency", context = "none", bounds = B$catch_rate),
      ypt = list(target = "target_yards_per_target", prior = "prior_yards_per_target", career = "career_yards_per_target", prior_n = "prior_targets_n", career_n = "career_targets_before", target_n = "target_targets_n", kind = "efficiency", context = "none", bounds = B$ypt),
      rec_td_rate = list(target = "target_rec_td_rate", prior = "prior_rec_td_rate", career = "career_rec_td_rate", prior_n = "prior_targets_n", career_n = "career_targets_before", target_n = "target_targets_n", kind = "td", context = "rec_td", bounds = B$rec_td_rate)
    ),
    stop("Unsupported position: ", pos)
  )
}

player_rate20 <- function(prior_rate, prior_n, career_rate, career_n, recent_weight = 0.65) {
  prior_rate <- num20(prior_rate); prior_n <- pmax(0, num20(prior_n))
  career_rate <- num20(career_rate); career_n <- pmax(0, num20(career_n))
  has_prior <- is.finite(prior_rate) & prior_n > 0
  has_career <- is.finite(career_rate) & career_n > 0
  out <- rep(NA_real_, max(length(prior_rate), length(career_rate)))
  both <- has_prior & has_career
  out[both] <- recent_weight * prior_rate[both] + (1 - recent_weight) * career_rate[both]
  out[has_prior & !has_career] <- prior_rate[has_prior & !has_career]
  out[!has_prior & has_career] <- career_rate[!has_prior & has_career]
  out
}

shrink_rate20 <- function(prior_rate, prior_n, career_rate, career_n, league_mean, k, recent_weight) {
  pr <- player_rate20(prior_rate, prior_n, career_rate, career_n, recent_weight)
  prior_n <- pmax(0, num20(prior_n)); career_n <- pmax(0, num20(career_n))
  # Career history helps, but recent exposure receives substantially more weight.
  effective_n <- pmin(500, prior_n + 0.25 * career_n)
  pr[!is.finite(pr)] <- league_mean
  effective_n[!is.finite(effective_n)] <- 0
  (effective_n * pr + k * league_mean) / pmax(1e-8, effective_n + k)
}

positive_mean20 <- function(x, default = 1) {
  x <- num20(x)
  x <- x[is.finite(x) & x > 0]
  if (length(x) == 0) default else mean(x)
}

context_baselines20 <- function(train) {
  list(
    rec_rz = positive_mean20(train$rec_prior_redzone_target_rate, 0.10),
    team_rz_pass = positive_mean20(train$team_prior_redzone_pass_rate, 0.50),
    team_rz_rush = positive_mean20(train$team_prior_redzone_rush_rate, 0.50),
    qb_rz = positive_mean20(train$qb_prior_redzone_throw_rate, 0.10),
    player_qb_rz = positive_mean20(train$player_qb_prior_redzone_throw_rate, 0.10),
    team_points = positive_mean20(train$team_prior_points_pg, 15)
  )
}

ratio_or_one20 <- function(x, baseline) {
  x <- num20(x)
  if (!is.finite(baseline) || baseline <= 0) return(rep(1, length(x)))
  out <- x / baseline
  out[!is.finite(out) | x <= 0] <- 1
  clip20(out, 0.5, 1.75)
}

context_index20 <- function(data, context_type, baselines) {
  n <- nrow(data)
  if (n == 0 || identical(context_type, "none")) return(rep(1, n))
  getv <- function(nm) if (nm %in% names(data)) data[[nm]] else rep(0, n)

  if (context_type == "rec_td") {
    a <- ratio_or_one20(getv("rec_prior_redzone_target_rate"), baselines$rec_rz)
    b <- ratio_or_one20(getv("team_prior_redzone_pass_rate"), baselines$team_rz_pass)
    c <- ratio_or_one20(getv("qb_prior_redzone_throw_rate"), baselines$qb_rz)
    return(clip20((0.50 * a + 0.30 * b + 0.20 * c), 0.65, 1.40))
  }
  if (context_type == "pass_td") {
    a <- ratio_or_one20(getv("team_prior_redzone_pass_rate"), baselines$team_rz_pass)
    b <- ratio_or_one20(getv("team_prior_points_pg"), baselines$team_points)
    c <- ratio_or_one20(getv("player_qb_prior_redzone_throw_rate"), baselines$player_qb_rz)
    return(clip20((0.45 * a + 0.30 * b + 0.25 * c), 0.65, 1.40))
  }
  if (context_type == "rush_td") {
    a <- ratio_or_one20(getv("team_prior_redzone_rush_rate"), baselines$team_rz_rush)
    b <- ratio_or_one20(getv("team_prior_points_pg"), baselines$team_points)
    return(clip20((0.70 * a + 0.30 * b), 0.65, 1.40))
  }
  rep(1, n)
}

fit_rate_params20 <- function(train, position, component) {
  specs <- get_rate_specs20(position)
  sp <- specs[[component]]
  if (is.null(sp)) stop("Unknown rate component: ", component)
  needed <- c(sp$target, sp$prior, sp$career, sp$prior_n, sp$career_n, sp$target_n)
  if (!all(needed %in% names(train))) stop("Missing shrinkage columns for ", position, " ", component)

  actual <- num20(train[[sp$target]])
  exposure <- pmax(0, num20(train[[sp$target_n]]))
  keep <- is.finite(actual) & is.finite(exposure) & exposure > 0
  if (sum(keep) < 20) {
    fallback_mean <- mean(actual[is.finite(actual)], na.rm = TRUE)
    if (!is.finite(fallback_mean)) fallback_mean <- mean(sp$bounds)
    return(data.frame(
      position = position, component = component, kind = sp$kind, context = sp$context,
      league_mean = fallback_mean, k = 80,
      recent_weight = 0.65, context_strength = 0,
      context_mean_rec_rz = 0.10, context_mean_team_rz_pass = 0.50,
      context_mean_team_rz_rush = 0.50, context_mean_qb_rz = 0.10,
      context_mean_player_qb_rz = 0.10, context_mean_team_points = 15,
      validation_MAE = NA_real_, validation_RMSE = NA_real_, n = sum(keep)
    ))
  }

  # Exposure-weighted league baseline is more stable than the unweighted mean
  # of noisy player rates.
  league_mean <- weighted_mean_safe20(actual[keep], pmin(exposure[keep], 200))
  if (!is.finite(league_mean)) league_mean <- mean(actual[keep], na.rm = TRUE)
  bases <- context_baselines20(train[keep, , drop = FALSE])
  strengths <- if (sp$kind == "td") TD_CONTEXT_STRENGTH_CANDIDATES else 0

  candidates <- list()
  for (rw in SHRINK_RECENCY_WEIGHTS) {
    for (k in SHRINK_K_CANDIDATES) {
      base <- shrink_rate20(
        train[[sp$prior]], train[[sp$prior_n]], train[[sp$career]], train[[sp$career_n]],
        league_mean = league_mean, k = k, recent_weight = rw
      )
      idx <- context_index20(train, sp$context, bases)
      for (strength in strengths) {
        pred <- base * (1 + strength * (idx - 1))
        pred <- clip20(pred, sp$bounds[1], sp$bounds[2])
        ok <- keep & is.finite(pred)
        if (sum(ok) < 10) next
        candidates[[length(candidates) + 1]] <- data.frame(
          k = k, recent_weight = rw, context_strength = strength,
          MAE = mean(abs(pred[ok] - actual[ok])),
          RMSE = sqrt(mean((pred[ok] - actual[ok])^2)),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  cand <- dplyr::bind_rows(candidates) |>
    dplyr::arrange(MAE, RMSE, dplyr::desc(k))
  if (nrow(cand) == 0) stop("No shrinkage candidates for ", position, " ", component)
  best <- cand[1, , drop = FALSE]

  data.frame(
    position = position, component = component, kind = sp$kind, context = sp$context,
    league_mean = league_mean,
    k = best$k, recent_weight = best$recent_weight, context_strength = best$context_strength,
    context_mean_rec_rz = bases$rec_rz,
    context_mean_team_rz_pass = bases$team_rz_pass,
    context_mean_team_rz_rush = bases$team_rz_rush,
    context_mean_qb_rz = bases$qb_rz,
    context_mean_player_qb_rz = bases$player_qb_rz,
    context_mean_team_points = bases$team_points,
    validation_MAE = best$MAE, validation_RMSE = best$RMSE, n = sum(keep),
    stringsAsFactors = FALSE
  )
}

apply_rate_params20 <- function(data, position, component, params_row) {
  sp <- get_rate_specs20(position)[[component]]
  if (is.null(sp)) stop("Unknown rate component: ", component)
  if (nrow(params_row) == 0) stop("Missing shrinkage parameters: ", position, " ", component)
  p <- params_row[1, , drop = FALSE]
  bases <- list(
    rec_rz = num20(p$context_mean_rec_rz),
    team_rz_pass = num20(p$context_mean_team_rz_pass),
    team_rz_rush = num20(p$context_mean_team_rz_rush),
    qb_rz = num20(p$context_mean_qb_rz),
    player_qb_rz = num20(p$context_mean_player_qb_rz),
    team_points = num20(p$context_mean_team_points)
  )
  league_mean <- num20(p$league_mean)
  if (!is.finite(league_mean)) league_mean <- mean(sp$bounds)
  k <- num20(p$k); if (!is.finite(k) || k <= 0) k <- 80
  recent_weight <- num20(p$recent_weight); if (!is.finite(recent_weight)) recent_weight <- 0.65
  base <- shrink_rate20(
    data[[sp$prior]], data[[sp$prior_n]], data[[sp$career]], data[[sp$career_n]],
    league_mean = league_mean, k = k, recent_weight = recent_weight
  )
  idx <- context_index20(data, sp$context, bases)
  pred <- base * (1 + num20(p$context_strength) * (idx - 1))
  clip20(pred, sp$bounds[1], sp$bounds[2])
}

fit_all_rate_params20 <- function(train, position) {
  specs <- get_rate_specs20(position)
  dplyr::bind_rows(lapply(names(specs), function(nm) fit_rate_params20(train, position, nm)))
}

predict_all_rates20 <- function(data, position, params) {
  specs <- get_rate_specs20(position)
  out <- list()
  for (nm in names(specs)) {
    row <- params |> dplyr::filter(.data$position == position, .data$component == nm)
    if (nrow(row) == 0) next
    out[[nm]] <- apply_rate_params20(data, position, nm, row)
  }
  as.data.frame(out)
}

# ------------------------------------------------------------
# Recombine structured projections into a stat line / fantasy points.
# ------------------------------------------------------------
compose_statline20 <- function(position, opportunity, rates, data) {
  pos <- toupper(as.character(position)[1])
  n <- nrow(data)
  getp <- function(df, nm, default = 0) if (nm %in% names(df)) clip20(df[[nm]]) else rep(default, n)
  team_pass <- if ("team_projected_pass_attempts_pg" %in% names(data)) clip20(data$team_projected_pass_attempts_pg, 15, 50) else clip20(data$team_prior_pass_attempts_pg, 15, 50)
  team_carry <- if ("team_projected_carries_pg" %in% names(data)) clip20(data$team_projected_carries_pg, 15, 40) else clip20(data$team_prior_carries_pg, 15, 40)

  if (pos == "QB") {
    pass_att <- getp(opportunity, "pass_attempts_pg")
    carries <- getp(opportunity, "carries_pg")
    ypa <- getp(rates, "pass_ypa", 6.8)
    int_rate <- getp(rates, "int_rate", 0.025)
    rush_ypc <- getp(rates, "rush_ypc", 4.0)
    pass_td_rate <- getp(rates, "pass_td_rate", 0.045)
    rush_td_rate <- getp(rates, "rush_td_rate", 0.04)
    pass_yd <- pass_att * ypa
    pass_td <- pass_att * pass_td_rate
    interceptions <- pass_att * int_rate
    rush_yd <- carries * rush_ypc
    rush_td <- carries * rush_td_rate
    fppg <- pass_yd * SCORING$pass_yd + pass_td * SCORING$pass_td + interceptions * SCORING$interception +
      rush_yd * SCORING$rush_yd + rush_td * SCORING$rush_td
    return(data.frame(
      projected_pass_attempts_pg = pass_att,
      projected_pass_yards_pg = pass_yd,
      projected_pass_tds_pg = pass_td,
      projected_interceptions_pg = interceptions,
      projected_carries_pg = carries,
      projected_rush_yards_pg = rush_yd,
      projected_rush_tds_pg = rush_td,
      projected_pass_ypa = ypa,
      projected_interception_rate = int_rate,
      projected_rush_ypc = rush_ypc,
      opportunity_fppg_raw = pmax(0, fppg)
    ))
  }

  target_share <- getp(opportunity, "target_share")
  targets <- team_pass * target_share
  catch_rate <- getp(rates, "catch_rate", 0.65)
  ypt <- getp(rates, "ypt", 7.0)
  rec_td_rate <- getp(rates, "rec_td_rate", 0.045)
  receptions <- targets * catch_rate
  rec_yd <- targets * ypt
  rec_td <- targets * rec_td_rate

  if (pos == "RB") {
    carry_share <- getp(opportunity, "carry_share")
    carries <- team_carry * carry_share
    rush_ypc <- getp(rates, "rush_ypc", 4.2)
    rush_td_rate <- getp(rates, "rush_td_rate", 0.035)
    rush_yd <- carries * rush_ypc
    rush_td <- carries * rush_td_rate
    fppg <- rush_yd * SCORING$rush_yd + rush_td * SCORING$rush_td +
      receptions * SCORING$reception + rec_yd * SCORING$rec_yd + rec_td * SCORING$rec_td
    return(data.frame(
      projected_carry_share = carry_share,
      projected_carries_pg = carries,
      projected_rush_yards_pg = rush_yd,
      projected_rush_tds_pg = rush_td,
      projected_rush_ypc = rush_ypc,
      projected_target_share = target_share,
      projected_targets_pg = targets,
      projected_receptions_pg = receptions,
      projected_rec_yards_pg = rec_yd,
      projected_rec_tds_pg = rec_td,
      projected_catch_rate = catch_rate,
      projected_yards_per_target = ypt,
      opportunity_fppg_raw = pmax(0, fppg)
    ))
  }

  carries <- if (pos == "WR") getp(opportunity, "carries_pg") else rep(0, n)
  rush_ypc <- if (pos == "WR") getp(rates, "rush_ypc", 5.0) else rep(0, n)
  rush_td_rate <- if (pos == "WR") getp(rates, "rush_td_rate", 0.02) else rep(0, n)
  rush_yd <- carries * rush_ypc
  rush_td <- carries * rush_td_rate
  fppg <- receptions * SCORING$reception + rec_yd * SCORING$rec_yd + rec_td * SCORING$rec_td +
    rush_yd * SCORING$rush_yd + rush_td * SCORING$rush_td
  data.frame(
    projected_target_share = target_share,
    projected_targets_pg = targets,
    projected_receptions_pg = receptions,
    projected_rec_yards_pg = rec_yd,
    projected_rec_tds_pg = rec_td,
    projected_catch_rate = catch_rate,
    projected_yards_per_target = ypt,
    projected_carries_pg = carries,
    projected_rush_yards_pg = rush_yd,
    projected_rush_tds_pg = rush_td,
    opportunity_fppg_raw = pmax(0, fppg)
  )
}

# ------------------------------------------------------------
# Residual correction: learn only the remaining FPPG error after
# the structured projection. Direct 1.2 prediction may be used as
# a disagreement feature, but the correction is capped.
# ------------------------------------------------------------
# HOTFIX 5: residual features must be position-specific.  When historical OOF
# rows from multiple positions are bound together, bind_rows() creates the union
# of every component column.  A QB-only slice can therefore *have a column name*
# such as projected_target_share even though every QB value is NA.  The old
# residual feature bank selected by name alone, so a QB residual tree could save
# WR/RB-only features and then fail when predicting a standalone QB frame.
residual_component_features20 <- function(position) {
  pos <- toupper(as.character(position)[1])
  switch(
    pos,
    QB = c(
      "projected_pass_attempts_pg", "projected_carries_pg",
      "projected_pass_yards_pg", "projected_pass_tds_pg",
      "projected_interceptions_pg", "projected_rush_yards_pg",
      "projected_rush_tds_pg"
    ),
    RB = c(
      "projected_carry_share", "projected_carries_pg",
      "projected_target_share", "projected_targets_pg",
      "projected_receptions_pg", "projected_rush_yards_pg",
      "projected_rec_yards_pg", "projected_rush_tds_pg",
      "projected_rec_tds_pg"
    ),
    WR = c(
      "projected_target_share", "projected_targets_pg",
      "projected_receptions_pg", "projected_carries_pg",
      "projected_rec_yards_pg", "projected_rush_yards_pg",
      "projected_rec_tds_pg", "projected_rush_tds_pg"
    ),
    TE = c(
      "projected_target_share", "projected_targets_pg",
      "projected_receptions_pg", "projected_rec_yards_pg",
      "projected_rec_tds_pg"
    ),
    character()
  )
}

residual_feature_bank20 <- function(position, available_names = NULL) {
  pos <- toupper(as.character(position)[1])
  common <- c(
    "opportunity_fppg_calibrated", "direct_1_2_fppg", "base_direct_gap",
    "prior_fppg", "two_year_fppg", "recent_weighted_fppg", "fppg_trend",
    "prior_games", "experience", "is_rookie", "age", "draft_capital_score",
    "team_projected_pass_attempts_pg", "team_projected_carries_pg",
    "target_competition_index", "carry_competition_index"
  )
  position_context <- switch(
    pos,
    QB = c("qb_team_continuity"),
    RB = c("rb_receiving_fit"),
    WR = c("wr_context_fit_score"),
    TE = c("te_context_fit_score"),
    character()
  )

  feats <- unique(c(
    common,
    residual_component_features20(pos),
    position_context,
    utils::head(POSITION_CONTEXT_FEATURES[[pos]], 12)
  ))
  if (!is.null(available_names)) feats <- intersect(feats, available_names)
  feats
}

# Drop columns that exist only because bind_rows() created a cross-position
# union, plus any other all-missing/constant fields that cannot help a tree.
usable_residual_features20 <- function(data, features) {
  features <- intersect(features, names(data))
  if (length(features) == 0 || nrow(data) == 0) return(character())
  keep <- vapply(features, function(nm) {
    x <- num20(data[[nm]])
    x <- x[is.finite(x)]
    if (length(x) < 8) return(FALSE)
    length(unique(x)) > 1 && stats::sd(x) > 1e-12
  }, logical(1))
  features[keep]
}

fit_residual_model20 <- function(data, position, n_trees = RESIDUAL_FINAL_TREES, seed = SEED) {
  if (!"residual_target_20" %in% names(data) || nrow(data) < RESIDUAL_MIN_ROWS) return(NULL)
  feats <- residual_feature_bank20(position, names(data))
  feats <- usable_residual_features20(data, feats)
  if (length(feats) < 5) return(NULL)

  fit <- fit_fantasy_model(data, feats, "residual_target_20", seed = seed, n_trees = n_trees)
  fit$position <- toupper(as.character(position)[1])
  fit$residual_features_20 <- feats
  # prepare_feature_frame() uses zero for non-finite training values, so zero is
  # the schema-safe default if an otherwise legitimate trained feature is absent
  # from a future frame. Position-inapplicable component fields are already
  # excluded above; this is only a final runtime guardrail.
  fit$residual_feature_defaults_20 <- stats::setNames(rep(0, length(feats)), feats)
  fit
}

align_residual_prediction_frame20 <- function(model, new_data) {
  if (is.null(model)) return(new_data)
  needed <- unique(c(model$features, unlist(lapply(model$trees, function(x) x$features), use.names = FALSE)))
  missing <- setdiff(needed, names(new_data))
  if (length(missing) > 0) {
    defaults <- model$residual_feature_defaults_20
    for (nm in missing) {
      val <- 0
      if (!is.null(defaults) && nm %in% names(defaults) && is.finite(num20(defaults[[nm]])[1])) {
        val <- num20(defaults[[nm]])[1]
      }
      new_data[[nm]] <- rep(val, nrow(new_data))
    }
    message("[2.0 RESIDUAL] Added missing trained feature(s) using safe defaults: ", paste(missing, collapse = ", "))
  }
  new_data
}

predict_residual20 <- function(model, new_data) {
  if (is.null(model)) return(rep(0, nrow(new_data)))
  new_data <- align_residual_prediction_frame20(model, new_data)
  clip20(predict_fantasy_model(model, new_data), -RESIDUAL_CLIP_FPPG, RESIDUAL_CLIP_FPPG)
}

get_architecture_weight20 <- function(position, path = "output/selected_2_0_architecture_weights.csv") {
  if (!file.exists(path)) return(0)
  x <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || !all(c("position", "selected_opportunity_weight") %in% names(x))) return(0)
  w <- x$selected_opportunity_weight[x$position == position]
  if (length(w) == 0 || !is.finite(w[1])) 0 else as.numeric(w[1])
}

get_2_0_calibration <- function(position, path = "output/selected_2_0_calibration.csv") {
  default <- list(intercept = 0, slope = 1, residual_sd = 0)
  if (!file.exists(path)) return(default)
  x <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || !all(c("position", "intercept", "slope", "residual_sd") %in% names(x))) return(default)
  r <- x[x$position == position, , drop = FALSE]
  if (nrow(r) == 0) return(default)
  list(intercept = num20(r$intercept[1]), slope = num20(r$slope[1]), residual_sd = num20(r$residual_sd[1]))
}
