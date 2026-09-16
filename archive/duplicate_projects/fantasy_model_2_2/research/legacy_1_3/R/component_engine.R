# ============================================================
# FANTASY MODEL 1.3 - COMPONENT PROJECTION ENGINE
# ============================================================
# This layer decomposes fantasy production into team volume,
# player opportunity, efficiency and touchdown components.
# It intentionally reuses the mobile-safe rpart ensemble helpers
# defined in config.R.

component_clip <- function(x, lo = -Inf, hi = Inf) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- 0
  pmin(hi, pmax(lo, x))
}

COMPONENT_BLEND_CANDIDATES <- c(0, 0.25, 0.50, 0.75, 1.00)
DEFAULT_COMPONENT_WEIGHT <- 0.50
COMPONENT_FINAL_TREES <- 12
COMPONENT_VALIDATION_TREES <- 6
TEAM_VOLUME_FINAL_TREES <- 16
TEAM_VOLUME_VALIDATION_TREES <- 8

TEAM_VOLUME_FEATURES <- c(
  "team_prior_pass_attempts_pg", "team_prior_carries_pg",
  "team_prior_pass_yd_pg", "team_prior_rush_yd_pg", "team_prior_points_pg",
  "team_prior_pass_rate", "team_prior_neutral_pass_rate", "team_prior_plays_pg",
  "team_prior_redzone_pass_rate", "team_prior_deep_throw_rate",
  "team_prior_shotgun_rate", "team_prior_no_huddle_rate", "team_prior_avg_air_yards"
)

COMPONENT_EXTRA_FEATURES <- c(
  "team_projected_pass_attempts_pg", "team_projected_carries_pg",
  "prior_teammate_max_target_share", "prior_teammate_top2_target_share",
  "prior_teammate_target_hhi", "prior_teammate_max_carry_share",
  "prior_teammate_top2_carry_share", "prior_teammate_carry_hhi",
  "target_competition_index", "carry_competition_index",
  "qb_wr_deep_synergy", "qb_wr_redzone_synergy", "qb_te_middle_synergy",
  "qb_te_redzone_synergy", "qb_rb_receiving_synergy",
  "team_pass_opportunity_index", "team_rush_opportunity_index"
)

component_feature_bank <- function(position, family = c("opportunity", "efficiency", "td"), available_names = NULL) {
  family <- match.arg(family)
  pos <- toupper(position)
  core <- get_model_features(pos)

  opportunity <- c(
    "prior_fppg", "two_year_fppg", "recent_weighted_fppg", "fppg_trend",
    "prior_games", "two_year_games", "career_games_before", "experience", "is_rookie",
    "age", "age_squared", "draft_round", "draft_pick", "draft_capital_score",
    "prior_targets_pg", "two_year_targets_pg", "prior_carries_pg", "two_year_carries_pg",
    "prior_pass_attempts_pg", "two_year_pass_attempts_pg", "prior_target_share", "prior_wopr",
    "team_projected_pass_attempts_pg", "team_projected_carries_pg",
    "team_prior_plays_pg", "team_prior_pass_rate", "team_prior_neutral_pass_rate",
    "team_prior_redzone_pass_rate", "team_prior_shotgun_rate", "team_prior_no_huddle_rate",
    "prior_teammate_max_target_share", "prior_teammate_top2_target_share",
    "prior_teammate_target_hhi", "prior_teammate_max_carry_share",
    "prior_teammate_top2_carry_share", "prior_teammate_carry_hhi",
    "target_competition_index", "carry_competition_index",
    "team_pass_opportunity_index", "team_rush_opportunity_index",
    "qb_team_continuity", "qb_prior_wr_target_rate", "qb_prior_te_target_rate", "qb_prior_rb_target_rate"
  )

  efficiency <- c(
    "prior_fppg", "two_year_fppg", "prior_games", "experience", "age", "age_squared",
    "height", "weight", "draft_capital_score",
    "prior_catch_rate", "prior_yards_per_target", "prior_yards_per_carry", "prior_yards_per_attempt",
    "prior_air_yards_share", "prior_wopr",
    "rec_prior_avg_depth_target", "rec_prior_deep_target_rate", "rec_prior_short_target_rate",
    "rec_prior_middle_target_rate", "rec_prior_route_vertical", "rec_prior_route_in_break",
    "rec_prior_route_out_break", "rec_prior_route_screen", "rec_prior_route_hitch",
    "rec_prior_ngs_avg_cushion", "rec_prior_ngs_avg_separation",
    "qb_prior_avg_air_yards", "qb_prior_deep_throw_rate", "qb_prior_middle_throw_rate",
    "qb_prior_ngs_time_to_throw", "qb_prior_ngs_cpoe",
    "player_qb_prior_avg_air_yards", "player_qb_prior_deep_throw_rate",
    "player_qb_prior_ngs_time_to_throw", "player_qb_prior_ngs_cpoe",
    "qb_receiver_depth_fit", "qb_receiver_location_fit", "qb_receiver_route_fit",
    "wr_context_fit_score", "te_middle_fit", "te_route_fit", "te_context_fit_score",
    "qb_wr_deep_synergy", "qb_te_middle_synergy", "qb_rb_receiving_synergy"
  )

  td <- c(
    "prior_fppg", "recent_weighted_fppg", "prior_games", "experience", "age",
    "prior_pass_td_rate", "prior_rush_td_rate", "prior_rec_td_rate",
    "team_prior_redzone_pass_rate", "team_prior_redzone_rush_rate",
    "rec_prior_redzone_target_rate", "qb_prior_redzone_throw_rate", "player_qb_prior_redzone_throw_rate",
    "qb_prior_wr_target_rate", "qb_prior_te_target_rate", "qb_prior_rb_target_rate",
    "qb_wr_redzone_synergy", "qb_te_redzone_synergy", "qb_rb_receiving_synergy",
    "team_prior_points_pg", "team_projected_pass_attempts_pg", "team_projected_carries_pg",
    "prior_target_share", "prior_carries_pg", "prior_targets_pg", "draft_capital_score"
  )

  bank <- switch(family, opportunity = opportunity, efficiency = efficiency, td = td)
  # Keep the position-specific 1.2 context available, but only after the
  # family-specific variables so feature subsampling favors relevant context.
  bank <- unique(c(bank, intersect(core, POSITION_CONTEXT_FEATURES[[pos]]), COMPONENT_EXTRA_FEATURES))
  if (!is.null(available_names)) bank <- intersect(bank, available_names)
  bank
}

get_component_specs <- function(position) {
  pos <- toupper(position)
  switch(
    pos,
    QB = list(
      pass_attempts_pg = list(target = "target_pass_attempts_pg", family = "opportunity", lo = 0, hi = 50),
      pass_ypa = list(target = "target_yards_per_attempt", family = "efficiency", lo = 3, hi = 12),
      pass_td_rate = list(target = "target_pass_td_rate", family = "td", lo = 0, hi = 0.12),
      interception_rate = list(target = "target_interception_rate", family = "td", lo = 0, hi = 0.10),
      carries_pg = list(target = "target_carries_pg", family = "opportunity", lo = 0, hi = 18),
      rush_ypc = list(target = "target_yards_per_carry", family = "efficiency", lo = 0, hi = 10),
      rush_td_rate = list(target = "target_rush_td_rate", family = "td", lo = 0, hi = 0.25)
    ),
    RB = list(
      carry_share = list(target = "target_carry_share", family = "opportunity", lo = 0, hi = 1),
      target_share = list(target = "target_target_share", family = "opportunity", lo = 0, hi = 0.45),
      rush_ypc = list(target = "target_yards_per_carry", family = "efficiency", lo = 0, hi = 9),
      catch_rate = list(target = "target_catch_rate", family = "efficiency", lo = 0, hi = 1),
      ypt = list(target = "target_yards_per_target", family = "efficiency", lo = 0, hi = 15),
      rush_td_rate = list(target = "target_rush_td_rate", family = "td", lo = 0, hi = 0.25),
      rec_td_rate = list(target = "target_rec_td_rate", family = "td", lo = 0, hi = 0.20)
    ),
    WR = list(
      target_share = list(target = "target_target_share", family = "opportunity", lo = 0, hi = 0.45),
      catch_rate = list(target = "target_catch_rate", family = "efficiency", lo = 0, hi = 1),
      ypt = list(target = "target_yards_per_target", family = "efficiency", lo = 0, hi = 16),
      rec_td_rate = list(target = "target_rec_td_rate", family = "td", lo = 0, hi = 0.20),
      carries_pg = list(target = "target_carries_pg", family = "opportunity", lo = 0, hi = 8),
      rush_ypc = list(target = "target_yards_per_carry", family = "efficiency", lo = 0, hi = 15),
      rush_td_rate = list(target = "target_rush_td_rate", family = "td", lo = 0, hi = 0.30)
    ),
    TE = list(
      target_share = list(target = "target_target_share", family = "opportunity", lo = 0, hi = 0.40),
      catch_rate = list(target = "target_catch_rate", family = "efficiency", lo = 0, hi = 1),
      ypt = list(target = "target_yards_per_target", family = "efficiency", lo = 0, hi = 14),
      rec_td_rate = list(target = "target_rec_td_rate", family = "td", lo = 0, hi = 0.22)
    ),
    stop("Unsupported component position: ", pos)
  )
}

fit_component_models <- function(data, position, n_trees = COMPONENT_FINAL_TREES, seed = SEED) {
  specs <- get_component_specs(position)
  out <- list()
  idx <- 0
  for (nm in names(specs)) {
    idx <- idx + 1
    sp <- specs[[nm]]
    features <- component_feature_bank(position, sp$family, names(data))
    d <- data[is.finite(suppressWarnings(as.numeric(data[[sp$target]]))), , drop = FALSE]
    if (nrow(d) < 35 || length(features) < 5) next
    model <- fit_fantasy_model(
      d, features, sp$target, seed = seed + idx * 1000,
      n_trees = n_trees
    )
    model$component <- nm
    model$position <- position
    model$clip_lo <- sp$lo
    model$clip_hi <- sp$hi
    out[[nm]] <- model
  }
  out
}

predict_component_models <- function(model_list, new_data) {
  preds <- list()
  sds <- list()
  for (nm in names(model_list)) {
    obj <- model_list[[nm]]
    detail <- predict_fantasy_model_detail(obj, new_data)
    preds[[nm]] <- component_clip(detail$model_prediction, obj$clip_lo, obj$clip_hi)
    sds[[nm]] <- component_clip(detail$model_disagreement_sd, 0, Inf)
  }
  list(predictions = as.data.frame(preds), disagreement = as.data.frame(sds))
}

component_fantasy_projection <- function(position, component_predictions, data) {
  p <- component_predictions
  n <- nrow(data)
  z <- function(name, default = 0) {
    if (name %in% names(p)) component_clip(p[[name]]) else rep(default, n)
  }
  team_pass <- if ("team_projected_pass_attempts_pg" %in% names(data)) component_clip(data$team_projected_pass_attempts_pg, 15, 50) else component_clip(data$team_prior_pass_attempts_pg, 15, 50)
  team_carry <- if ("team_projected_carries_pg" %in% names(data)) component_clip(data$team_projected_carries_pg, 15, 40) else component_clip(data$team_prior_carries_pg, 15, 40)

  if (position == "QB") {
    pass_att <- z("pass_attempts_pg")
    pass_ypa <- z("pass_ypa")
    pass_td_rate <- z("pass_td_rate")
    int_rate <- z("interception_rate")
    carries <- z("carries_pg")
    rush_ypc <- z("rush_ypc")
    rush_td_rate <- z("rush_td_rate")
    pass_yd <- pass_att * pass_ypa
    pass_td <- pass_att * pass_td_rate
    interceptions <- pass_att * int_rate
    rush_yd <- carries * rush_ypc
    rush_td <- carries * rush_td_rate
    fppg <- pass_yd * SCORING$pass_yd + pass_td * SCORING$pass_td + interceptions * SCORING$interception +
      rush_yd * SCORING$rush_yd + rush_td * SCORING$rush_td
    return(data.frame(
      projected_pass_attempts_pg = pass_att, projected_pass_yards_pg = pass_yd,
      projected_pass_tds_pg = pass_td, projected_interceptions_pg = interceptions,
      projected_carries_pg = carries, projected_rush_yards_pg = rush_yd,
      projected_rush_tds_pg = rush_td, component_fppg_raw = pmax(0, fppg)
    ))
  }

  target_share <- z("target_share")
  targets <- team_pass * target_share
  catch_rate <- z("catch_rate")
  ypt <- z("ypt")
  rec_td_rate <- z("rec_td_rate")
  receptions <- targets * catch_rate
  rec_yd <- targets * ypt
  rec_td <- targets * rec_td_rate

  if (position == "RB") {
    carry_share <- z("carry_share")
    carries <- team_carry * carry_share
    rush_ypc <- z("rush_ypc")
    rush_td_rate <- z("rush_td_rate")
    rush_yd <- carries * rush_ypc
    rush_td <- carries * rush_td_rate
    fppg <- rush_yd * SCORING$rush_yd + rush_td * SCORING$rush_td +
      receptions * SCORING$reception + rec_yd * SCORING$rec_yd + rec_td * SCORING$rec_td
    return(data.frame(
      projected_carry_share = carry_share, projected_carries_pg = carries,
      projected_rush_yards_pg = rush_yd, projected_rush_tds_pg = rush_td,
      projected_target_share = target_share, projected_targets_pg = targets,
      projected_receptions_pg = receptions, projected_rec_yards_pg = rec_yd,
      projected_rec_tds_pg = rec_td, component_fppg_raw = pmax(0, fppg)
    ))
  }

  carries <- if (position == "WR") z("carries_pg") else rep(0, n)
  rush_ypc <- if (position == "WR") z("rush_ypc") else rep(0, n)
  rush_td_rate <- if (position == "WR") z("rush_td_rate") else rep(0, n)
  rush_yd <- carries * rush_ypc
  rush_td <- carries * rush_td_rate
  fppg <- receptions * SCORING$reception + rec_yd * SCORING$rec_yd + rec_td * SCORING$rec_td +
    rush_yd * SCORING$rush_yd + rush_td * SCORING$rush_td
  data.frame(
    projected_target_share = target_share, projected_targets_pg = targets,
    projected_receptions_pg = receptions, projected_rec_yards_pg = rec_yd,
    projected_rec_tds_pg = rec_td, projected_carries_pg = carries,
    projected_rush_yards_pg = rush_yd, projected_rush_tds_pg = rush_td,
    component_fppg_raw = pmax(0, fppg)
  )
}

get_selected_component_weight <- function(position, path = "output/selected_component_blend_weights.csv") {
  if (!file.exists(path)) return(DEFAULT_COMPONENT_WEIGHT)
  x <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || !all(c("position", "selected_component_weight") %in% names(x))) return(DEFAULT_COMPONENT_WEIGHT)
  w <- x$selected_component_weight[x$position == position]
  if (length(w) == 0 || !is.finite(w[1])) DEFAULT_COMPONENT_WEIGHT else as.numeric(w[1])
}

get_component_calibration <- function(position, path = "output/selected_component_calibration.csv") {
  default <- list(intercept = 0, slope = 1, residual_sd = 0)
  if (!file.exists(path)) return(default)
  x <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || !all(c("position", "intercept", "slope", "residual_sd") %in% names(x))) return(default)
  row <- x[x$position == position, , drop = FALSE]
  if (nrow(row) == 0) return(default)
  list(intercept = as.numeric(row$intercept[1]), slope = as.numeric(row$slope[1]), residual_sd = as.numeric(row$residual_sd[1]))
}

component_model_importance <- function(models, position) {
  rows <- list()
  for (nm in names(models)) {
    imp <- get_feature_importance(models[[nm]])
    if (nrow(imp) == 0) next
    imp$position <- position
    imp$component <- nm
    rows[[length(rows) + 1]] <- imp
  }
  dplyr::bind_rows(rows)
}
