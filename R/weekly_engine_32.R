# ============================================================
# FANTASY MODEL 3.2 - DENOISED ROLE + OFFENSIVE CONTEXT ENGINE
# ============================================================
# Challenger only. The 3.1/2.5 production forecast remains the incumbent.
# 3.2 deliberately reduces the feature surface and adds a compact set of
# strictly lagged offensive-context / opportunity composites.

if (!exists("wk31_fit", mode = "function")) source("R/weekly_engine_31.R")

MODEL32_OFFENSE_METRICS <- c(
  "off_plays", "off_pass_rate", "off_neutral_pass_rate", "off_redzone_pass_rate",
  "off_pass_epa", "off_rush_epa", "off_pass_success", "off_rush_success",
  "off_sack_allowed_rate", "off_qb_hit_allowed_rate", "off_deep_pass_rate",
  "off_explosive_pass_rate", "off_no_huddle_rate", "off_shotgun_rate"
)
MODEL32_OFFENSE_FEATURES <- paste0("team_", MODEL32_OFFENSE_METRICS, "_roll4")
MODEL32_DERIVED_FEATURES <- c(
  "recent_form_32", "role_stability_32",
  "expected_targets_32", "expected_carries_32", "expected_pass_attempts_32",
  "expected_touches_32", "scoring_role_32", "scoring_environment_32",
  "pace_role_32", "qb_rush_value_32", "pressure_exposure_32",
  "receiver_air_role_32", "receiver_redzone_role_32"
)
MODEL32_ALPHA_GRID <- c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
MODEL32_ALPHA_CAP <- c(QB = 0.20, RB = 0.25, WR = 0.30, TE = 0.25)

wk32_num <- function(x, default = 0) {
  z <- suppressWarnings(as.numeric(x))
  z[!is.finite(z)] <- default
  z
}

wk32_offense_context <- function(path = "data/raw/weekly_offense_context_raw.csv") {
  if (!file.exists(path)) return(data.frame())
  z <- tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
  if (!nrow(z) || !all(c("season","week","team") %in% names(z))) return(data.frame())
  z$season <- suppressWarnings(as.integer(z$season))
  z$week <- suppressWarnings(as.integer(z$week))
  z$team <- as.character(z$team)
  z
}

wk32_add_offense_context <- function(d, context = NULL) {
  if (!nrow(d)) return(d)
  if (is.null(context)) context <- wk32_offense_context()

  for (nm in MODEL32_OFFENSE_FEATURES) if (!nm %in% names(d)) d[[nm]] <- 0
  if (!nrow(context) || !all(c("season","week","team") %in% names(d))) return(d)

  keys <- unique(data.frame(
    season = suppressWarnings(as.integer(d$season)),
    week = suppressWarnings(as.integer(d$week)),
    team = as.character(d$team),
    stringsAsFactors = FALSE
  ))
  keys <- keys[is.finite(keys$season) & is.finite(keys$week) & nzchar(keys$team), , drop = FALSE]
  if (!nrow(keys)) return(d)

  rows <- vector("list", nrow(keys))
  for (i in seq_len(nrow(keys))) {
    k <- keys[i, , drop = FALSE]
    src <- context[
      context$season == k$season &
        as.character(context$team) == k$team &
        context$week < k$week,
      , drop = FALSE
    ]
    src <- src[order(src$week), , drop = FALSE]
    r <- k
    for (m in MODEL32_OFFENSE_METRICS) {
      vals <- if (m %in% names(src)) wk32_num(src[[m]], NA_real_) else numeric()
      vals <- vals[is.finite(vals)]
      v <- if (length(vals)) mean(utils::tail(vals, 4), na.rm = TRUE) else 0
      r[[paste0("team_", m, "_roll4")]] <- v
    }
    rows[[i]] <- r
  }
  ctx <- dplyr::bind_rows(rows)
  base <- d[, setdiff(names(d), MODEL32_OFFENSE_FEATURES), drop = FALSE]
  out <- dplyr::left_join(base, ctx, by = c("season","week","team"))
  for (nm in MODEL32_OFFENSE_FEATURES) {
    if (!nm %in% names(out)) out[[nm]] <- 0
    out[[nm]] <- wk32_num(out[[nm]], 0)
  }
  out
}

wk32_add_features <- function(d, offense_context = NULL) {
  if (!nrow(d)) return(d)
  d <- wk31_add_features(d)
  d <- wk32_add_offense_context(d, offense_context)

  r3 <- wk32_num(if ("roll3_fppg" %in% names(d)) d$roll3_fppg else 0)
  r5 <- wk32_num(if ("roll5_fppg" %in% names(d)) d$roll5_fppg else r3)
  o3 <- pmin(1, pmax(0, wk32_num(if ("roll3_offense_pct" %in% names(d)) d$roll3_offense_pct else 0)))
  o5 <- pmin(1, pmax(0, wk32_num(if ("roll5_offense_pct" %in% names(d)) d$roll5_offense_pct else o3)))
  tsh <- pmax(0, wk32_num(if ("roll3_target_share" %in% names(d)) d$roll3_target_share else 0))
  csh <- pmax(0, wk32_num(if ("roll3_carry_share" %in% names(d)) d$roll3_carry_share else 0))
  psh <- pmax(0, wk32_num(if ("roll3_pass_attempt_share" %in% names(d)) d$roll3_pass_attempt_share else 0))
  tpa <- pmax(0, wk32_num(if ("roll3_team_pass_attempts" %in% names(d)) d$roll3_team_pass_attempts else 0))
  tca <- pmax(0, wk32_num(if ("roll3_team_carries" %in% names(d)) d$roll3_team_carries else 0))
  itt <- pmax(0, wk32_num(if ("implied_team_total" %in% names(d)) d$implied_team_total else 0))
  pos <- toupper(as.character(d$position))

  d$recent_form_32 <- 0.65 * r3 + 0.35 * r5
  d$role_stability_32 <- pmax(0, 1 - 2 * abs(o3 - o5))
  d$expected_targets_32 <- tsh * tpa
  d$expected_carries_32 <- csh * tca
  d$expected_pass_attempts_32 <- psh * tpa
  d$expected_touches_32 <- d$expected_targets_32 + d$expected_carries_32

  d$scoring_role_32 <- ifelse(
    pos == "QB", psh,
    ifelse(pos == "RB", 0.70 * csh + 0.30 * tsh, tsh)
  )
  d$scoring_environment_32 <- itt * d$scoring_role_32
  d$pace_role_32 <- pmax(0, wk32_num(d$team_off_plays_roll4)) * d$scoring_role_32
  d$qb_rush_value_32 <- pmax(0, wk32_num(if ("roll5_carries" %in% names(d)) d$roll5_carries else 0)) *
    pmax(0, wk32_num(if ("season_to_date_rush_ypc" %in% names(d)) d$season_to_date_rush_ypc else 0))
  d$pressure_exposure_32 <- d$expected_pass_attempts_32 *
    pmax(0, wk32_num(d$team_off_sack_allowed_rate_roll4))
  d$receiver_air_role_32 <- d$expected_targets_32 *
    pmax(0, wk32_num(if ("preseason_deep_target_rate" %in% names(d)) d$preseason_deep_target_rate else 0))
  d$receiver_redzone_role_32 <- d$expected_targets_32 *
    pmax(0, wk32_num(if ("preseason_redzone_target_rate" %in% names(d)) d$preseason_redzone_target_rate else 0))
  d
}

MODEL32_CORE <- list(
  QB = c(
    "preseason_prior_fppg","prior_game_fppg","roll3_fppg","roll5_fppg","season_to_date_fppg",
    "roll3_pass_attempts","roll5_pass_attempts","roll3_pass_attempt_share",
    "roll3_offense_pct","roll5_offense_pct","roll3_offense_snaps",
    "roll3_carries","roll5_carries","season_to_date_rush_ypc",
    "roll3_ngs_time_to_throw","implied_team_total","team_spread_line","is_home","rest_days",
    "opp_pos_residual_roll8","def_pass_success_allowed_roll4","def_sack_rate_roll4",
    "def_qb_hit_rate_roll4","qb_pressure_matchup","opportunity_per_snap",
    "injury_risk","practice_risk","season_week",
    "adaptive_prior_fppg_31","role_change_score_31",
    "recent_form_32","role_stability_32","expected_pass_attempts_32",
    "scoring_environment_32","qb_rush_value_32","pressure_exposure_32"
  ),
  RB = c(
    "preseason_prior_fppg","prior_game_fppg","roll3_fppg","roll5_fppg","season_to_date_fppg",
    "roll3_carries","roll5_carries","roll3_targets","roll5_targets",
    "roll3_carry_share","roll3_target_share","roll3_offense_pct","roll5_offense_pct",
    "roll3_offense_snaps","roll3_team_carries","roll3_team_pass_attempts",
    "roll3_rush_ypc","season_to_date_rush_ypc","favorite_rush_interaction",
    "implied_team_total","team_spread_line","expected_team_plays","injury_risk","practice_risk",
    "season_week","adaptive_prior_fppg_31","role_change_score_31",
    "recent_form_32","role_stability_32","expected_carries_32","expected_targets_32",
    "expected_touches_32","scoring_environment_32","pace_role_32"
  ),
  WR = c(
    "preseason_prior_fppg","prior_game_fppg","roll3_fppg","roll5_fppg","season_to_date_fppg",
    "roll3_targets","roll5_targets","roll3_target_share","roll3_offense_pct","roll5_offense_pct",
    "roll3_offense_snaps","roll3_team_pass_attempts","implied_team_total","team_spread_line",
    "matchup_role_interaction","roll3_ngs_yac_oe","roll3_ngs_separation","roll3_ngs_cushion",
    "preseason_deep_target_rate","preseason_redzone_target_rate",
    "injury_risk","practice_risk","season_week","adaptive_prior_fppg_31","role_change_score_31",
    "recent_form_32","role_stability_32","expected_targets_32","scoring_environment_32",
    "pace_role_32","receiver_air_role_32","receiver_redzone_role_32"
  ),
  TE = c(
    "preseason_prior_fppg","prior_game_fppg","roll3_fppg","roll5_fppg","season_to_date_fppg",
    "roll3_targets","roll5_targets","roll3_target_share","roll3_offense_pct","roll5_offense_pct",
    "roll3_offense_snaps","roll3_team_pass_attempts","implied_team_total","team_spread_line",
    "matchup_role_interaction","opp_pos_residual_roll8",
    "preseason_middle_target_rate","preseason_redzone_target_rate",
    "injury_risk","practice_risk","season_week","adaptive_prior_fppg_31","role_change_score_31",
    "recent_form_32","role_stability_32","expected_targets_32","scoring_environment_32",
    "pace_role_32","receiver_redzone_role_32"
  )
)

MODEL32_CONTEXT_BY_POSITION <- list(
  QB = c(
    "team_off_plays_roll4","team_off_neutral_pass_rate_roll4","team_off_pass_epa_roll4",
    "team_off_pass_success_roll4","team_off_sack_allowed_rate_roll4",
    "team_off_qb_hit_allowed_rate_roll4","team_off_deep_pass_rate_roll4",
    "team_off_explosive_pass_rate_roll4","team_off_no_huddle_rate_roll4"
  ),
  RB = c(
    "team_off_plays_roll4","team_off_neutral_pass_rate_roll4","team_off_redzone_pass_rate_roll4",
    "team_off_rush_epa_roll4","team_off_rush_success_roll4","team_off_no_huddle_rate_roll4"
  ),
  WR = c(
    "team_off_plays_roll4","team_off_neutral_pass_rate_roll4","team_off_redzone_pass_rate_roll4",
    "team_off_pass_epa_roll4","team_off_pass_success_roll4","team_off_deep_pass_rate_roll4",
    "team_off_explosive_pass_rate_roll4","team_off_no_huddle_rate_roll4"
  ),
  TE = c(
    "team_off_plays_roll4","team_off_neutral_pass_rate_roll4","team_off_redzone_pass_rate_roll4",
    "team_off_pass_epa_roll4","team_off_pass_success_roll4","team_off_shotgun_rate_roll4"
  )
)

wk32_feature_sets <- function(pos) {
  pos <- toupper(as.character(pos)[1])
  core <- unique(MODEL32_CORE[[pos]])
  ctx <- unique(c(core, MODEL32_CONTEXT_BY_POSITION[[pos]]))
  full <- unique(c(MODEL31_SAFE_FEATURES, MODEL32_DERIVED_FEATURES, MODEL32_CONTEXT_BY_POSITION[[pos]]))
  list(
    denoise = core,
    denoise_context = ctx,
    full_context = full
  )
}

wk32_fit <- function(d, pos, config_row, features, offense_context = NULL, seed = 42L) {
  d <- wk32_add_features(d, offense_context)
  wk31_fit(d, pos, config_row, target_col = "weekly_fppg", features = features, seed = seed)
}

wk32_predict <- function(obj, d, offense_context = NULL) {
  d <- wk32_add_features(d, offense_context)
  wk31_predict(obj, d)
}

wk32_select_feature_set <- function(train, pos, config_row, offense_context = NULL, seed = 42L) {
  train <- train[as.character(train$position) == pos, , drop = FALSE]
  years <- sort(unique(suppressWarnings(as.integer(train$season))))
  years <- years[is.finite(years)]
  sets <- wk32_feature_sets(pos)

  if (length(years) < 2) {
    return(data.frame(
      position = pos, feature_set = names(sets),
      n_features = vapply(sets, length, integer(1)),
      inner_MAE = NA_real_, inner_RMSE = NA_real_,
      selected = names(sets) == "denoise_context",
      stringsAsFactors = FALSE
    ))
  }

  val_year <- max(years)
  inner_train <- train[suppressWarnings(as.integer(train$season)) < val_year, , drop = FALSE]
  inner_val <- train[suppressWarnings(as.integer(train$season)) == val_year, , drop = FALSE]
  if (nrow(inner_train) < 100 || nrow(inner_val) < 20) {
    return(data.frame(
      position = pos, feature_set = names(sets),
      n_features = vapply(sets, length, integer(1)),
      inner_MAE = NA_real_, inner_RMSE = NA_real_,
      selected = names(sets) == "denoise_context",
      stringsAsFactors = FALSE
    ))
  }

  rows <- list()
  for (i in seq_along(sets)) {
    nm <- names(sets)[i]
    features <- sets[[i]]
    fit <- tryCatch(
      wk32_fit(inner_train, pos, config_row, features, offense_context,
               seed = seed + i * 31L + val_year),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      rows[[i]] <- data.frame(position=pos, feature_set=nm, n_features=length(features),
                              inner_MAE=NA_real_, inner_RMSE=NA_real_, stringsAsFactors=FALSE)
      next
    }
    pred <- wk32_predict(fit, inner_val, offense_context)
    mm <- wk31_weighted_metrics(inner_val$weekly_fppg, pred, wk31_training_weight(inner_val, pos))
    rows[[i]] <- data.frame(
      position = pos, feature_set = nm, n_features = length(features),
      inner_MAE = mm[["MAE"]], inner_RMSE = mm[["RMSE"]],
      stringsAsFactors = FALSE
    )
  }
  out <- dplyr::bind_rows(rows)
  # Tiny complexity penalty breaks near-ties toward simpler models.
  out$selection_score <- out$inner_MAE + 0.10 * out$inner_RMSE + 0.0005 * sqrt(out$n_features)
  best <- if (any(is.finite(out$selection_score))) {
    which.min(replace(out$selection_score, !is.finite(out$selection_score), Inf))
  } else which(out$feature_set == "denoise_context")[1]
  out$selected <- seq_len(nrow(out)) == best
  out
}

wk32_select_alpha <- function(oof, pos) {
  if (!nrow(oof)) return(0.10)
  cap <- as.numeric(MODEL32_ALPHA_CAP[[pos]])
  grid <- MODEL32_ALPHA_GRID[MODEL32_ALPHA_GRID <= cap + 1e-9]
  score <- vapply(grid, function(a) {
    cand <- pmax(0, wk32_num(oof$production_base_32) +
                   a * (wk32_num(oof$direct_fppg_32) - wk32_num(oof$production_base_32)))
    err <- abs(wk32_num(oof$actual_fppg, NA_real_) - cand)
    starter <- if ("starter_cohort" %in% names(oof)) wk25_bool(oof$starter_cohort) else rep(FALSE, nrow(oof))
    relevant <- if ("relevant_cohort" %in% names(oof)) wk25_bool(oof$relevant_cohort) else rep(TRUE, nrow(oof))
    all_mae <- mean(err, na.rm = TRUE)
    st_mae <- if (any(starter)) mean(err[starter], na.rm = TRUE) else all_mae
    rel_mae <- if (any(relevant)) mean(err[relevant], na.rm = TRUE) else all_mae
    0.50 * st_mae + 0.30 * rel_mae + 0.20 * all_mae
  }, numeric(1))
  best <- min(score[is.finite(score)], na.rm = TRUE)
  near <- grid[is.finite(score) & score <= best + 0.002]
  if (length(near)) min(near) else 0
}
