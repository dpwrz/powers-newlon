# ============================================================
# FANTASY MODEL 3.2 - DENOISED ROLE + OFFENSIVE CONTEXT ENGINE
# ============================================================
# Challenger only. The 3.1/2.5 production forecast remains the incumbent.
# 3.2 deliberately reduces the feature surface and adds a compact set of
# strictly lagged offensive-context / opportunity composites.

if (!exists("schedule_team_rows21", mode = "function")) source("R/weekly_engine.R")
if (!exists("wk31_fit", mode = "function")) source("R/weekly_engine_31.R")

MODEL32_OFFENSE_METRICS <- c(
  "off_plays", "off_pass_rate", "off_neutral_pass_rate", "off_redzone_pass_rate",
  "off_pass_epa", "off_rush_epa", "off_pass_success", "off_rush_success",
  "off_sack_allowed_rate", "off_qb_hit_allowed_rate", "off_deep_pass_rate",
  "off_explosive_pass_rate", "off_redzone_pass_td_rate", "off_redzone_rush_td_rate",
  "off_no_huddle_rate", "off_shotgun_rate"
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
  z <- if (file.exists(path)) {
    tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
  } else data.frame()

  # Fast fallback: every defense-week row is also the opposing offense-week.
  # Re-map the existing defense PBP summary through the schedule, avoiding a
  # second multi-season PBP download just to recover offense efficiency/style.
  if ((!nrow(z) || !all(c("season","week","team") %in% names(z))) &&
      file.exists("data/raw/weekly_defense_context_raw.csv")) {
    def <- tryCatch(
      readr::read_csv("data/raw/weekly_defense_context_raw.csv", show_col_types = FALSE, progress = FALSE),
      error = function(e) data.frame()
    )
    sched <- data.frame()
    for (sp in c("data/raw/schedules_weekly_model.csv",
                 paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv"))) {
      if (!file.exists(sp)) next
      ss <- tryCatch(readr::read_csv(sp, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
      if (nrow(ss)) sched <- dplyr::bind_rows(sched, ss)
    }
    if (!nrow(sched) && requireNamespace("nflreadr", quietly = TRUE)) {
      sched <- tryCatch(nflreadr::load_schedules(seasons = WEEKLY_TRAIN_START:CURRENT_SEASON),
                        error = function(e) data.frame())
    }
    if (nrow(def) && nrow(sched) && exists("schedule_team_rows21", mode = "function")) {
      ts <- schedule_team_rows21(sched) |>
        dplyr::select(season, week, defense = team, team = opponent) |>
        dplyr::distinct(season, week, defense, .keep_all = TRUE)
      z <- def |>
        dplyr::left_join(ts, by = c("season","week","defense")) |>
        dplyr::filter(!is.na(team), team != "") |>
        dplyr::transmute(
          season = as.integer(wk32_num(season, NA_real_)),
          week = as.integer(wk32_num(week, NA_real_)),
          team = as.character(team),
          off_plays = wk32_num(def_dropbacks) + wk32_num(def_rushes),
          off_pass_rate = wk32_num(def_dropbacks) / pmax(wk32_num(def_dropbacks) + wk32_num(def_rushes), 1),
          off_neutral_pass_rate = off_pass_rate,
          off_redzone_pass_rate = 0,
          off_pass_epa = wk32_num(def_pass_epa_allowed),
          off_rush_epa = wk32_num(def_rush_epa_allowed),
          off_pass_success = wk32_num(def_pass_success_allowed),
          off_rush_success = wk32_num(def_rush_success_allowed),
          off_sack_allowed_rate = wk32_num(def_sack_rate),
          off_qb_hit_allowed_rate = wk32_num(def_qb_hit_rate),
          off_deep_pass_rate = wk32_num(def_deep_pass_rate),
          off_explosive_pass_rate = wk32_num(def_explosive_pass_rate),
          off_redzone_pass_td_rate = wk32_num(def_redzone_pass_td_rate),
          off_redzone_rush_td_rate = wk32_num(def_redzone_rush_td_rate),
          off_no_huddle_rate = 0,
          off_shotgun_rate = 0
        )
      if (nrow(z)) {
        tryCatch(readr::write_csv(z, path), error = function(e) NULL)
        cat("[3.2] Built offense context from existing defense PBP summaries: ", nrow(z), " rows.\n", sep = "")
      }
    }
  }

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
    "team_off_plays_roll4","team_off_pass_rate_roll4","team_off_pass_epa_roll4",
    "team_off_pass_success_roll4","team_off_sack_allowed_rate_roll4",
    "team_off_qb_hit_allowed_rate_roll4","team_off_deep_pass_rate_roll4",
    "team_off_explosive_pass_rate_roll4","team_off_redzone_pass_td_rate_roll4"
  ),
  RB = c(
    "team_off_plays_roll4","team_off_pass_rate_roll4","team_off_rush_epa_roll4",
    "team_off_rush_success_roll4","team_off_redzone_rush_td_rate_roll4"
  ),
  WR = c(
    "team_off_plays_roll4","team_off_pass_rate_roll4","team_off_pass_epa_roll4",
    "team_off_pass_success_roll4","team_off_deep_pass_rate_roll4",
    "team_off_explosive_pass_rate_roll4","team_off_redzone_pass_td_rate_roll4"
  ),
  TE = c(
    "team_off_plays_roll4","team_off_pass_rate_roll4","team_off_pass_epa_roll4",
    "team_off_pass_success_roll4","team_off_redzone_pass_td_rate_roll4"
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


wk32_promotion_gate <- function(d, pos, min_weeks = 4L, min_starters = 20L) {
  if (!nrow(d)) {
    return(data.frame(position=pos,promoted=FALSE,n=0L,weeks=0L,starter_n=0L,
      all_mae_gain_pct=NA_real_,starter_mae_gain_pct=NA_real_,week_win_rate=NA_real_,
      bootstrap_p_improves=NA_real_,max_week_mae_degradation_pct=NA_real_,
      failed_checks="no_oof_rows", stringsAsFactors=FALSE))
  }
  d <- d[as.character(d$position) == pos, , drop=FALSE]
  starter <- if ("starter_cohort" %in% names(d)) wk25_bool(d$starter_cohort) else rep(FALSE,nrow(d))
  ae_base <- abs(wk32_num(d$actual_fppg,NA_real_) - wk32_num(d$incumbent_fppg_32,NA_real_))
  ae_cand <- abs(wk32_num(d$actual_fppg,NA_real_) - wk32_num(d$candidate_fppg_32,NA_real_))
  pct_gain <- function(b,c) if(is.finite(b)&&b>0&&is.finite(c)) 100*(b-c)/b else NA_real_
  all_gain <- pct_gain(mean(ae_base,na.rm=TRUE),mean(ae_cand,na.rm=TRUE))
  st_gain <- if(any(starter)) pct_gain(mean(ae_base[starter],na.rm=TRUE),mean(ae_cand[starter],na.rm=TRUE)) else NA_real_

  weeks <- sort(unique(as.integer(d$week)))
  wg <- vapply(weeks,function(w){
    idx <- as.integer(d$week)==w
    b <- mean(ae_base[idx],na.rm=TRUE); c <- mean(ae_cand[idx],na.rm=TRUE)
    pct_gain(b,c)
  },numeric(1))
  win_rate <- if(length(wg)) mean(wg>0,na.rm=TRUE) else NA_real_
  max_deg <- if(any(is.finite(wg))) abs(min(c(0,wg),na.rm=TRUE)) else NA_real_

  dd <- d
  dd$production_base_31 <- dd$incumbent_fppg_32
  dd$candidate_fppg_31 <- dd$candidate_fppg_32
  boot <- wk31_bootstrap_gain(dd,"Starter",reps=1000L,
    seed=3200L+match(pos,c("QB","RB","WR","TE")))

  checks <- c(
    enough_weeks = length(weeks) >= min_weeks,
    enough_starters = sum(starter,na.rm=TRUE) >= min_starters,
    all_MAE = is.finite(all_gain) && all_gain >= 0.20,
    starter_MAE = is.finite(st_gain) && st_gain > 0,
    weekly_stability = is.finite(win_rate) && win_rate >= 0.60,
    paired_confidence = is.finite(boot$p_improves[1]) && boot$p_improves[1] >= 0.65,
    no_catastrophic_week = is.finite(max_deg) && max_deg <= 5
  )

  data.frame(
    position=pos,promoted=all(checks),n=nrow(d),weeks=length(weeks),
    starter_n=sum(starter,na.rm=TRUE),all_mae_gain_pct=all_gain,
    starter_mae_gain_pct=st_gain,week_win_rate=win_rate,
    bootstrap_p_improves=boot$p_improves[1],
    max_week_mae_degradation_pct=max_deg,
    failed_checks=paste(names(checks)[!checks],collapse=";"),
    stringsAsFactors=FALSE
  )
}
