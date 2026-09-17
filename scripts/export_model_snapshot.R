#!/usr/bin/env Rscript

# Fantasy Model 3.0 -> Web 1.0 production snapshot exporter
# Exports current-week + all 18 weekly forecasts, ROS, quality metrics,
# embedded production opportunity context, optional 3.0 role challenger,
# and movement versus the immediately previous website snapshot.

args <- if (interactive()) character() else commandArgs(trailingOnly = TRUE)
default_root <- Sys.getenv("FANTASY_MODEL_ROOT", "")
if (!nzchar(default_root)) {
  roots <- c(
    "C:/Users/dpowe/OneDrive/Documents/Powers-Newlon Model",
    "C:/Documents/Powers-Newlon Model",
    ".."
  )
  roots <- roots[dir.exists(roots)]
  default_root <- if (length(roots)) roots[[1]] else ".."
}
root <- if (length(args) >= 1 && nzchar(args[[1]])) normalizePath(args[[1]], mustWork = FALSE) else normalizePath(default_root, mustWork = FALSE)
out_path <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else Sys.getenv("FANTASY_WEB_SNAPSHOT", "public/data/model_snapshot.json")

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first: install.packages('jsonlite')")

read_optional <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
}
read_text_optional <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), error = function(e) NULL)
}
clean_chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; trimws(x) }
clean_num <- function(x) suppressWarnings(as.numeric(x))
first_col <- function(df, candidates, default = NA) {
  if (is.null(df) || !nrow(df)) return(rep(default, 0))
  hit <- candidates[candidates %in% base::names(df)]
  if (!length(hit)) return(rep(default, nrow(df)))
  df[[hit[[1]]]]
}
first_existing <- function(paths) { x <- paths[file.exists(paths)]; if (length(x)) x[[1]] else NULL }
key_for <- function(df) {
  ids <- clean_chr(first_col(df, c("gsis_id", "player_id", "nflverse_id"), ""))
  nm <- tolower(gsub("[^a-z0-9]", "", clean_chr(first_col(df, c("player_name", "player_display_name", "display_name", "name"), ""))))
  ifelse(nzchar(ids), ids, nm)
}
scalar <- function(x) if (length(x) && !is.na(x[[1]])) x[[1]] else NULL
`%||%` <- function(a,b) if (!is.null(a)) a else b

out <- file.path(root, "output")
weekly_file <- first_existing(c(file.path(out,"weekly_2026_projections.csv"), file.path(out,"final_2026_rankings.csv")))
season_file <- first_existing(c(file.path(out,"final_2026_rankings.csv"), file.path(out,"final_rankings.csv")))
dynasty_file <- first_existing(c(file.path(out,"dynasty_values_3_0.csv"), file.path(out,"final_dynasty_rankings.csv"), file.path(out,"dynasty_rankings.csv")))
ros_file <- first_existing(c(file.path(out,"rest_of_season_2026.csv")))
role_file <- first_existing(c(file.path(out,"weekly_role_forecasts_3_0.csv")))
identity_file <- first_existing(c(
  file.path(root,"data","processed","sleeper_player_identity_3_0.csv"),
  file.path(root,"data","app","sleeper_identity_cache.csv"),
  file.path(root,"data","processed","sleeper_identity_cache.csv"),
  file.path(out,"sleeper_identity_map.csv")
))
validation_file <- first_existing(c(file.path(out,"weekly_2_4_validation_metrics.csv")))
cohort_file <- first_existing(c(file.path(out,"weekly_2_4_cohort_metrics.csv")))
confidence_file <- first_existing(c(file.path(out,"weekly_2_4_confidence_calibration.csv")))
quality_report_file <- first_existing(c(file.path(out,"weekly_2_4_model_quality_report.txt"), file.path(out,"weekly_model_quality_report.txt")))
accuracy25_file <- first_existing(c(file.path(out,"weekly_2_5_validation_metrics.csv")))
manifest25_file <- first_existing(c(file.path(out,"weekly_2_5_champion_manifest.csv")))
live_weekly_file <- first_existing(c(file.path(out,paste0("live_accuracy_weekly_summary_", 2026, ".csv"))))
live_position_file <- first_existing(c(file.path(out,paste0("live_accuracy_weekly_position_", 2026, ".csv"))))
live_cumulative_file <- first_existing(c(file.path(out,paste0("live_accuracy_cumulative_position_", 2026, ".csv"))))
live_misses_file <- first_existing(c(file.path(out,paste0("live_accuracy_biggest_misses_", 2026, ".csv"))))
live_refresh_file <- first_existing(c(file.path(root,"data","state",paste0("live_refresh_state_", 2026, ".json"))))

weekly <- read_optional(weekly_file)
season <- read_optional(season_file)
dynasty <- read_optional(dynasty_file)
ros <- read_optional(ros_file)
role <- read_optional(role_file)
identity <- read_optional(identity_file)
validation <- read_optional(validation_file)
cohorts <- read_optional(cohort_file)
confidence_calibration <- read_optional(confidence_file)
quality_report <- read_text_optional(quality_report_file)
accuracy25 <- read_optional(accuracy25_file)
manifest25 <- read_optional(manifest25_file)
live_weekly <- read_optional(live_weekly_file)
live_position <- read_optional(live_position_file)
live_cumulative <- read_optional(live_cumulative_file)
live_misses <- read_optional(live_misses_file)
live_refresh <- if (!is.null(live_refresh_file) && file.exists(live_refresh_file)) tryCatch(jsonlite::fromJSON(live_refresh_file, simplifyVector = TRUE), error = function(e) NULL) else NULL

if (is.null(dynasty) && is.null(season) && is.null(weekly)) stop("No Fantasy Model production outputs found under: ", root)

# Read the prior website snapshot before overwrite so the new export can show
# how each future week / ROS changed after the latest model refresh.
prior_snapshot <- NULL
if (file.exists(out_path)) {
  prior_snapshot <- tryCatch(jsonlite::fromJSON(out_path, simplifyVector = FALSE), error = function(e) NULL)
}
prior_generated_at <- if (!is.null(prior_snapshot)) prior_snapshot$generated_at else NULL
prior_player_map <- new.env(hash = TRUE, parent = emptyenv())
if (!is.null(prior_snapshot$players)) {
  for (p in prior_snapshot$players) {
    k <- if (!is.null(p$gsis_id) && nzchar(as.character(p$gsis_id))) as.character(p$gsis_id) else tolower(gsub("[^a-z0-9]", "", as.character(p$player_name %||% "")))
    if (nzchar(k)) assign(k, p, envir = prior_player_map)
  }
}

# Current projected week = first unplayed week in the full weekly schedule.
projection_week <- NA_real_
weekly_current <- weekly
if (!is.null(weekly) && nrow(weekly)) {
  wk <- clean_num(first_col(weekly, c("season_week","week")))
  played <- clean_num(first_col(weekly, c("game_played","is_actual"), 0))
  candidate <- wk[is.na(played) | played == 0]
  candidate <- candidate[is.finite(candidate)]
  if (!length(candidate)) candidate <- wk[is.finite(wk)]
  if (length(candidate)) projection_week <- min(candidate)
  if (is.finite(projection_week)) weekly_current <- weekly[wk == projection_week, , drop = FALSE]
}

base <- if (!is.null(dynasty)) dynasty else if (!is.null(season)) season else weekly_current
players <- data.frame(
  key = key_for(base),
  gsis_id = clean_chr(first_col(base, c("gsis_id","player_id","nflverse_id"), "")),
  sleeper_id = clean_chr(first_col(base, c("sleeper_id","sleeper_player_id"), "")),
  player_name = clean_chr(first_col(base, c("player_name","player_display_name","display_name","name"), "")),
  position = clean_chr(first_col(base, c("position","pos"), "")),
  team = clean_chr(first_col(base, c("team","current_team","recent_team"), "")),
  stringsAsFactors = FALSE
)

merge_fields <- function(players, df, mappings) {
  if (is.null(df) || !nrow(df)) return(players)
  tmp <- data.frame(key = key_for(df), stringsAsFactors = FALSE)
  for (nm in base::names(mappings)) tmp[[nm]] <- mappings[[nm]](df)
  tmp <- tmp[nzchar(tmp$key) & !duplicated(tmp$key), , drop = FALSE]
  idx <- match(players$key, tmp$key)
  for (nm in base::names(mappings)) {
    incoming <- tmp[[nm]][idx]
    if (!nm %in% base::names(players)) { players[[nm]] <- incoming; next }
    current <- players[[nm]]
    if (is.character(current)) {
      replace <- (is.na(current) | !nzchar(current)) & !is.na(incoming) & nzchar(as.character(incoming))
    } else replace <- is.na(current) & !is.na(incoming)
    current[replace] <- incoming[replace]
    players[[nm]] <- current
  }
  players
}

players <- merge_fields(players, season, list(
  team = function(d) clean_chr(first_col(d,c("current_team","team","recent_team"),"")),
  age = function(d) clean_num(first_col(d,c("age","weekly_age","player_age"))),
  season_fppg = function(d) clean_num(first_col(d,c("projected_fppg","season_fppg","year1_fppg","fppg"))),
  floor = function(d) clean_num(first_col(d,c("projection_floor_fppg","season_floor_fppg"))),
  ceiling = function(d) clean_num(first_col(d,c("projection_ceiling_fppg","season_ceiling_fppg"))),
  season_rank = function(d) clean_num(first_col(d,c("overall_rank","rank"))),
  position_rank = function(d) clean_num(first_col(d,c("position_rank"))),
  elite_probability = function(d) clean_num(first_col(d,c("elite_probability"))),
  starter_probability = function(d) clean_num(first_col(d,c("starter_probability"))),
  breakout_probability = function(d) clean_num(first_col(d,c("breakout_probability"))),
  upside_index = function(d) clean_num(first_col(d,c("upside_index"))),
  uncertainty_sd = function(d) clean_num(first_col(d,c("total_uncertainty_sd","calibration_residual_sd"))),
  vorp_fppg = function(d) clean_num(first_col(d,c("vorp_fppg"))),
  replacement_fppg = function(d) clean_num(first_col(d,c("replacement_fppg"))),
  confidence = function(d) first_col(d,c("confidence")),
  projected_pass_attempts = function(d) clean_num(first_col(d,c("projected_pass_attempts_pg","projected_pass_attempts"))),
  projected_pass_yards = function(d) clean_num(first_col(d,c("projected_pass_yards_pg","projected_pass_yards"))),
  projected_pass_tds = function(d) clean_num(first_col(d,c("projected_pass_tds_pg","projected_pass_tds"))),
  projected_interceptions = function(d) clean_num(first_col(d,c("projected_interceptions_pg","projected_interceptions"))),
  projected_carries = function(d) clean_num(first_col(d,c("projected_carries_pg","projected_carries"))),
  projected_rush_yards = function(d) clean_num(first_col(d,c("projected_rush_yards_pg","projected_rush_yards"))),
  projected_rush_tds = function(d) clean_num(first_col(d,c("projected_rush_tds_pg","projected_rush_tds"))),
  projected_targets = function(d) clean_num(first_col(d,c("projected_targets_pg","projected_targets"))),
  projected_receptions = function(d) clean_num(first_col(d,c("projected_receptions_pg","projected_receptions"))),
  projected_rec_yards = function(d) clean_num(first_col(d,c("projected_rec_yards_pg","projected_rec_yards"))),
  projected_rec_tds = function(d) clean_num(first_col(d,c("projected_rec_tds_pg","projected_rec_tds"))),
  projected_catch_rate = function(d) clean_num(first_col(d,c("projected_catch_rate"))),
  projected_target_share = function(d) clean_num(first_col(d,c("projected_target_share"))),
  projected_carry_share = function(d) clean_num(first_col(d,c("projected_carry_share")))
))

players <- merge_fields(players, dynasty, list(
  dynasty_value = function(d) clean_num(first_col(d,c("model_dynasty_value","dynasty_value","value"))),
  dynasty_rank = function(d) clean_num(first_col(d,c("model_dynasty_rank","overall_rank","dynasty_rank","rank"))),
  season_fppg = function(d) clean_num(first_col(d,c("year1_fppg","season_fppg","projected_fppg"))),
  year3_fppg = function(d) clean_num(first_col(d,c("year3_fppg"))),
  elite_probability = function(d) clean_num(first_col(d,c("elite_probability"))),
  starter_probability = function(d) clean_num(first_col(d,c("starter_probability"))),
  breakout_probability = function(d) clean_num(first_col(d,c("breakout_probability")))
))

players <- merge_fields(players, weekly_current, list(
  week_projection = function(d) clean_num(first_col(d,c("projected_weekly_fppg","candidate_fppg_25","projected_weekly_fppg_24","projected_weekly_fppg_232","candidate23_fppg"))),
  floor = function(d) clean_num(first_col(d,c("weekly_floor","floor","projection_floor"))),
  ceiling = function(d) clean_num(first_col(d,c("weekly_ceiling","ceiling","projection_ceiling"))),
  expected_error = function(d) clean_num(first_col(d,c("expected_abs_error","expected_error"))),
  opponent = function(d) clean_chr(first_col(d,c("opponent"),"")),
  matchup_grade = function(d) clean_chr(first_col(d,c("matchup_grade"),"")),
  injury_status = function(d) clean_chr(first_col(d,c("injury_status"),"")),
  practice_status = function(d) clean_chr(first_col(d,c("practice_status"),"")),
  confidence = function(d) first_col(d,c("projection_confidence","confidence")),
  projected_targets = function(d) clean_num(first_col(d,c("projected_targets","projected_targets_pg"))),
  projected_carries = function(d) clean_num(first_col(d,c("projected_carries","projected_carries_pg"))),
  projected_pass_attempts = function(d) clean_num(first_col(d,c("projected_pass_attempts","projected_pass_attempts_pg"))),
  projected_receptions = function(d) clean_num(first_col(d,c("projected_receptions","projected_receptions_pg"))),
  projected_pass_yards = function(d) clean_num(first_col(d,c("projected_pass_yards","projected_pass_yards_pg"))),
  projected_pass_tds = function(d) clean_num(first_col(d,c("projected_pass_tds","projected_pass_tds_pg"))),
  projected_interceptions = function(d) clean_num(first_col(d,c("projected_interceptions","projected_interceptions_pg"))),
  projected_rush_yards = function(d) clean_num(first_col(d,c("projected_rush_yards","projected_rush_yards_pg"))),
  projected_rush_tds = function(d) clean_num(first_col(d,c("projected_rush_tds","projected_rush_tds_pg"))),
  projected_rec_yards = function(d) clean_num(first_col(d,c("projected_rec_yards","projected_rec_yards_pg"))),
  projected_rec_tds = function(d) clean_num(first_col(d,c("projected_rec_tds","projected_rec_tds_pg"))),
  projected_target_share = function(d) clean_num(first_col(d,c("projected_target_share"))),
  projected_carry_share = function(d) clean_num(first_col(d,c("projected_carry_share"))),
  boom_probability = function(d) clean_num(first_col(d,c("boom_probability"))),
  bust_probability = function(d) clean_num(first_col(d,c("bust_probability")))
))

players <- merge_fields(players, ros, list(
  ros_remaining_points = function(d) clean_num(first_col(d,c("projected_remaining_points"))),
  ros_full_season_points = function(d) clean_num(first_col(d,c("projected_full_season_points"))),
  ros_rank = function(d) clean_num(first_col(d,c("ros_position_rank","ros_rank")))
))
players <- merge_fields(players, role, list(
  role_score = function(d) clean_num(first_col(d,c("role_score","projected_role_score","opportunity_score"))),
  regime_probability = function(d) clean_num(first_col(d,c("regime_probability","role_regime_probability","role_change_probability")))
))

if (!is.null(identity) && nrow(identity)) {
  tmp <- data.frame(key = key_for(identity), sleeper_id = clean_chr(first_col(identity,c("sleeper_id","sleeper_player_id","player_id_sleeper"),"")), stringsAsFactors = FALSE)
  tmp <- tmp[nzchar(tmp$key) & !duplicated(tmp$key), , drop = FALSE]
  idx <- match(players$key, tmp$key)
  incoming <- tmp$sleeper_id[idx]
  fill <- (!nzchar(players$sleeper_id) | is.na(players$sleeper_id)) & !is.na(incoming) & nzchar(incoming)
  players$sleeper_id[fill] <- incoming[fill]
}

# Compact per-player all-week schedule. This is the key Web 0.5 change:
# the browser can inspect W1-W18 and see movement from the previous export.
weekly_by_key <- list()
if (!is.null(weekly) && nrow(weekly)) {
  weekly$.web_key <- key_for(weekly)
  all_keys <- unique(weekly$.web_key[nzchar(weekly$.web_key)])
  for (k in all_keys) {
    d <- weekly[weekly$.web_key == k, , drop = FALSE]
    wk <- clean_num(first_col(d,c("week","season_week")))
    ord <- order(wk)
    d <- d[ord, , drop = FALSE]; wk <- wk[ord]
    prior_p <- if (exists(k, envir = prior_player_map, inherits = FALSE)) get(k, envir = prior_player_map) else NULL
    prior_weeks <- list()
    if (!is.null(prior_p$weekly_projections)) {
      for (pw in prior_p$weekly_projections) prior_weeks[[as.character(pw$week)]] <- pw$projection
    }
    rows <- vector("list", nrow(d))
    for (j in seq_len(nrow(d))) {
      proj <- clean_num(first_col(d[j,,drop=FALSE],c("projected_weekly_fppg","candidate_fppg_25","projected_weekly_fppg_24","projected_weekly_fppg_232","candidate23_fppg")))[1]
      prev <- prior_weeks[[as.character(wk[j])]]
      prev <- if (length(prev)) suppressWarnings(as.numeric(prev)) else NA_real_
      rows[[j]] <- list(
        week = wk[j],
        opponent = scalar(clean_chr(first_col(d[j,,drop=FALSE],c("opponent"),""))),
        projection = if (is.finite(proj)) proj else NULL,
        floor = scalar(clean_num(first_col(d[j,,drop=FALSE],c("weekly_floor","floor","projection_floor")))),
        ceiling = scalar(clean_num(first_col(d[j,,drop=FALSE],c("weekly_ceiling","ceiling","projection_ceiling")))),
        expected_error = scalar(clean_num(first_col(d[j,,drop=FALSE],c("expected_abs_error","expected_error")))),
        confidence = scalar(first_col(d[j,,drop=FALSE],c("projection_confidence","confidence"))),
        projected_targets = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_targets","projected_targets_pg")))),
        projected_carries = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_carries","projected_carries_pg")))),
        projected_pass_attempts = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_pass_attempts","projected_pass_attempts_pg")))),
        projected_receptions = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_receptions","projected_receptions_pg")))),
        projected_pass_yards = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_pass_yards","projected_pass_yards_pg")))),
        projected_pass_tds = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_pass_tds","projected_pass_tds_pg")))),
        projected_interceptions = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_interceptions","projected_interceptions_pg")))),
        projected_rush_yards = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_rush_yards","projected_rush_yards_pg")))),
        projected_rush_tds = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_rush_tds","projected_rush_tds_pg")))),
        projected_rec_yards = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_rec_yards","projected_rec_yards_pg")))),
        projected_rec_tds = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_rec_tds","projected_rec_tds_pg")))),
        projected_fumbles_lost = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_fumbles_lost","projected_fumbles_lost_pg")))),
        projected_target_share = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_target_share")))),
        projected_carry_share = scalar(clean_num(first_col(d[j,,drop=FALSE],c("projected_carry_share")))),
        injury_status = scalar(clean_chr(first_col(d[j,,drop=FALSE],c("injury_status"),""))),
        practice_status = scalar(clean_chr(first_col(d[j,,drop=FALSE],c("practice_status"),""))),
        matchup_grade = scalar(clean_chr(first_col(d[j,,drop=FALSE],c("matchup_grade"),""))),
        game_played = scalar(clean_num(first_col(d[j,,drop=FALSE],c("game_played","is_actual"),0))),
        actual = scalar(clean_num(first_col(d[j,,drop=FALSE],c("actual_weekly_fppg")))),
        availability_factor = scalar(clean_num(first_col(d[j,,drop=FALSE],c("availability_factor")))),
        implied_team_total = scalar(clean_num(first_col(d[j,,drop=FALSE],c("implied_team_total")))),
        topN_probability = scalar(clean_num(first_col(d[j,,drop=FALSE],c("topN_probability")))),
        boom_probability = scalar(clean_num(first_col(d[j,,drop=FALSE],c("boom_probability")))),
        bust_probability = scalar(clean_num(first_col(d[j,,drop=FALSE],c("bust_probability")))),
        role_trend = scalar(clean_num(first_col(d[j,,drop=FALSE],c("role_fppg_trend","role_trend")))),
        matchup_delta = scalar(clean_num(first_col(d[j,,drop=FALSE],c("calibrated_matchup_delta_23","matchup_delta_22","matchup_delta")))),
        previous_projection = if (is.finite(prev)) prev else NULL,
        delta_projection = if (is.finite(prev) && is.finite(proj)) proj - prev else NULL
      )
    }
    missing_weeks <- setdiff(1:18, as.integer(wk[is.finite(wk)]))
    if (length(missing_weeks)) {
      for (mw in missing_weeks) {
        prev <- prior_weeks[[as.character(mw)]]
        prev <- if (length(prev)) suppressWarnings(as.numeric(prev)) else NA_real_
        rows[[length(rows)+1]] <- list(
          week = mw, opponent = "BYE", projection = 0, floor = 0, ceiling = 0,
          expected_error = 0, confidence = "BYE", projected_targets = 0,
          projected_carries = 0, projected_pass_attempts = 0, projected_receptions = 0,
          projected_pass_yards = 0, projected_pass_tds = 0, projected_interceptions = 0,
          projected_rush_yards = 0, projected_rush_tds = 0, projected_rec_yards = 0, projected_rec_tds = 0, projected_fumbles_lost = 0,
          projected_target_share = 0, projected_carry_share = 0,
          injury_status = NULL, practice_status = NULL, matchup_grade = "BYE",
          game_played = 0, actual = NULL, availability_factor = 0,
          implied_team_total = NULL, topN_probability = 0, boom_probability = 0, bust_probability = 0,
          role_trend = 0, matchup_delta = 0,
          previous_projection = if (is.finite(prev)) prev else NULL,
          delta_projection = if (is.finite(prev)) 0 - prev else NULL
        )
      }
    }
    rows <- rows[order(vapply(rows, function(x) as.integer(x$week), integer(1)))]
    weekly_by_key[[k]] <- rows
  }
}

players <- players[nzchar(players$player_name) & nzchar(players$position), , drop = FALSE]
players$week <- if (is.finite(projection_week)) projection_week else NA_real_
players$weekly_projections <- I(lapply(players$key, function(k) weekly_by_key[[k]] %||% list()))
players$previous_ros_remaining_points <- NA_real_
players$ros_delta <- NA_real_
for (i in seq_len(nrow(players))) {
  k <- players$key[i]
  if (!exists(k, envir = prior_player_map, inherits = FALSE)) next
  pp <- get(k, envir = prior_player_map)
  prev_ros <- suppressWarnings(as.numeric(pp$ros_remaining_points %||% NA_real_))
  if (is.finite(prev_ros)) {
    players$previous_ros_remaining_points[i] <- prev_ros
    if (is.finite(players$ros_remaining_points[i])) players$ros_delta[i] <- players$ros_remaining_points[i] - prev_ros
  }
}
players$key <- NULL
for (nm in names(players)) if (is.character(players[[nm]])) players[[nm]][players[[nm]] == ""] <- NA

quality <- list(
  validation = if (!is.null(validation)) validation else list(),
  cohorts = if (!is.null(cohorts)) cohorts else list(),
  confidence_calibration = if (!is.null(confidence_calibration)) confidence_calibration else list(),
  report = quality_report,
  accuracy25 = if (!is.null(accuracy25)) accuracy25 else list(),
  live_weekly = if (!is.null(live_weekly)) live_weekly else list(),
  live_position = if (!is.null(live_position)) live_position else list(),
  live_cumulative = if (!is.null(live_cumulative)) live_cumulative else list(),
  live_misses = if (!is.null(live_misses)) live_misses else list()
)

# Surface the actual 2.5 position champion when the accuracy tournament exists.
position_champions <- list()
if (!is.null(manifest25) && nrow(manifest25)) {
  for (i in seq_len(nrow(manifest25))) {
    pos <- as.character(manifest25$position[i])
    promoted <- tolower(as.character(manifest25$promoted_for_2026[i])) %in% c("true","t","1","yes")
    alpha <- suppressWarnings(as.numeric(manifest25$final_alpha_2026[i]))
    position_champions[[length(position_champions)+1]] <- list(
      position = pos,
      production = if (promoted) paste0("2.5 guarded blend (", round(100*alpha), "% XGBoost challenger)") else as.character(manifest25$incumbent[i]),
      challenger = as.character(manifest25$challenger[i]),
      promoted = promoted,
      overall_mae = suppressWarnings(as.numeric(if (promoted) manifest25$challenger_MAE[i] else manifest25$incumbent_MAE[i])),
      starter_mae = suppressWarnings(as.numeric(if (promoted) manifest25$challenger_starter_MAE[i] else manifest25$incumbent_starter_MAE[i]))
    )
  }
} else if (!is.null(validation) && nrow(validation)) {
  for (i in seq_len(nrow(validation))) {
    pos <- as.character(validation$position[i])
    promoted <- tolower(as.character(validation$promoted_for_2026[i])) %in% c("true","t","1")
    position_champions[[length(position_champions)+1]] <- list(
      position = pos, production = if (promoted) "2.4.3 signal-attribution" else "2.4.3 guardrailed incumbent",
      challenger = if (promoted) "2.4.3 promoted" else "2.4.3 challenger rejected", promoted = promoted,
      overall_mae = suppressWarnings(as.numeric(validation$model24_MAE[i])), starter_mae = suppressWarnings(as.numeric(validation$model24_starter_MAE[i]))
    )
  }
}

snapshot <- list(
  release_version = "3.0.0",
  web_version = "1.0.0",
  generated_at = format(Sys.time(), tz="UTC", usetz=TRUE),
  previous_generated_at = prior_generated_at,
  projection_engine = if (!is.null(manifest25) && nrow(manifest25)) "3.0 production: position-specific guarded blend" else "2.4.3 guardrailed weekly stack",
  dynasty_engine = "1.1 model-first dynasty",
  role_engine = if (!is.null(manifest25) && nrow(manifest25)) "3.0 production stack + embedded opportunity context" else "2.4.3 embedded opportunity / role context",
  role_challenger = if (!is.null(role) && nrow(role)) "3.0 role/regime challenger available in shadow mode" else "3.0 role/regime challenger not exported; not used in production",
  projection_week = if (is.finite(projection_week)) projection_week else NULL,
  player_count = nrow(players),
  position_champions = position_champions,
  quality = quality,
  live_refresh = live_refresh,
  players = players
)

dir.create(dirname(out_path), recursive=TRUE, showWarnings=FALSE)
jsonlite::write_json(snapshot, out_path, pretty=FALSE, auto_unbox=TRUE, na="null", digits=NA)
cat(sprintf("[WEB 1.0] Wrote %s with %d players; all weekly projections included.\n", out_path, nrow(players)))
cat(sprintf("[WEB 1.0] Current projection week: %s; previous snapshot: %s.\n", ifelse(is.finite(projection_week), projection_week, "NA"), prior_generated_at %||% "none"))
cat(sprintf("[WEB 1.0] Role challenger rows: %d. Production opportunity context comes from weekly 2.4.3 regardless.\n", ifelse(is.null(role),0,nrow(role))))
