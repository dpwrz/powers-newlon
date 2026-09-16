# ============================================================
# FANTASY MODEL 2.3 - PROJECT EVERY 2026 WEEK + REST OF SEASON
# ============================================================
source("config.R")
ensure_packages(c("dplyr", "readr", "nflreadr", "rpart"))
source("R/weekly_engine.R")
source("R/weekly_engine_22.R")
source("R/weekly_engine_23.R")

season_projection_path <- paste0("output/", CURRENT_SEASON, "_projections.csv")
weekly_train_path <- if (file.exists("data/processed/weekly_model_table_2_3.csv")) "data/processed/weekly_model_table_2_3.csv" else "data/processed/weekly_model_table_2_2.csv"
legacy_weights_path <- "output/weekly_2_3_selected_legacy_weights.csv"
meta_weights_path <- "output/weekly_2_3_selected_meta_weights.csv"
version_weights_path <- "output/weekly_2_3_selected_version_weights.csv"
matchup_cal_path <- "output/weekly_2_3_matchup_calibration_parameters.csv"
stack22_path <- "output/weekly_2_3_selected_22_stack.csv"
metrics_path <- "output/weekly_2_3_validation_metrics.csv"
residual_path <- "output/weekly_2_3_residual_pool.csv"
confidence_path <- "output/weekly_2_3_confidence_calibration.csv"
schedule_path <- "data/raw/schedules_weekly_model.csv"
required23 <- c(season_projection_path, weekly_train_path, legacy_weights_path, meta_weights_path, version_weights_path, matchup_cal_path, stack22_path, metrics_path, residual_path, confidence_path)
for (pp in required23) if (!file.exists(pp)) stop("Missing 2.3 prerequisite: ", pp)

season_proj <- readr::read_csv(season_projection_path, show_col_types = FALSE, progress = FALSE)
hist_weekly <- readr::read_csv(weekly_train_path, show_col_types = FALSE, progress = FALSE)
legacy_weights <- readr::read_csv(legacy_weights_path, show_col_types = FALSE, progress = FALSE)
meta_weights <- readr::read_csv(meta_weights_path, show_col_types = FALSE, progress = FALSE)
version_weights <- readr::read_csv(version_weights_path, show_col_types = FALSE, progress = FALSE)
matchup_calibration <- readr::read_csv(matchup_cal_path, show_col_types = FALSE, progress = FALSE)
weights22 <- readr::read_csv(stack22_path, show_col_types = FALSE, progress = FALSE)
residual_pool <- readr::read_csv(residual_path, show_col_types = FALSE, progress = FALSE)
confidence_calibration <- readr::read_csv(confidence_path, show_col_types = FALSE, progress = FALSE)
metrics <- readr::read_csv(metrics_path, show_col_types = FALSE, progress = FALSE)

# Schedule resolution is cache-first because the auto-refresh runner has already
# attempted the live nflverse poll. This avoids a second 60-second network wait
# and keeps projections available when GitHub/nflverse is temporarily unavailable.
live_schedule_path <- paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv")
read_schedule_cache21 <- function(path) {
  if (!file.exists(path)) return(data.frame())
  z <- tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
  if (!nrow(z) || !"season" %in% names(z)) return(data.frame())
  z |> dplyr::filter(suppressWarnings(as.integer(season)) == CURRENT_SEASON)
}

schedules <- read_schedule_cache21(live_schedule_path)
if (!nrow(schedules)) schedules <- read_schedule_cache21(schedule_path)
if (!nrow(schedules)) {
  schedules <- tryCatch(
    nflreadr::load_schedules(seasons = CURRENT_SEASON),
    error = function(e) {
      warning("Could not load the current schedule from nflverse and no usable local schedule cache was found: ", conditionMessage(e))
      data.frame()
    }
  )
}
if (!nrow(schedules)) {
  stop(
    "No schedule rows are available for ", CURRENT_SEASON, ". Expected either ",
    live_schedule_path, " or ", schedule_path, " to contain the current season."
  )
}
team_schedule <- schedule_team_rows21(schedules)
if (!nrow(team_schedule) || !"season" %in% names(team_schedule)) {
  stop("Could not construct regular-season team schedule rows for ", CURRENT_SEASON, ".")
}
team_schedule <- team_schedule |> dplyr::filter(season == CURRENT_SEASON)
if (nrow(team_schedule) == 0) stop("No regular-season schedule rows for ", CURRENT_SEASON)

# Current-season actual stats may not exist before Week 1. The auto-refresh
# runner writes a current cache before entering this pipeline, so use that cache
# first and avoid a redundant remote request that can time out mid-refresh.
current_stats_path <- paste0("data/raw/player_weekly_stats_", CURRENT_SEASON, ".csv")
current_raw <- data.frame()
if (file.exists(current_stats_path)) {
  current_raw <- tryCatch(
    readr::read_csv(current_stats_path, show_col_types = FALSE, progress = FALSE),
    error = function(e) data.frame()
  )
  if (nrow(current_raw) && "season" %in% names(current_raw)) {
    current_raw <- current_raw[suppressWarnings(as.integer(current_raw$season)) == CURRENT_SEASON, , drop = FALSE]
  }
}
if (!nrow(current_raw)) {
  current_raw <- tryCatch(
    nflreadr::load_player_stats(seasons = CURRENT_SEASON, summary_level = "week"),
    error = function(e) data.frame()
  )
}
current_weekly <- normalize_weekly_stats21(current_raw)
if (nrow(current_weekly) > 0) current_weekly$passing_interceptions_22 <- wk_num(first_existing_col21(current_weekly, c("passing_interceptions", "interceptions"), 0))
if (nrow(current_weekly) > 0) readr::write_csv(current_raw, current_stats_path)
rm(current_raw); invisible(gc(full = TRUE))

# Current-season snap share: live playing-time signal. Historical/current PFR snap
# data updates during the season; if unavailable, the model falls back to its prior.
cur_snap <- data.frame()
if (nrow(current_weekly) > 0) {
  cur_snap_raw <- tryCatch(nflreadr::load_snap_counts(seasons = CURRENT_SEASON), error = function(e) data.frame())
  if (nrow(cur_snap_raw) > 0 && "pfr_player_id" %in% names(cur_snap_raw)) {
    player_ids <- tryCatch(nflreadr::load_players(), error = function(e) data.frame())
    if (nrow(player_ids) > 0 && all(c("gsis_id", "pfr_id") %in% names(player_ids))) {
      id_map <- player_ids |>
        dplyr::transmute(player_id = wk_chr(gsis_id), pfr_player_id = wk_chr(pfr_id)) |>
        dplyr::filter(!is.na(pfr_player_id), pfr_player_id != "") |>
        dplyr::distinct(pfr_player_id, .keep_all = TRUE)
      cur_snap <- cur_snap_raw |>
        dplyr::mutate(pfr_player_id = wk_chr(pfr_player_id), season = as.integer(wk_num(season)), week = as.integer(wk_num(week)),
                      offense_snaps = wk_num(offense_snaps), offense_pct = wk_num(offense_pct)) |>
        dplyr::left_join(id_map, by = "pfr_player_id") |>
        dplyr::filter(!is.na(player_id), week >= 1, week <= 18) |>
        dplyr::group_by(season, week, player_id) |>
        dplyr::summarise(offense_snaps = sum(offense_snaps, na.rm = TRUE), offense_pct = max(offense_pct, na.rm = TRUE), .groups = "drop")
      cur_snap$offense_pct[!is.finite(cur_snap$offense_pct)] <- NA_real_
      cur_snap$offense_pct <- ifelse(cur_snap$offense_pct > 1.5, cur_snap$offense_pct / 100, cur_snap$offense_pct)
    }
  }
  if (nrow(cur_snap) > 0) current_weekly <- current_weekly |> dplyr::left_join(cur_snap, by = c("season", "week", "player_id"))
  if (!"offense_snaps" %in% names(current_weekly)) current_weekly$offense_snaps <- NA_real_
  if (!"offense_pct" %in% names(current_weekly)) current_weekly$offense_pct <- NA_real_
}

# Current-season NGS is lagged/current-state only. It is never joined to an
# unplayed week as an outcome; completed-game NGS is summarized into the state.
cur_ngs <- data.frame()
if (isTRUE(WEEKLY_22_USE_NGS) && nrow(current_weekly) > 0) {
  parts <- list()
  for (typ in c("passing", "receiving", "rushing")) {
    rr <- tryCatch(nflreadr::load_nextgen_stats(seasons = CURRENT_SEASON, stat_type = typ), error = function(e) data.frame())
    nn <- normalize_ngs22(rr, typ)
    if (nrow(nn) > 0) parts[[typ]] <- nn
  }
  if (length(parts) > 0) {
    cur_ngs <- Reduce(function(x, y) dplyr::full_join(x, y, by = c("season", "week", "player_id")), parts)
    current_weekly <- current_weekly |> dplyr::left_join(cur_ngs, by = c("season", "week", "player_id"))
  }
}

# Current injury reports. The nflverse injury source currently ends after 2024,
# so this is an optional hook; unavailable current data never aborts projections.
# unavailable future weeks remain healthy/unknown rather than being fabricated.
cur_inj_raw <- tryCatch(nflreadr::load_injuries(seasons = CURRENT_SEASON), error = function(e) data.frame())
cur_inj <- prepare_injuries21(cur_inj_raw)

# Canonical projected players from the season model.
for (nm in c("player_id", "player_display_name", "position", "current_team", "projected_fppg")) season_proj <- ensure_weekly_col21(season_proj, nm, NA)
for (nm in c("rec_prior_deep_target_rate", "rec_prior_middle_target_rate", "rec_prior_redzone_target_rate",
             "prior_catch_rate", "prior_yards_per_target", "prior_yards_per_carry", "prior_yards_per_attempt",
             "prior_pass_td_rate", "prior_rush_td_rate", "prior_rec_td_rate", "prior_interception_rate")) season_proj <- ensure_weekly_col21(season_proj, nm, NA_real_)
players <- season_proj |>
  dplyr::transmute(
    player_id = wk_chr(player_id), player_display_name = wk_chr(player_display_name), position = toupper(wk_chr(position)),
    team = wk_chr(current_team), preseason_prior_fppg = pmax(0, wk_num(projected_fppg)),
    preseason_deep_target_rate = wk_num(rec_prior_deep_target_rate),
    preseason_middle_target_rate = wk_num(rec_prior_middle_target_rate),
    preseason_redzone_target_rate = wk_num(rec_prior_redzone_target_rate),
    prior_catch_rate = dplyr::coalesce(wk_num(prior_catch_rate), .65),
    prior_yards_per_target = dplyr::coalesce(wk_num(prior_yards_per_target), 7.0),
    prior_yards_per_carry = dplyr::coalesce(wk_num(prior_yards_per_carry), 4.3),
    prior_yards_per_attempt = dplyr::coalesce(wk_num(prior_yards_per_attempt), 7.0),
    prior_pass_td_rate = dplyr::coalesce(wk_num(prior_pass_td_rate), .045),
    prior_rush_td_rate = dplyr::coalesce(wk_num(prior_rush_td_rate), .035),
    prior_rec_td_rate = dplyr::coalesce(wk_num(prior_rec_td_rate), .045),
    prior_interception_rate = dplyr::coalesce(wk_num(prior_interception_rate), .025)
  ) |>
  dplyr::filter(position %in% POSITIONS, !is.na(team), team != "") |>
  dplyr::distinct(player_id, .keep_all = TRUE)

# This is an intentional player x team-schedule expansion: many players share a
# team and each team has many future weeks. Declare the relationship explicitly
# so dplyr does not emit a false-positive many-to-many warning.
future_grid <- players |>
  dplyr::inner_join(team_schedule, by = "team", relationship = "many-to-many") |>
  dplyr::filter(season == CURRENT_SEASON) |>
  dplyr::arrange(player_id, week)
if (nrow(future_grid) == 0) stop("Could not map 2026 projected players to the schedule.")

# ------------------------------------------------------------
# Build the latest current-season player/team state from completed games only.
# ------------------------------------------------------------
if (nrow(current_weekly) > 0) {
  tw <- current_weekly |>
    dplyr::group_by(season, week, team) |>
    dplyr::summarise(team_targets = sum(targets, na.rm = TRUE), team_carries = sum(carries, na.rm = TRUE), team_pass_attempts = sum(pass_attempts, na.rm = TRUE), .groups = "drop")
  current_weekly <- current_weekly |>
    dplyr::left_join(tw, by = c("season", "week", "team")) |>
    dplyr::mutate(target_share_week = wk_rate(targets, team_targets), carry_share_week = wk_rate(carries, team_carries), pass_attempt_share_week = wk_rate(pass_attempts, team_pass_attempts))

  player_state <- current_weekly |>
    dplyr::group_by(player_id, position) |>
    dplyr::arrange(week, .by_group = TRUE) |>
    dplyr::summarise(
      games_played_prior = dplyr::n(), prior_game_fppg = dplyr::last(weekly_fppg),
      roll3_fppg = mean(utils::tail(weekly_fppg, 3), na.rm = TRUE), roll5_fppg = mean(utils::tail(weekly_fppg, 5), na.rm = TRUE),
      roll3_fppg_sd = ifelse(length(utils::tail(weekly_fppg, 3)) >= 2, stats::sd(utils::tail(weekly_fppg, 3), na.rm = TRUE), 0),
      season_to_date_fppg = mean(weekly_fppg, na.rm = TRUE),
      roll3_targets = mean(utils::tail(targets, 3), na.rm = TRUE), roll5_targets = mean(utils::tail(targets, 5), na.rm = TRUE),
      roll3_carries = mean(utils::tail(carries, 3), na.rm = TRUE), roll5_carries = mean(utils::tail(carries, 5), na.rm = TRUE),
      roll3_pass_attempts = mean(utils::tail(pass_attempts, 3), na.rm = TRUE), roll5_pass_attempts = mean(utils::tail(pass_attempts, 5), na.rm = TRUE),
      roll3_target_share = mean(utils::tail(target_share_week, 3), na.rm = TRUE),
      roll3_carry_share = mean(utils::tail(carry_share_week, 3), na.rm = TRUE),
      roll3_pass_attempt_share = mean(utils::tail(pass_attempt_share_week, 3), na.rm = TRUE),
      roll3_offense_pct = mean(utils::tail(offense_pct, 3), na.rm = TRUE),
      roll5_offense_pct = mean(utils::tail(offense_pct, 5), na.rm = TRUE),
      roll3_offense_snaps = mean(utils::tail(offense_snaps, 3), na.rm = TRUE),
      season_targets_prior = sum(targets, na.rm = TRUE),
      season_carries_prior = sum(carries, na.rm = TRUE),
      season_pass_attempts_prior = sum(pass_attempts, na.rm = TRUE),
      season_to_date_catch_rate = wk_rate(sum(receptions, na.rm = TRUE), sum(targets, na.rm = TRUE), NA_real_),
      season_to_date_ypt = wk_rate(sum(receiving_yards, na.rm = TRUE), sum(targets, na.rm = TRUE), NA_real_),
      season_to_date_rush_ypc = wk_rate(sum(rushing_yards, na.rm = TRUE), sum(carries, na.rm = TRUE), NA_real_),
      season_to_date_pass_ypa = wk_rate(sum(passing_yards, na.rm = TRUE), sum(pass_attempts, na.rm = TRUE), NA_real_),
      season_to_date_pass_td_rate = wk_rate(sum(passing_tds, na.rm = TRUE), sum(pass_attempts, na.rm = TRUE), NA_real_),
      season_to_date_interception_rate = wk_rate(sum(passing_interceptions_22, na.rm = TRUE), sum(pass_attempts, na.rm = TRUE), NA_real_),
      season_to_date_rush_td_rate = wk_rate(sum(rushing_tds, na.rm = TRUE), sum(carries, na.rm = TRUE), NA_real_),
      season_to_date_rec_td_rate = wk_rate(sum(receiving_tds, na.rm = TRUE), sum(targets, na.rm = TRUE), NA_real_),
      roll3_catch_rate = wk_rate(sum(utils::tail(receptions, 3), na.rm = TRUE), sum(utils::tail(targets, 3), na.rm = TRUE), NA_real_),
      roll3_ypt = wk_rate(sum(utils::tail(receiving_yards, 3), na.rm = TRUE), sum(utils::tail(targets, 3), na.rm = TRUE), NA_real_),
      roll3_rush_ypc = wk_rate(sum(utils::tail(rushing_yards, 3), na.rm = TRUE), sum(utils::tail(carries, 3), na.rm = TRUE), NA_real_),
      roll3_pass_ypa = wk_rate(sum(utils::tail(passing_yards, 3), na.rm = TRUE), sum(utils::tail(pass_attempts, 3), na.rm = TRUE), NA_real_),
      roll3_pass_td_rate = wk_rate(sum(utils::tail(passing_tds, 3), na.rm = TRUE), sum(utils::tail(pass_attempts, 3), na.rm = TRUE), NA_real_),
      roll3_interception_rate = wk_rate(sum(utils::tail(passing_interceptions_22, 3), na.rm = TRUE), sum(utils::tail(pass_attempts, 3), na.rm = TRUE), NA_real_),
      roll3_rush_td_rate = wk_rate(sum(utils::tail(rushing_tds, 3), na.rm = TRUE), sum(utils::tail(carries, 3), na.rm = TRUE), NA_real_),
      roll3_rec_td_rate = wk_rate(sum(utils::tail(receiving_tds, 3), na.rm = TRUE), sum(utils::tail(targets, 3), na.rm = TRUE), NA_real_),
      dplyr::across(dplyr::any_of(c("ngs_cpoe", "ngs_air_yards", "ngs_time_to_throw", "ngs_aggressiveness", "ngs_separation", "ngs_cushion", "ngs_yac_oe", "ngs_air_yard_share", "ngs_rush_yoe_pa", "ngs_box_rate", "ngs_time_to_los")), ~mean(utils::tail(.x[is.finite(.x)], 3), na.rm = TRUE), .names = "roll3_{.col}"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      snap_trend = roll3_offense_pct - roll5_offense_pct,
      role_fppg_trend = roll3_fppg - roll5_fppg,
      target_trend = roll3_targets - roll5_targets,
      carry_trend = roll3_carries - roll5_carries,
      opportunity_per_snap = dplyr::case_when(
        position == "QB" ~ wk_rate(roll3_pass_attempts + roll3_carries, roll3_offense_snaps),
        position == "RB" ~ wk_rate(roll3_carries + roll3_targets, roll3_offense_snaps),
        TRUE ~ wk_rate(roll3_targets, roll3_offense_snaps)
      )
    ) |> dplyr::select(-position)
  player_state$ngs_available <- as.numeric(rowSums(is.finite(as.matrix(player_state[, intersect(grep("^roll3_ngs_", names(player_state), value = TRUE), names(player_state)), drop = FALSE]))) > 0)
  team_state <- tw |>
    dplyr::group_by(team) |>
    dplyr::arrange(week, .by_group = TRUE) |>
    dplyr::summarise(roll3_team_pass_attempts = mean(utils::tail(team_pass_attempts, 3), na.rm = TRUE), roll3_team_carries = mean(utils::tail(team_carries, 3), na.rm = TRUE), .groups = "drop")
} else {
  player_state <- data.frame(player_id = character())
  team_state <- data.frame(team = character())
}

future_grid <- future_grid |> dplyr::left_join(player_state, by = "player_id") |> dplyr::left_join(team_state, by = "team")

# HOTFIX 2.2.6: before Week 1 there may be no current-season player/team rows.
# In that case the left joins above correctly add no rolling team-volume columns.
# Accessing a missing tibble column with `$` returns NULL, which previously produced
# numeric(0) here and caused a vctrs incompatible-size error when assigned back to
# the non-empty future schedule grid.  Use the validated 2.0 team-volume forecast
# as the preseason fallback and keep current-season rolling volume whenever it exists.
team_volume_path_226 <- paste0("data/processed/team_projection_table_2_0_", CURRENT_SEASON, ".csv")
team_volume_226 <- data.frame(team = character(), team_projected_pass_attempts_pg = numeric(), team_projected_carries_pg = numeric())
if (file.exists(team_volume_path_226)) {
  tv <- readr::read_csv(team_volume_path_226, show_col_types = FALSE, progress = FALSE)
  for (nm in c("team", "team_projected_pass_attempts_pg", "team_projected_carries_pg")) tv <- ensure_weekly_col21(tv, nm, NA)
  team_volume_226 <- tv |>
    dplyr::transmute(
      team = wk_chr(team),
      team_projected_pass_attempts_pg = wk_num(team_projected_pass_attempts_pg),
      team_projected_carries_pg = wk_num(team_projected_carries_pg)
    ) |>
    dplyr::filter(!is.na(team), team != "") |>
    dplyr::distinct(team, .keep_all = TRUE)
}
future_grid <- future_grid |> dplyr::left_join(team_volume_226, by = "team")
for (nm in c("roll3_team_pass_attempts", "roll3_team_carries")) {
  if (!nm %in% names(future_grid)) future_grid[[nm]] <- NA_real_
}
if (!"team_projected_pass_attempts_pg" %in% names(future_grid)) future_grid$team_projected_pass_attempts_pg <- NA_real_
if (!"team_projected_carries_pg" %in% names(future_grid)) future_grid$team_projected_carries_pg <- NA_real_

# Prefer current-season rolling state; otherwise use the 2.0 preseason team-volume
# forecast. League-level fallbacks are only a final guardrail if a team is unmapped.
had_current_team_state_226 <- is.finite(wk_num(future_grid$roll3_team_pass_attempts)) | is.finite(wk_num(future_grid$roll3_team_carries))
future_grid$roll3_team_pass_attempts <- dplyr::coalesce(
  wk_num(future_grid$roll3_team_pass_attempts),
  wk_num(future_grid$team_projected_pass_attempts_pg),
  34
)
future_grid$roll3_team_carries <- dplyr::coalesce(
  wk_num(future_grid$roll3_team_carries),
  wk_num(future_grid$team_projected_carries_pg),
  27
)
future_grid$expected_team_plays <- future_grid$roll3_team_pass_attempts + future_grid$roll3_team_carries
future_grid$expected_team_pass_rate <- wk_rate(future_grid$roll3_team_pass_attempts, future_grid$expected_team_plays, .56)
cat("[2.2.6 WEEKLY PROJECT] Team-volume state ready: ", nrow(future_grid),
    " scheduled player-weeks | current-season rolling team state rows: ",
    sum(had_current_team_state_226, na.rm = TRUE),
    " | preseason/fallback rows: ", sum(!had_current_team_state_226, na.rm = TRUE), "\n", sep = "")

# Training-consistent defaults before a player has appeared this season.
for (nm in c("games_played_prior", "roll3_fppg_sd", "roll3_targets", "roll5_targets", "roll3_carries", "roll5_carries", "roll3_pass_attempts", "roll5_pass_attempts",
             "roll3_target_share", "roll3_carry_share", "roll3_pass_attempt_share", "roll3_team_pass_attempts", "roll3_team_carries",
             "roll3_offense_pct", "roll5_offense_pct", "roll3_offense_snaps", "snap_trend", "opportunity_per_snap",
             "role_fppg_trend", "target_trend", "carry_trend", "expected_team_plays", "expected_team_pass_rate",
             "season_targets_prior", "season_carries_prior", "season_pass_attempts_prior",
             "roll3_catch_rate", "roll3_ypt", "roll3_rush_ypc", "roll3_pass_ypa", "roll3_pass_td_rate", "roll3_interception_rate", "roll3_rush_td_rate", "roll3_rec_td_rate",
             "season_to_date_catch_rate", "season_to_date_ypt", "season_to_date_rush_ypc", "season_to_date_pass_ypa", "season_to_date_pass_td_rate", "season_to_date_interception_rate", "season_to_date_rush_td_rate", "season_to_date_rec_td_rate",
             "ngs_available", "roll3_ngs_cpoe", "roll3_ngs_air_yards", "roll3_ngs_time_to_throw", "roll3_ngs_aggressiveness", "roll3_ngs_separation", "roll3_ngs_cushion", "roll3_ngs_yac_oe", "roll3_ngs_air_yard_share", "roll3_ngs_rush_yoe_pa", "roll3_ngs_box_rate", "roll3_ngs_time_to_los")) {
  if (!nm %in% names(future_grid)) future_grid[[nm]] <- 0
  future_grid[[nm]] <- dplyr::coalesce(wk_num(future_grid[[nm]]), 0)
}
for (nm in c("prior_game_fppg", "roll3_fppg", "roll5_fppg", "season_to_date_fppg")) {
  if (!nm %in% names(future_grid)) future_grid[[nm]] <- future_grid$preseason_prior_fppg
  future_grid[[nm]] <- dplyr::coalesce(wk_num(future_grid[[nm]]), future_grid$preseason_prior_fppg)
}

# ------------------------------------------------------------
# Opponent positional allowance state: use current-season completed games when
# available; otherwise fall back to the final historical season state.
# ------------------------------------------------------------
hist_pos <- readr::read_csv("data/processed/weekly_defense_position_results.csv", show_col_types = FALSE, progress = FALSE)
if (nrow(current_weekly) > 0) {
  # nflverse current player-week data can already contain an `opponent` field.
  # Joining another `opponent` from the schedule without protecting the name
  # creates opponent.x/opponent.y, after which downstream filter(opponent) fails.
  # Keep the observed/current-stat opponent when present and use the schedule as
  # the authoritative fallback. One team has at most one regular-season game per
  # week, so this is a many-to-one join.
  cur_sched <- team_schedule |>
    dplyr::select(season, week, team, opponent_sched = opponent)
  if (!"opponent" %in% names(current_weekly)) current_weekly$opponent <- NA_character_
  current_weekly$opponent <- wk_chr(current_weekly$opponent)
  cur_prior <- players |> dplyr::select(player_id, preseason_prior_fppg)
  cur_actual <- current_weekly |>
    dplyr::left_join(cur_sched, by = c("season", "week", "team"), relationship = "many-to-one") |>
    dplyr::mutate(opponent = dplyr::coalesce(wk_chr(opponent), wk_chr(opponent_sched))) |>
    dplyr::select(-dplyr::any_of("opponent_sched")) |>
    dplyr::left_join(cur_prior, by = "player_id", relationship = "many-to-one")
  add_current_expectation21 <- function(d) {
    d <- d[order(d$week), , drop = FALSE]
    d$games_before <- pmax(0, seq_len(nrow(d)) - 1L)
    d$roll3_before <- lag_roll_mean21(d$weekly_fppg, 3)
    d$expected_before_week <- weekly_baseline21(d$preseason_prior_fppg, d$roll3_before, d$games_before)
    d
  }
  cur_actual <- dplyr::bind_rows(lapply(split(cur_actual, cur_actual$player_id), add_current_expectation21))
  cur_actual$defense_residual_allowed <- cur_actual$weekly_fppg - cur_actual$expected_before_week
  cur_def <- cur_actual |>
    dplyr::filter(!is.na(opponent), opponent != "") |>
    dplyr::group_by(season, week, defense = opponent, position) |>
    dplyr::summarise(def_pos_residual = mean(defense_residual_allowed, na.rm = TRUE), def_pos_fppg_allowed = mean(weekly_fppg, na.rm = TRUE), .groups = "drop")
  hist_pos <- dplyr::bind_rows(hist_pos, cur_def)
}

latest_pos_state21 <- function(defense_arg, pos_arg) {
  cur <- hist_pos |> dplyr::filter(.data$defense == defense_arg, .data$position == pos_arg, .data$season == CURRENT_SEASON) |> dplyr::arrange(week)
  if (nrow(cur) > 0) {
    return(c(res4 = mean(utils::tail(cur$def_pos_residual, 4), na.rm = TRUE), res8 = mean(utils::tail(cur$def_pos_residual, 8), na.rm = TRUE), fp4 = mean(utils::tail(cur$def_pos_fppg_allowed, 4), na.rm = TRUE)))
  }
  prev <- hist_pos |> dplyr::filter(.data$defense == defense_arg, .data$position == pos_arg, .data$season == TRAIN_END)
  if (nrow(prev) == 0) return(c(res4 = 0, res8 = 0, fp4 = NA_real_))
  c(res4 = mean(prev$def_pos_residual, na.rm = TRUE), res8 = mean(prev$def_pos_residual, na.rm = TRUE), fp4 = mean(prev$def_pos_fppg_allowed, na.rm = TRUE))
}

# PBP defense state.
def_raw <- readr::read_csv("data/raw/weekly_defense_context_raw.csv", show_col_types = FALSE, progress = FALSE)
def_metrics <- c("def_pass_epa_allowed", "def_rush_epa_allowed", "def_pass_success_allowed", "def_rush_success_allowed", "def_explosive_pass_rate", "def_deep_pass_rate", "def_middle_pass_rate", "def_deep_epa_allowed", "def_middle_epa_allowed", "def_sack_rate", "def_qb_hit_rate", "def_redzone_pass_td_rate", "def_redzone_rush_td_rate")
for (nm in c("season", "week", "defense", def_metrics)) def_raw <- ensure_weekly_col21(def_raw, nm, 0)
latest_def_state21 <- function(defense_arg) {
  cur <- def_raw |> dplyr::filter(.data$defense == defense_arg, .data$season == CURRENT_SEASON) |> dplyr::arrange(week)
  src <- if (nrow(cur) > 0) cur else def_raw |> dplyr::filter(.data$defense == defense_arg, .data$season == TRAIN_END) |> dplyr::arrange(week)
  out <- list()
  for (nm in def_metrics) {
    z <- wk_num(src[[nm]])
    out[[paste0(nm, "_roll4")]] <- if (length(z) > 0) mean(utils::tail(z, 4), na.rm = TRUE) else 0
    if (nm %in% c("def_pass_epa_allowed", "def_rush_epa_allowed")) out[[paste0(nm, "_roll8")]] <- if (length(z) > 0) mean(utils::tail(z, 8), na.rm = TRUE) else 0
  }
  as.data.frame(out)
}

def_pairs <- future_grid |> dplyr::distinct(opponent, position)
def_rows <- lapply(seq_len(nrow(def_pairs)), function(i) {
  opp <- def_pairs$opponent[i]; pos <- def_pairs$position[i]
  a <- latest_pos_state21(opp, pos); b <- latest_def_state21(opp)
  cbind(data.frame(opponent = opp, position = pos, opp_pos_residual_roll4 = a["res4"], opp_pos_residual_roll8 = a["res8"], opp_pos_fppg_allowed_roll4 = a["fp4"]), b)
})
def_state <- dplyr::bind_rows(def_rows)
future_grid <- future_grid |> dplyr::left_join(def_state, by = c("opponent", "position"))
future_grid$def_wr_residual_roll4 <- ifelse(future_grid$position == "WR", future_grid$opp_pos_residual_roll4, 0)
future_grid$def_te_residual_roll4 <- ifelse(future_grid$position == "TE", future_grid$opp_pos_residual_roll4, 0)
for (nm in c("preseason_deep_target_rate", "preseason_middle_target_rate", "preseason_redzone_target_rate")) future_grid[[nm]] <- dplyr::coalesce(wk_num(future_grid[[nm]]), 0)
future_grid$qb_pass_matchup <- future_grid$roll3_pass_attempts * dplyr::coalesce(wk_num(future_grid$def_pass_epa_allowed_roll4), 0)
future_grid$qb_pressure_matchup <- future_grid$roll3_pass_attempts * dplyr::coalesce(wk_num(future_grid$def_sack_rate_roll4), 0)
future_grid$rb_rush_matchup <- future_grid$roll3_carries * dplyr::coalesce(wk_num(future_grid$def_rush_epa_allowed_roll4), 0)
future_grid$rb_receiving_matchup <- future_grid$roll3_targets * dplyr::coalesce(wk_num(future_grid$opp_pos_residual_roll4), 0)
future_grid$wr_volume_matchup <- future_grid$roll3_target_share * dplyr::coalesce(wk_num(future_grid$def_pass_epa_allowed_roll4), 0)
future_grid$wr_deep_matchup <- future_grid$preseason_deep_target_rate * dplyr::coalesce(wk_num(future_grid$def_deep_epa_allowed_roll4), 0)
future_grid$wr_redzone_matchup <- future_grid$preseason_redzone_target_rate * dplyr::coalesce(wk_num(future_grid$def_redzone_pass_td_rate_roll4), 0)
future_grid$te_middle_matchup <- future_grid$preseason_middle_target_rate * dplyr::coalesce(wk_num(future_grid$def_middle_epa_allowed_roll4), 0)
future_grid$te_redzone_matchup <- future_grid$preseason_redzone_target_rate * dplyr::coalesce(wk_num(future_grid$def_redzone_pass_td_rate_roll4), 0)

# Injury status by exact player-week when available.
if (nrow(cur_inj$player) > 0) future_grid <- future_grid |> dplyr::left_join(cur_inj$player, by = c("season", "week", "player_id"))
if (nrow(cur_inj$team) > 0) future_grid <- future_grid |> dplyr::left_join(cur_inj$team, by = c("season", "week", "team"))
for (nm in c("injury_risk", "practice_risk", "team_skill_out_count", "team_skill_questionable_count")) {
  if (!nm %in% names(future_grid)) future_grid[[nm]] <- 0
  future_grid[[nm]] <- dplyr::coalesce(wk_num(future_grid[[nm]]), 0)
}
if (!"injury_status" %in% names(future_grid)) future_grid$injury_status <- NA_character_
if (!"practice_status" %in% names(future_grid)) future_grid$practice_status <- NA_character_
future_grid$season_week <- wk_num(future_grid$week)
future_grid$total_line <- dplyr::coalesce(wk_num(future_grid$total_line), 44)
future_grid$team_spread_line <- dplyr::coalesce(wk_num(future_grid$team_spread_line), 0)
future_grid$implied_team_total <- dplyr::coalesce(wk_num(future_grid$implied_team_total), future_grid$total_line / 2)
future_grid$is_home <- dplyr::coalesce(wk_num(future_grid$is_home), 0)
future_grid$rest_days <- dplyr::coalesce(wk_num(future_grid$rest_days), 7)
future_grid$favorite_points <- pmax(0, wk_num(future_grid$team_spread_line))
future_grid$underdog_points <- pmax(0, -wk_num(future_grid$team_spread_line))
future_grid$favorite_rush_interaction <- future_grid$favorite_points * dplyr::coalesce(wk_num(future_grid$roll3_carries), 0)
future_grid$underdog_target_interaction <- future_grid$underdog_points * dplyr::coalesce(wk_num(future_grid$roll3_targets), 0)
future_grid$matchup_role_interaction <- dplyr::coalesce(wk_num(future_grid$opp_pos_residual_roll4), 0) * pmax(0, dplyr::coalesce(wk_num(future_grid$preseason_prior_fppg), 0))

# Fill any model feature not supplied by current state with zero/fallback so schema
# stays identical to training.
all_week_features <- unique(c(unlist(lapply(POSITIONS, function(p) unique(c(get_features22(p, "full"), get_features22(p, "neutral"), get_features22(p, "opportunity"))))), unlist(lapply(POSITIONS, get_weekly_features21))))
for (nm in all_week_features) if (!nm %in% names(future_grid)) future_grid[[nm]] <- 0
for (nm in all_week_features) future_grid[[nm]] <- dplyr::coalesce(wk_num(future_grid[[nm]]), 0)
future_grid$preseason_prior_fppg <- pmax(0, wk_num(future_grid$preseason_prior_fppg))
future_grid$baseline_weekly_fppg <- weekly_baseline21(future_grid$preseason_prior_fppg, future_grid$roll3_fppg, future_grid$games_played_prior)

# ------------------------------------------------------------
# Predict each scheduled player-week with the 2.3 historical meta-calibration.
# ------------------------------------------------------------
lookup_phase_row23 <- function(tbl, pos, phase_value) {
  z <- tbl[tbl$position == pos & tbl$phase == phase_value, , drop = FALSE]
  if (nrow(z) == 0) z <- tbl[tbl$position == pos & tbl$phase == "ALL", , drop = FALSE]
  if (nrow(z) > 1) z <- z[1, , drop = FALSE]
  z
}

pred_rows <- list()
for (pos in POSITIONS) {
  d <- future_grid |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next
  d$baseline_weekly_fppg <- weekly_baseline21(d$preseason_prior_fppg, d$roll3_fppg, d$games_played_prior)
  d$week_phase_23 <- wk23_phase(d$week)

  legacy_path <- paste0("models/weekly_model_2_1_", pos, ".rds")
  neutral_path <- paste0("models/weekly_2_2_neutral_", pos, ".rds")
  structured_path <- paste0("models/weekly_2_2_structured_", pos, ".rds")
  matchup_path <- paste0("models/weekly_2_2_matchup_", pos, ".rds")
  direct_path <- paste0("models/weekly_2_2_direct_", pos, ".rds")
  for (pp in c(legacy_path, neutral_path, structured_path, direct_path)) if (!file.exists(pp)) stop("Missing 2.3 base model: ", pp)

  legacy_fit <- readRDS(legacy_path)
  neutral_fit <- readRDS(neutral_path)
  structured_fit <- readRDS(structured_path)
  matchup_fit <- if (file.exists(matchup_path)) readRDS(matchup_path) else NULL
  direct_fit <- readRDS(direct_path)

  d$legacy21_model_fppg <- pmax(0, predict_fantasy_model(legacy_fit, d))
  d$legacy21_honest_fppg <- d$baseline_weekly_fppg
  for (ph in unique(d$week_phase_23)) {
    idx <- d$week_phase_23 == ph; lw <- lookup_phase_row23(legacy_weights, pos, ph)
    if (nrow(lw) == 0) lw <- data.frame(weekly_model_weight = 0, prior_weight = 1)
    d$legacy21_honest_fppg[idx] <- apply_legacy21_weight23(d[idx, , drop = FALSE], lw)
  }

  d$neutral_direct_fppg <- pmax(0, predict_fantasy_model(neutral_fit, d))
  st <- predict_structured22(structured_fit, d)
  for (nm in names(st)) d[[nm]] <- st[[nm]]
  d$matchup_delta_22_raw <- predict_matchup_delta22(matchup_fit, d, pos)
  d$structured_matchup_fppg <- pmax(0, d$structured_neutral_fppg + d$matchup_delta_22_raw)
  mc <- matchup_calibration |> dplyr::filter(position == pos) |> dplyr::slice(1)
  d$calibrated_matchup_delta_23 <- apply_matchup_calibration23(d$matchup_delta_22_raw, mc)
  d$structured_matchup_calibrated_fppg <- pmax(0, d$structured_neutral_fppg + d$calibrated_matchup_delta_23)
  d$direct_full_fppg <- pmax(0, predict_fantasy_model(direct_fit, d))

  w22 <- weights22 |> dplyr::filter(position == pos) |> dplyr::slice(1)
  if (nrow(w22) == 0) w22 <- safe_start_stack22()
  d$honest22_fppg <- pmax(0, apply_stack22(d, w22))

  d$meta_precal_fppg <- d$baseline_weekly_fppg
  for (ph in unique(d$week_phase_23)) {
    idx <- d$week_phase_23 == ph; mw <- lookup_phase_row23(meta_weights, pos, ph)
    if (nrow(mw) == 0) mw <- safe_meta_stack23()
    d$meta_precal_fppg[idx] <- pmax(0, apply_meta_stack23(d[idx, , drop = FALSE], mw))
  }
  d$model_disagreement_23 <- model_disagreement23(d)

  rpath <- paste0("models/weekly_2_3_residual_calibrator_", pos, ".rds")
  rcal <- if (file.exists(rpath)) readRDS(rpath) else NULL
  d$historical_residual_correction_23 <- predict_residual_calibrator23(rcal, d, pos)
  d$candidate23_fppg <- pmax(0, d$meta_precal_fppg + d$historical_residual_correction_23)

  d$projected_weekly_fppg_pre_availability <- d$candidate23_fppg
  d$version_weight_21 <- 0; d$version_weight_22 <- 0; d$version_weight_23 <- 1
  for (ph in unique(d$week_phase_23)) {
    idx <- d$week_phase_23 == ph; vw <- lookup_phase_row23(version_weights, pos, ph)
    if (nrow(vw) == 0) vw <- safe_version_guard23()
    d$projected_weekly_fppg_pre_availability[idx] <- pmax(0, apply_version_guard23(d[idx, , drop = FALSE], vw))
    d$version_weight_21[idx] <- wk_num(vw$w_legacy21_version[1]); d$version_weight_22[idx] <- wk_num(vw$w_22_version[1]); d$version_weight_23[idx] <- wk_num(vw$w_23_version[1])
  }

  # Expose production decomposition for the app.
  d$matchup_delta_22 <- d$calibrated_matchup_delta_23
  d$matchup_delta_vs_prior <- d$calibrated_matchup_delta_23
  d$matchup_grade <- matchup_grade22(d$calibrated_matchup_delta_23)
  d$availability_factor <- dplyr::case_when(
    d$injury_risk >= 0.99 ~ 0, d$injury_risk >= 0.75 ~ 0.25, d$injury_risk >= 0.35 ~ 0.85, d$practice_risk >= 0.70 ~ 0.90, TRUE ~ 1
  )
  d$projected_weekly_fppg <- d$projected_weekly_fppg_pre_availability * d$availability_factor
  conf <- assign_confidence23(d$model_disagreement_23, pos, confidence_calibration)
  d$projection_confidence <- conf$projection_confidence
  d$expected_abs_error <- conf$expected_abs_error

  # Keep backward-compatible stack columns; 2.3 adds explicit version weights.
  d$stack_prior_weight <- NA_real_; d$stack_neutral_weight <- NA_real_; d$stack_structured_matchup_weight <- NA_real_; d$stack_direct_weight <- NA_real_

  res <- residual_pool |> dplyr::filter(position == pos) |> dplyr::pull(residual)
  dist <- empirical_distribution22(d$projected_weekly_fppg, res, pos)
  for (nm in names(dist)) d[[nm]] <- dist[[nm]]
  out_mask <- d$availability_factor <= 0
  if (any(out_mask)) {
    d$weekly_median[out_mask] <- 0; d$weekly_floor[out_mask] <- 0; d$weekly_ceiling[out_mask] <- 0
    d$boom_probability[out_mask] <- 0; d$bust_probability[out_mask] <- 1
  }
  pred_rows[[length(pred_rows) + 1]] <- d
}
future_pred <- dplyr::bind_rows(pred_rows)

# Replace already-played weeks with actual points in the season ledger, while
# keeping the pregame projection columns for later accuracy audits when available.
actual_lookup <- if (nrow(current_weekly) > 0) current_weekly |> dplyr::select(player_id, week, actual_weekly_fppg = weekly_fppg) else data.frame(player_id = character(), week = integer(), actual_weekly_fppg = numeric())
future_pred <- future_pred |>
  dplyr::left_join(actual_lookup, by = c("player_id", "week")) |>
  dplyr::mutate(
    is_actual = as.integer(is.finite(actual_weekly_fppg)),
    season_ledger_points = dplyr::if_else(is_actual == 1, actual_weekly_fppg, projected_weekly_fppg)
  ) |>
  dplyr::group_by(week, position) |>
  dplyr::arrange(dplyr::desc(projected_weekly_fppg), .by_group = TRUE) |>
  dplyr::mutate(weekly_position_rank = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::arrange(week, dplyr::desc(projected_weekly_fppg))

# Empirical Top-N probability from the same historical residual pool. This is a
# decision metric, not a separate classifier.
add_topn_group22 <- function(d) {
  if (nrow(d) == 0) return(d)
  pos <- as.character(d$position[1]); cutoff <- as.numeric(WEEKLY_22_TOP_PROB_CUTOFF[[pos]])
  res <- residual_pool |> dplyr::filter(position == pos) |> dplyr::pull(residual)
  res <- wk_num(res); res <- res[is.finite(res)]
  if (length(res) < 30 || !is.finite(cutoff)) { d$topN_probability <- NA_real_; return(d) }
  draws <- min(1000L, WEEKLY_22_SIM_DRAWS)
  n <- nrow(d); counts <- rep(0, n)
  set.seed(SEED + as.integer(d$week[1]) * 100 + match(pos, POSITIONS))
  for (b in seq_len(draws)) {
    sim <- pmax(0, d$projected_weekly_fppg + sample(res, n, replace = TRUE))
    rr <- rank(-sim, ties.method = "min")
    counts <- counts + as.numeric(rr <= cutoff)
  }
  d$topN_probability <- counts / draws
  d
}
future_pred <- dplyr::bind_rows(lapply(split(future_pred, interaction(future_pred$week, future_pred$position, drop = TRUE)), add_topn_group22)) |>
  dplyr::arrange(week, dplyr::desc(projected_weekly_fppg))

readr::write_csv(future_pred, paste0("output/weekly_", CURRENT_SEASON, "_projections.csv"))

# Determine the next/upcoming week from the schedule helper if available.
current_week <- tryCatch(as.integer(nflreadr::get_current_week()), error = function(e) {
  unplayed <- team_schedule |> dplyr::filter(game_played == 0)
  if (nrow(unplayed) > 0) min(unplayed$week, na.rm = TRUE) else max(team_schedule$week, na.rm = TRUE)
})
if (!is.finite(current_week)) current_week <- 1L
writeLines(as.character(current_week), "output/current_week.txt")
week_now <- future_pred |> dplyr::filter(week == current_week, is_actual == 0) |> dplyr::arrange(dplyr::desc(projected_weekly_fppg))
readr::write_csv(week_now, paste0("output/week_", current_week, "_rankings.csv"))

# Keep lightweight projection snapshots so the app can show how the model's
# opinion changed during the season. Historical snapshots are never model
# features; they are audit/display data only.
if (isTRUE(WEEKLY_23_SAVE_PROJECTION_HISTORY) && nrow(week_now) > 0) {
  hist_path <- paste0("output/projection_history_", CURRENT_SEASON, ".csv")
  snap <- week_now |>
    dplyr::transmute(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), week, player_id, player_display_name, position, team, opponent,
      projected_weekly_fppg, weekly_floor, weekly_ceiling, projection_confidence, expected_abs_error,
      calibrated_matchup_delta_23, model_disagreement_23, historical_residual_correction_23,
      version_weight_21, version_weight_22, version_weight_23
    )
  old_hist <- if (file.exists(hist_path)) tryCatch(readr::read_csv(hist_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
  readr::write_csv(dplyr::bind_rows(old_hist, snap), hist_path)
}

ros <- future_pred |>
  dplyr::group_by(player_id, player_display_name, position, team) |>
  dplyr::summarise(
    actual_points_to_date = sum(dplyr::if_else(is_actual == 1, actual_weekly_fppg, 0), na.rm = TRUE),
    projected_remaining_points = sum(dplyr::if_else(is_actual == 0, projected_weekly_fppg, 0), na.rm = TRUE),
    projected_full_season_points = sum(season_ledger_points, na.rm = TRUE),
    remaining_games = sum(is_actual == 0), .groups = "drop"
  ) |>
  dplyr::group_by(position) |>
  dplyr::arrange(dplyr::desc(projected_full_season_points), .by_group = TRUE) |>
  dplyr::mutate(ros_position_rank = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(projected_full_season_points))
readr::write_csv(ros, paste0("output/rest_of_season_", CURRENT_SEASON, ".csv"))

cat("[2.3 WEEKLY PROJECT] Projected ", nrow(future_pred), " player-weeks for ", CURRENT_SEASON, ".\n", sep = "")
cat("[2.3 WEEKLY PROJECT] Upcoming week: ", current_week, " | ranked players: ", nrow(week_now), "\n", sep = "")
cat("[2.3 WEEKLY PROJECT] Outputs: weekly_", CURRENT_SEASON, "_projections.csv, week_", current_week, "_rankings.csv, rest_of_season_", CURRENT_SEASON, ".csv\n", sep = "")
