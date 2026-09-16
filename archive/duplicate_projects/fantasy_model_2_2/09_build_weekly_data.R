# ============================================================
# FANTASY MODEL 2.2 - BUILD STRICT PRE-KICKOFF PLAYER-WEEK TABLE
# ============================================================
source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr", "nflreadr", "rpart"))
source("R/weekly_engine.R")
source("R/weekly_engine_22.R")

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

if (!file.exists("data/raw/weekly_defense_context_raw.csv")) {
  stop("Missing weekly PBP defense store. Run source(\"BUILD_WEEKLY_DEFENSE_CONTEXT.R\") once first.")
}

hist_seasons <- WEEKLY_TRAIN_START:TRAIN_END
cat("[2.2 WEEKLY DATA] Loading player-week stats: ", min(hist_seasons), "-", max(hist_seasons), "\n", sep = "")
weekly_raw <- nflreadr::load_player_stats(seasons = hist_seasons, summary_level = "week")
readr::write_csv(weekly_raw, "data/raw/player_weekly_stats.csv")
weekly <- normalize_weekly_stats21(weekly_raw)
# Standardized fields used by 2.2 rate shrinkage. nflverse names QB interceptions
# passing_interceptions in current player-week files.
weekly$passing_interceptions_22 <- wk_num(first_existing_col21(weekly, c("passing_interceptions", "interceptions"), 0))
weekly$fumbles_lost_22 <- wk_num(first_existing_col21(weekly, c("fumbles_lost"), 0))
rm(weekly_raw); invisible(gc(full = TRUE))
if (nrow(weekly) == 0) stop("No historical weekly player stats were loaded.")

cat("[2.2 WEEKLY DATA] Loading schedules through ", CURRENT_SEASON, "...\n", sep = "")
schedules <- nflreadr::load_schedules(seasons = WEEKLY_TRAIN_START:CURRENT_SEASON)
readr::write_csv(schedules, "data/raw/schedules_weekly_model.csv")
team_schedule <- schedule_team_rows21(schedules)

cat("[2.2 WEEKLY DATA] Loading historical injury reports where available...\n")
# nflverse's injury source currently ends after 2024. Preserve the historical
# signal rather than allowing a failed 2025/2026 request to erase all injury data.
injuries <- data.frame()
if (isTRUE(WEEKLY_USE_INJURIES)) {
  injury_end <- min(2024L, TRAIN_END)
  if (injury_end >= WEEKLY_TRAIN_START) {
    injuries <- tryCatch(
      nflreadr::load_injuries(seasons = WEEKLY_TRAIN_START:injury_end),
      error = function(e) { warning("Historical injury reports unavailable; continuing without them. ", conditionMessage(e)); data.frame() }
    )
  }
}
if (nrow(injuries) > 0) readr::write_csv(injuries, "data/raw/injuries_weekly_model.csv")
inj <- prepare_injuries21(injuries)
rm(injuries); invisible(gc(full = TRUE))

# Snap share is a lightweight live proxy for playing time / route opportunity.
# It is lagged before entering any player-week model, so the current game's snaps
# are never used to predict that same game.
cat("[2.2 WEEKLY DATA] Loading historical snap counts...\n")
snap_raw <- tryCatch(nflreadr::load_snap_counts(seasons = hist_seasons), error = function(e) {
  warning("Snap counts unavailable; weekly model will continue without snap-role features. ", conditionMessage(e)); data.frame()
})
if (nrow(snap_raw) > 0) {
  player_ids <- tryCatch(nflreadr::load_players(), error = function(e) data.frame())
  if (nrow(player_ids) > 0 && all(c("gsis_id", "pfr_id") %in% names(player_ids)) && "pfr_player_id" %in% names(snap_raw)) {
    id_map <- player_ids |>
      dplyr::transmute(player_id = wk_chr(gsis_id), pfr_player_id = wk_chr(pfr_id)) |>
      dplyr::filter(!is.na(pfr_player_id), pfr_player_id != "") |>
      dplyr::distinct(pfr_player_id, .keep_all = TRUE)
    snap_week <- snap_raw |>
      dplyr::mutate(pfr_player_id = wk_chr(pfr_player_id), season = as.integer(wk_num(season)), week = as.integer(wk_num(week)),
                    offense_snaps = wk_num(offense_snaps), offense_pct = wk_num(offense_pct)) |>
      dplyr::left_join(id_map, by = "pfr_player_id") |>
      dplyr::filter(!is.na(player_id), week >= 1, week <= 18) |>
      dplyr::group_by(season, week, player_id) |>
      dplyr::summarise(offense_snaps = sum(offense_snaps, na.rm = TRUE), offense_pct = max(offense_pct, na.rm = TRUE), .groups = "drop")
    snap_week$offense_pct[!is.finite(snap_week$offense_pct)] <- NA_real_
    # PFR usually stores pct as 0-1, but normalize defensively if a source version uses 0-100.
    snap_week$offense_pct <- ifelse(snap_week$offense_pct > 1.5, snap_week$offense_pct / 100, snap_week$offense_pct)
    weekly <- weekly |> dplyr::left_join(snap_week, by = c("season", "week", "player_id"))
    readr::write_csv(snap_week, "data/raw/snap_counts_weekly_model.csv")
    rm(player_ids, id_map, snap_week)
  } else {
    weekly$offense_snaps <- NA_real_; weekly$offense_pct <- NA_real_
  }
} else {
  weekly$offense_snaps <- NA_real_; weekly$offense_pct <- NA_real_
}
rm(snap_raw); invisible(gc(full = TRUE))

# ------------------------------------------------------------
# Weekly Next Gen Stats. These are joined to the game in which they occurred,
# then lagged inside add_player_rolls21 below. NGS is optional because NFL NGS
# applies minimum-attempt thresholds; absence is treated as unknown, not zero
# performance.
# ------------------------------------------------------------
if (isTRUE(WEEKLY_22_USE_NGS)) {
  cat("[2.2 WEEKLY DATA] Loading historical weekly Next Gen Stats...\n")
  ngs_parts <- list()
  for (typ in c("passing", "receiving", "rushing")) {
    raw_ngs <- tryCatch(nflreadr::load_nextgen_stats(seasons = hist_seasons, stat_type = typ), error = function(e) data.frame())
    norm_ngs <- normalize_ngs22(raw_ngs, typ)
    if (nrow(norm_ngs) > 0) ngs_parts[[typ]] <- norm_ngs
    rm(raw_ngs, norm_ngs); invisible(gc(full = TRUE))
  }
  if (length(ngs_parts) > 0) {
    ngs_week <- Reduce(function(x, y) dplyr::full_join(x, y, by = c("season", "week", "player_id")), ngs_parts)
    weekly <- weekly |> dplyr::left_join(ngs_week, by = c("season", "week", "player_id"))
    readr::write_csv(ngs_week, "data/raw/ngs_weekly_model_2_2.csv")
    rm(ngs_week)
  }
}

# ------------------------------------------------------------
# Team-week totals and player opportunity shares.
# ------------------------------------------------------------
team_week <- weekly |>
  dplyr::group_by(season, week, team) |>
  dplyr::summarise(
    team_targets = sum(targets, na.rm = TRUE),
    team_carries = sum(carries, na.rm = TRUE),
    team_pass_attempts = sum(pass_attempts, na.rm = TRUE),
    .groups = "drop"
  )
weekly <- weekly |>
  dplyr::left_join(team_week, by = c("season", "week", "team")) |>
  dplyr::mutate(
    target_share_week = wk_rate(targets, team_targets),
    carry_share_week = wk_rate(carries, team_carries),
    pass_attempt_share_week = wk_rate(pass_attempts, team_pass_attempts)
  )

# Schedule is the canonical opponent source. Historical weekly stats occasionally
# carry an opponent field too, but schedule rows are more consistent for byes/home/rest.
weekly <- weekly |>
  dplyr::left_join(team_schedule, by = c("season", "week", "team"), suffix = c("", "_sched")) |>
  dplyr::mutate(opponent = dplyr::coalesce(opponent_sched, opponent)) |>
  dplyr::select(-dplyr::any_of("opponent_sched"))

# ------------------------------------------------------------
# Preseason player prior from the already validated season feature store.
# This field is constructed before the target season and is therefore leak-free.
# ------------------------------------------------------------
if (!file.exists("data/processed/model_table.csv")) stop("Missing validated season feature store: data/processed/model_table.csv")
season_features <- readr::read_csv("data/processed/model_table.csv", show_col_types = FALSE, progress = FALSE)
for (nm in c("player_id", "season", "prior_fppg", "recent_weighted_fppg", "career_fppg_before", "is_rookie", "age", "experience",
             "rec_prior_deep_target_rate", "rec_prior_middle_target_rate", "rec_prior_redzone_target_rate",
             "prior_catch_rate", "prior_yards_per_target", "prior_yards_per_carry", "prior_yards_per_attempt",
             "prior_pass_td_rate", "prior_rush_td_rate", "prior_rec_td_rate")) season_features <- ensure_weekly_col21(season_features, nm, 0)
prior_map <- season_features |>
  dplyr::transmute(
    player_id = wk_chr(player_id), season = as.integer(wk_num(season)),
    preseason_prior_fppg = dplyr::coalesce(wk_num(recent_weighted_fppg), wk_num(prior_fppg), wk_num(career_fppg_before), 0),
    weekly_is_rookie = wk_num(is_rookie), weekly_age = wk_num(age), weekly_experience = wk_num(experience),
    preseason_deep_target_rate = wk_num(rec_prior_deep_target_rate),
    preseason_middle_target_rate = wk_num(rec_prior_middle_target_rate),
    preseason_redzone_target_rate = wk_num(rec_prior_redzone_target_rate),
    prior_catch_rate = wk_num(prior_catch_rate),
    prior_yards_per_target = wk_num(prior_yards_per_target),
    prior_yards_per_carry = wk_num(prior_yards_per_carry),
    prior_yards_per_attempt = wk_num(prior_yards_per_attempt),
    prior_pass_td_rate = wk_num(prior_pass_td_rate),
    prior_rush_td_rate = wk_num(prior_rush_td_rate),
    prior_rec_td_rate = wk_num(prior_rec_td_rate),
    prior_interception_rate = wk_num(first_existing_col21(season_features, c("prior_interception_rate", "prior_int_rate"), 0.025))
  ) |>
  dplyr::distinct(player_id, season, .keep_all = TRUE)
# When available, use the season model's own honest OOF projection as the
# historical preseason prior. This makes weekly backtests mirror the real 2.0 ->
# 2.1 handoff rather than substituting last season's FPPG.
oof_prior_path <- "output/validation_2_0_predictions.csv"
if (file.exists(oof_prior_path)) {
  oof_prior <- readr::read_csv(oof_prior_path, show_col_types = FALSE, progress = FALSE)
  if (all(c("player_id", "target_year", "predicted_fppg_2_0") %in% names(oof_prior))) {
    oof_prior <- oof_prior |>
      dplyr::transmute(player_id = wk_chr(player_id), season = as.integer(wk_num(target_year)), season_model_oof_prior = pmax(0, wk_num(predicted_fppg_2_0))) |>
      dplyr::distinct(player_id, season, .keep_all = TRUE)
    prior_map <- prior_map |> dplyr::left_join(oof_prior, by = c("player_id", "season")) |>
      dplyr::mutate(preseason_prior_fppg = dplyr::coalesce(season_model_oof_prior, preseason_prior_fppg)) |>
      dplyr::select(-season_model_oof_prior)
  }
}
weekly <- weekly |> dplyr::left_join(prior_map, by = c("player_id", "season"))
rm(season_features, prior_map); invisible(gc(full = TRUE))

# Fill missing preseason priors with the position-season veteran median, never with
# current-week production.
position_priors <- weekly |>
  dplyr::group_by(season, position) |>
  dplyr::summarise(position_preseason_median = stats::median(preseason_prior_fppg[is.finite(preseason_prior_fppg) & preseason_prior_fppg > 0], na.rm = TRUE), .groups = "drop")
position_priors$position_preseason_median[!is.finite(position_priors$position_preseason_median)] <- 0
weekly <- weekly |>
  dplyr::left_join(position_priors, by = c("season", "position")) |>
  dplyr::mutate(preseason_prior_fppg = dplyr::if_else(is.finite(preseason_prior_fppg), preseason_prior_fppg, pmax(0, 0.55 * position_preseason_median)))

# ------------------------------------------------------------
# Leak-free rolling player/team features. Every rolling statistic explicitly
# excludes the current player-week.
# ------------------------------------------------------------
add_player_rolls21 <- function(d) {
  d <- d[order(d$week), , drop = FALSE]
  d$games_played_prior <- lag_games_played21(nrow(d))
  d$prior_game_fppg <- dplyr::lag(d$weekly_fppg)
  d$roll3_fppg <- lag_roll_mean21(d$weekly_fppg, 3)
  d$roll5_fppg <- lag_roll_mean21(d$weekly_fppg, 5)
  d$roll3_fppg_sd <- lag_roll_sd21(d$weekly_fppg, 3)
  d$season_to_date_fppg <- lag_cum_mean21(d$weekly_fppg)
  d$roll3_targets <- lag_roll_mean21(d$targets, 3); d$roll5_targets <- lag_roll_mean21(d$targets, 5)
  d$roll3_carries <- lag_roll_mean21(d$carries, 3); d$roll5_carries <- lag_roll_mean21(d$carries, 5)
  d$roll3_pass_attempts <- lag_roll_mean21(d$pass_attempts, 3); d$roll5_pass_attempts <- lag_roll_mean21(d$pass_attempts, 5)
  d$roll3_target_share <- lag_roll_mean21(d$target_share_week, 3)
  d$roll3_carry_share <- lag_roll_mean21(d$carry_share_week, 3)
  d$roll3_pass_attempt_share <- lag_roll_mean21(d$pass_attempt_share_week, 3)
  d$roll3_offense_pct <- lag_roll_mean21(d$offense_pct, 3)
  d$roll5_offense_pct <- lag_roll_mean21(d$offense_pct, 5)
  d$roll3_offense_snaps <- lag_roll_mean21(d$offense_snaps, 3)
  d$snap_trend <- d$roll3_offense_pct - d$roll5_offense_pct
  d$role_fppg_trend <- d$roll3_fppg - d$roll5_fppg
  d$target_trend <- d$roll3_targets - d$roll5_targets
  d$carry_trend <- d$roll3_carries - d$roll5_carries
  d$opportunity_per_snap <- dplyr::case_when(
    d$position == "QB" ~ wk_rate(d$roll3_pass_attempts + d$roll3_carries, d$roll3_offense_snaps),
    d$position == "RB" ~ wk_rate(d$roll3_carries + d$roll3_targets, d$roll3_offense_snaps),
    TRUE ~ wk_rate(d$roll3_targets, d$roll3_offense_snaps)
  )

  # Exposure and efficiency rates are all pre-week (lagged/cumulative through
  # the previous game). These feed the 2.2 shrinkage stat-line layer.
  d$season_targets_prior <- lag_cum_sum22(d$targets)
  d$season_carries_prior <- lag_cum_sum22(d$carries)
  d$season_pass_attempts_prior <- lag_cum_sum22(d$pass_attempts)
  d$roll3_catch_rate <- lag_roll_rate22(d$receptions, d$targets, 3)
  d$roll3_ypt <- lag_roll_rate22(d$receiving_yards, d$targets, 3)
  d$roll3_rush_ypc <- lag_roll_rate22(d$rushing_yards, d$carries, 3)
  d$roll3_pass_ypa <- lag_roll_rate22(d$passing_yards, d$pass_attempts, 3)
  d$roll3_pass_td_rate <- lag_roll_rate22(d$passing_tds, d$pass_attempts, 3)
  d$roll3_interception_rate <- lag_roll_rate22(d$passing_interceptions_22, d$pass_attempts, 3)
  d$roll3_rush_td_rate <- lag_roll_rate22(d$rushing_tds, d$carries, 3)
  d$roll3_rec_td_rate <- lag_roll_rate22(d$receiving_tds, d$targets, 3)
  d$season_to_date_catch_rate <- lag_cum_rate22(d$receptions, d$targets)
  d$season_to_date_ypt <- lag_cum_rate22(d$receiving_yards, d$targets)
  d$season_to_date_rush_ypc <- lag_cum_rate22(d$rushing_yards, d$carries)
  d$season_to_date_pass_ypa <- lag_cum_rate22(d$passing_yards, d$pass_attempts)
  d$season_to_date_pass_td_rate <- lag_cum_rate22(d$passing_tds, d$pass_attempts)
  d$season_to_date_interception_rate <- lag_cum_rate22(d$passing_interceptions_22, d$pass_attempts)
  d$season_to_date_rush_td_rate <- lag_cum_rate22(d$rushing_tds, d$carries)
  d$season_to_date_rec_td_rate <- lag_cum_rate22(d$receiving_tds, d$targets)

  d <- add_ngs_rolls22(d)
  d
}
weekly <- dplyr::bind_rows(lapply(split(weekly, interaction(weekly$season, weekly$player_id, drop = TRUE)), add_player_rolls21))

add_team_rolls21 <- function(d) {
  d <- d[order(d$week), , drop = FALSE]
  d$roll3_team_pass_attempts <- lag_roll_mean21(d$team_pass_attempts, 3)
  d$roll3_team_carries <- lag_roll_mean21(d$team_carries, 3)
  d
}
team_roll <- dplyr::bind_rows(lapply(split(team_week, interaction(team_week$season, team_week$team, drop = TRUE)), add_team_rolls21)) |>
  dplyr::select(season, week, team, roll3_team_pass_attempts, roll3_team_carries)
weekly <- weekly |> dplyr::left_join(team_roll, by = c("season", "week", "team"))
weekly <- weekly |>
  dplyr::mutate(
    expected_team_plays = dplyr::coalesce(wk_num(roll3_team_pass_attempts), 0) + dplyr::coalesce(wk_num(roll3_team_carries), 0),
    expected_team_pass_rate = wk_rate(roll3_team_pass_attempts, expected_team_plays, .56)
  )

# ------------------------------------------------------------
# Opponent-adjusted fantasy allowance by position.
# A defense is rewarded/penalized for points allowed relative to what that player
# was expected to score entering the week, not raw fantasy points alone.
# ------------------------------------------------------------
weekly <- weekly |>
  dplyr::mutate(
    expected_before_week = weekly_baseline21(preseason_prior_fppg, roll3_fppg, games_played_prior),
    defense_residual_allowed = weekly_fppg - expected_before_week
  )
def_pos_week <- weekly |>
  dplyr::filter(!is.na(opponent), opponent != "") |>
  dplyr::group_by(season, week, defense = opponent, position) |>
  dplyr::summarise(
    def_pos_residual = mean(defense_residual_allowed, na.rm = TRUE),
    def_pos_fppg_allowed = mean(weekly_fppg, na.rm = TRUE),
    .groups = "drop"
  )
add_def_pos_rolls21 <- function(d) {
  d <- d[order(d$week), , drop = FALSE]
  d$opp_pos_residual_roll4 <- lag_roll_mean21(d$def_pos_residual, WEEKLY_DEFENSE_LOOKBACK_SHORT)
  d$opp_pos_residual_roll8 <- lag_roll_mean21(d$def_pos_residual, WEEKLY_DEFENSE_LOOKBACK_LONG)
  d$opp_pos_fppg_allowed_roll4 <- lag_roll_mean21(d$def_pos_fppg_allowed, WEEKLY_DEFENSE_LOOKBACK_SHORT)
  d
}
def_pos_roll <- dplyr::bind_rows(lapply(split(def_pos_week, interaction(def_pos_week$season, def_pos_week$defense, def_pos_week$position, drop = TRUE)), add_def_pos_rolls21))

# Previous-season fallback for Week 1 / early season.
def_pos_prev <- def_pos_week |>
  dplyr::group_by(season, defense, position) |>
  dplyr::summarise(prev_resid = mean(def_pos_residual, na.rm = TRUE), prev_fppg = mean(def_pos_fppg_allowed, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(season = season + 1L)
weekly <- weekly |>
  dplyr::left_join(def_pos_roll |> dplyr::select(season, week, defense, position, opp_pos_residual_roll4, opp_pos_residual_roll8, opp_pos_fppg_allowed_roll4),
                   by = c("season", "week", "opponent" = "defense", "position")) |>
  dplyr::left_join(def_pos_prev, by = c("season", "opponent" = "defense", "position")) |>
  dplyr::mutate(
    opp_pos_residual_roll4 = dplyr::coalesce(opp_pos_residual_roll4, prev_resid, 0),
    opp_pos_residual_roll8 = dplyr::coalesce(opp_pos_residual_roll8, prev_resid, 0),
    opp_pos_fppg_allowed_roll4 = dplyr::coalesce(opp_pos_fppg_allowed_roll4, prev_fppg, preseason_prior_fppg)
  ) |>
  dplyr::select(-dplyr::any_of(c("prev_resid", "prev_fppg")))

# ------------------------------------------------------------
# PBP defensive style / efficiency matchup features.
# ------------------------------------------------------------
def_raw <- readr::read_csv("data/raw/weekly_defense_context_raw.csv", show_col_types = FALSE, progress = FALSE)
def_metrics <- c("def_pass_epa_allowed", "def_rush_epa_allowed", "def_pass_success_allowed", "def_rush_success_allowed",
                 "def_explosive_pass_rate", "def_deep_pass_rate", "def_middle_pass_rate", "def_deep_epa_allowed", "def_middle_epa_allowed",
                 "def_sack_rate", "def_qb_hit_rate", "def_redzone_pass_td_rate", "def_redzone_rush_td_rate")
for (nm in c("season", "week", "defense", def_metrics)) def_raw <- ensure_weekly_col21(def_raw, nm, 0)

add_pbp_def_rolls21 <- function(d) {
  d <- d[order(d$week), , drop = FALSE]
  for (nm in def_metrics) {
    d[[paste0(nm, "_roll4")]] <- lag_roll_mean21(d[[nm]], WEEKLY_DEFENSE_LOOKBACK_SHORT)
    if (nm %in% c("def_pass_epa_allowed", "def_rush_epa_allowed")) d[[paste0(nm, "_roll8")]] <- lag_roll_mean21(d[[nm]], WEEKLY_DEFENSE_LOOKBACK_LONG)
  }
  d
}
def_roll <- dplyr::bind_rows(lapply(split(def_raw, interaction(def_raw$season, def_raw$defense, drop = TRUE)), add_pbp_def_rolls21))
def_prev <- def_raw |>
  dplyr::group_by(season, defense) |>
  dplyr::summarise(dplyr::across(dplyr::all_of(def_metrics), ~mean(.x, na.rm = TRUE), .names = "prev_{.col}"), .groups = "drop") |>
  dplyr::mutate(season = season + 1L)
roll_cols <- grep("_roll[48]$", names(def_roll), value = TRUE)
weekly <- weekly |>
  dplyr::left_join(def_roll |> dplyr::select(season, week, defense, dplyr::all_of(roll_cols)), by = c("season", "week", "opponent" = "defense")) |>
  dplyr::left_join(def_prev, by = c("season", "opponent" = "defense"))
for (nm in def_metrics) {
  r4 <- paste0(nm, "_roll4"); prev <- paste0("prev_", nm)
  if (!r4 %in% names(weekly)) weekly[[r4]] <- NA_real_
  if (!prev %in% names(weekly)) weekly[[prev]] <- 0
  weekly[[r4]] <- dplyr::coalesce(wk_num(weekly[[r4]]), wk_num(weekly[[prev]]), 0)
  r8 <- paste0(nm, "_roll8")
  if (r8 %in% names(weekly)) weekly[[r8]] <- dplyr::coalesce(wk_num(weekly[[r8]]), wk_num(weekly[[prev]]), 0)
}
weekly <- weekly |> dplyr::select(-dplyr::starts_with("prev_def_"))

# Convenience aliases for position-specific defense residuals.
weekly$def_wr_residual_roll4 <- ifelse(weekly$position == "WR", weekly$opp_pos_residual_roll4, 0)
weekly$def_te_residual_roll4 <- ifelse(weekly$position == "TE", weekly$opp_pos_residual_roll4, 0)
for (nm in c("preseason_deep_target_rate", "preseason_middle_target_rate", "preseason_redzone_target_rate")) weekly[[nm]] <- dplyr::coalesce(wk_num(weekly[[nm]]), 0)
weekly$qb_pass_matchup <- weekly$roll3_pass_attempts * dplyr::coalesce(wk_num(weekly$def_pass_epa_allowed_roll4), 0)
weekly$qb_pressure_matchup <- weekly$roll3_pass_attempts * dplyr::coalesce(wk_num(weekly$def_sack_rate_roll4), 0)
weekly$rb_rush_matchup <- weekly$roll3_carries * dplyr::coalesce(wk_num(weekly$def_rush_epa_allowed_roll4), 0)
weekly$rb_receiving_matchup <- weekly$roll3_targets * weekly$opp_pos_residual_roll4
weekly$wr_volume_matchup <- weekly$roll3_target_share * dplyr::coalesce(wk_num(weekly$def_pass_epa_allowed_roll4), 0)
weekly$wr_deep_matchup <- weekly$preseason_deep_target_rate * dplyr::coalesce(wk_num(weekly$def_deep_epa_allowed_roll4), 0)
weekly$wr_redzone_matchup <- weekly$preseason_redzone_target_rate * dplyr::coalesce(wk_num(weekly$def_redzone_pass_td_rate_roll4), 0)
weekly$te_middle_matchup <- weekly$preseason_middle_target_rate * dplyr::coalesce(wk_num(weekly$def_middle_epa_allowed_roll4), 0)
weekly$te_redzone_matchup <- weekly$preseason_redzone_target_rate * dplyr::coalesce(wk_num(weekly$def_redzone_pass_td_rate_roll4), 0)
weekly$favorite_points <- pmax(0, wk_num(weekly$team_spread_line))
weekly$underdog_points <- pmax(0, -wk_num(weekly$team_spread_line))
weekly$favorite_rush_interaction <- weekly$favorite_points * dplyr::coalesce(wk_num(weekly$roll3_carries), 0)
weekly$underdog_target_interaction <- weekly$underdog_points * dplyr::coalesce(wk_num(weekly$roll3_targets), 0)
weekly$matchup_role_interaction <- dplyr::coalesce(wk_num(weekly$opp_pos_residual_roll4), 0) * pmax(0, dplyr::coalesce(wk_num(weekly$preseason_prior_fppg), 0))

# Injury and team skill availability are known before kickoff.
if (nrow(inj$player) > 0) weekly <- weekly |> dplyr::left_join(inj$player, by = c("season", "week", "player_id"))
if (nrow(inj$team) > 0) weekly <- weekly |> dplyr::left_join(inj$team, by = c("season", "week", "team"))
for (nm in c("injury_risk", "practice_risk", "team_skill_out_count", "team_skill_questionable_count")) {
  if (!nm %in% names(weekly)) weekly[[nm]] <- 0
  weekly[[nm]] <- dplyr::coalesce(wk_num(weekly[[nm]]), 0)
}
weekly$season_week <- wk_num(weekly$week)
weekly$total_line <- ifelse(isTRUE(WEEKLY_USE_GAME_TOTAL), dplyr::coalesce(wk_num(weekly$total_line), 44), 44)
weekly$team_spread_line <- dplyr::coalesce(wk_num(weekly$team_spread_line), 0)
weekly$implied_team_total <- dplyr::coalesce(wk_num(weekly$implied_team_total), weekly$total_line / 2)
weekly$is_home <- dplyr::coalesce(wk_num(weekly$is_home), 0)
weekly$rest_days <- dplyr::coalesce(wk_num(weekly$rest_days), 7)
weekly$favorite_points <- pmax(0, wk_num(weekly$team_spread_line))
weekly$underdog_points <- pmax(0, -wk_num(weekly$team_spread_line))
weekly$favorite_rush_interaction <- weekly$favorite_points * dplyr::coalesce(wk_num(weekly$roll3_carries), 0)
weekly$underdog_target_interaction <- weekly$underdog_points * dplyr::coalesce(wk_num(weekly$roll3_targets), 0)
weekly$matchup_role_interaction <- dplyr::coalesce(wk_num(weekly$opp_pos_residual_roll4), 0) * pmax(0, dplyr::coalesce(wk_num(weekly$preseason_prior_fppg), 0))

# Fill rolling fields with conservative pre-week defaults.
for (nm in c("prior_game_fppg", "roll3_fppg", "roll5_fppg", "season_to_date_fppg")) weekly[[nm]] <- dplyr::coalesce(wk_num(weekly[[nm]]), weekly$preseason_prior_fppg)
for (nm in c("roll3_fppg_sd", "roll3_targets", "roll5_targets", "roll3_carries", "roll5_carries", "roll3_pass_attempts", "roll5_pass_attempts",
             "roll3_target_share", "roll3_carry_share", "roll3_pass_attempt_share", "roll3_team_pass_attempts", "roll3_team_carries",
             "roll3_offense_pct", "roll5_offense_pct", "roll3_offense_snaps", "snap_trend", "opportunity_per_snap",
             "role_fppg_trend", "target_trend", "carry_trend", "expected_team_plays", "expected_team_pass_rate",
             "season_targets_prior", "season_carries_prior", "season_pass_attempts_prior",
             "roll3_catch_rate", "roll3_ypt", "roll3_rush_ypc", "roll3_pass_ypa", "roll3_pass_td_rate",
             "roll3_interception_rate", "roll3_rush_td_rate", "roll3_rec_td_rate",
             "season_to_date_catch_rate", "season_to_date_ypt", "season_to_date_rush_ypc", "season_to_date_pass_ypa",
             "season_to_date_pass_td_rate", "season_to_date_interception_rate", "season_to_date_rush_td_rate", "season_to_date_rec_td_rate",
             "ngs_available", "roll3_ngs_cpoe", "roll3_ngs_air_yards", "roll3_ngs_time_to_throw", "roll3_ngs_aggressiveness",
             "roll3_ngs_separation", "roll3_ngs_cushion", "roll3_ngs_yac_oe", "roll3_ngs_air_yard_share",
             "roll3_ngs_rush_yoe_pa", "roll3_ngs_box_rate", "roll3_ngs_time_to_los")) {
  if (!nm %in% names(weekly)) weekly[[nm]] <- 0
  weekly[[nm]] <- dplyr::coalesce(wk_num(weekly[[nm]]), 0)
}

for (nm in c("prior_catch_rate", "prior_yards_per_target", "prior_yards_per_carry", "prior_yards_per_attempt", "prior_pass_td_rate", "prior_rush_td_rate", "prior_rec_td_rate", "prior_interception_rate")) {
  if (!nm %in% names(weekly)) weekly[[nm]] <- NA_real_
}
weekly$prior_catch_rate <- dplyr::coalesce(wk_num(weekly$prior_catch_rate), .65)
weekly$prior_yards_per_target <- dplyr::coalesce(wk_num(weekly$prior_yards_per_target), 7.0)
weekly$prior_yards_per_carry <- dplyr::coalesce(wk_num(weekly$prior_yards_per_carry), 4.3)
weekly$prior_yards_per_attempt <- dplyr::coalesce(wk_num(weekly$prior_yards_per_attempt), 7.0)
weekly$prior_pass_td_rate <- dplyr::coalesce(wk_num(weekly$prior_pass_td_rate), .045)
weekly$prior_rush_td_rate <- dplyr::coalesce(wk_num(weekly$prior_rush_td_rate), .035)
weekly$prior_rec_td_rate <- dplyr::coalesce(wk_num(weekly$prior_rec_td_rate), .045)
weekly$prior_interception_rate <- dplyr::coalesce(wk_num(weekly$prior_interception_rate), .025)

weekly <- weekly |>
  dplyr::filter(position %in% POSITIONS, is.finite(weekly_fppg), !is.na(opponent), opponent != "") |>
  dplyr::arrange(season, week, position, player_display_name)

readr::write_csv(weekly, "data/processed/weekly_model_table_2_2.csv")
# Backward-compatible copy for the 2.1 app/research scripts.
readr::write_csv(weekly, "data/processed/weekly_model_table_2_1.csv")
readr::write_csv(def_pos_week, "data/processed/weekly_defense_position_results.csv")
cat("[2.2 WEEKLY DATA] 2.2 feature table complete: ", nrow(weekly), " player-weeks.\n", sep = "")
cat("[2.2 WEEKLY DATA] Seasons: ", min(weekly$season), "-", max(weekly$season), ".\n", sep = "")
