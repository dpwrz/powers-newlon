cat("[2.0 HOTFIX 1] Direct feature builder loaded: current nflverse passing_interceptions alias supported.\n")
# ============================================================
# STEP 2 - BUILD 1.2 POSITION-SPECIFIC FANTASY + CONTEXT FEATURES
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr", "janitor"))

if (!file.exists("data/raw/player_stats.csv")) stop("Run 01_download_data.R first.")

stats_raw <- readr::read_csv("data/raw/player_stats.csv", show_col_types = FALSE) |> janitor::clean_names()
team_raw <- readr::read_csv("data/raw/team_stats.csv", show_col_types = FALSE) |> janitor::clean_names()
players <- readr::read_csv("data/raw/players.csv", show_col_types = FALSE) |> janitor::clean_names()
draft <- readr::read_csv("data/raw/draft_picks.csv", show_col_types = FALSE) |> janitor::clean_names()
roster_path <- paste0("data/raw/roster_", CURRENT_SEASON, ".csv")
roster <- if (file.exists(roster_path)) readr::read_csv(roster_path, show_col_types = FALSE) |> janitor::clean_names() else data.frame()

read_context <- function(path) {
  if (!file.exists(path)) return(data.frame())
  x <- tryCatch(readr::read_csv(path, show_col_types = FALSE) |> janitor::clean_names(), error = function(e) data.frame())
  if ("empty" %in% names(x) && ncol(x) == 1) return(data.frame())
  x
}
team_context_raw <- read_context("data/raw/team_context.csv")
receiver_context_raw <- read_context("data/raw/receiver_context.csv")
team_qb_context_raw <- read_context("data/raw/team_qb_context.csv")
qb_context_raw <- read_context("data/raw/qb_context.csv")
alignment_raw <- read_context("data/external/receiver_alignment.csv")

num <- function(x) suppressWarnings(as.numeric(x))
ensure <- function(df, col, default = 0) { if (!col %in% names(df)) df[[col]] <- default; df }
safe_div <- function(a, b) ifelse(is.finite(b) & b > 0, a / b, 0)
first_non_missing <- function(x, default = NA_character_) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) default else as.character(x[1])
}

# Normalize optional player-stat fields across nflreadr releases.
# nflverse 1.5+ uses `passing_interceptions`; older cached files may use
# `interceptions`. Preserve whichever real field exists BEFORE defaults are added.
had_interceptions_col <- "interceptions" %in% names(stats_raw)
had_passing_interceptions_col <- "passing_interceptions" %in% names(stats_raw)
if (had_passing_interceptions_col) {
  stats_raw$interceptions <- num(stats_raw$passing_interceptions)
} else if (!had_interceptions_col) {
  stats_raw$interceptions <- 0
}

optional_cols <- c(
  "games", "games_played", "team", "attempts", "carries", "targets",
  "completions", "passing_yards", "passing_tds", "interceptions", "passing_interceptions",
  "rushing_yards", "rushing_tds", "receptions", "receiving_yards", "receiving_tds",
  "fumbles_lost", "rushing_fumbles_lost", "receiving_fumbles_lost", "sack_fumbles_lost",
  "two_point_conversions", "passing_2pt_conversions", "rushing_2pt_conversions", "receiving_2pt_conversions",
  "target_share", "air_yards_share", "wopr", "receiving_air_yards"
)
for (nm in optional_cols) stats_raw <- ensure(stats_raw, nm, 0)

if (!all(c("player_id", "player_display_name", "position", "season") %in% names(stats_raw))) {
  stop("Player stats are missing required identity columns.")
}
if (all(is.na(stats_raw$games) | stats_raw$games == 0) && "games_played" %in% names(stats_raw)) stats_raw$games <- stats_raw$games_played

# Fix fantasy-scoring components that have changed names over time.
stats_raw <- stats_raw |>
  dplyr::mutate(
    position = toupper(position),
    player_id = as.character(player_id),
    season = as.integer(season),
    games = num(games),
    attempts = num(attempts), carries = num(carries), targets = num(targets),
    passing_yards = num(passing_yards), passing_tds = num(passing_tds), interceptions = num(interceptions),
    rushing_yards = num(rushing_yards), rushing_tds = num(rushing_tds),
    receptions = num(receptions), receiving_yards = num(receiving_yards), receiving_tds = num(receiving_tds),
    calc_fumbles_lost = ifelse(
      num(fumbles_lost) > 0, num(fumbles_lost),
      num(rushing_fumbles_lost) + num(receiving_fumbles_lost) + num(sack_fumbles_lost)
    ),
    calc_two_pt = ifelse(
      num(two_point_conversions) > 0, num(two_point_conversions),
      num(passing_2pt_conversions) + num(rushing_2pt_conversions) + num(receiving_2pt_conversions)
    ),
    fantasy_points =
      passing_yards * SCORING$pass_yd + passing_tds * SCORING$pass_td + interceptions * SCORING$interception +
      rushing_yards * SCORING$rush_yd + rushing_tds * SCORING$rush_td + receptions * SCORING$reception +
      receiving_yards * SCORING$rec_yd + receiving_tds * SCORING$rec_td +
      calc_fumbles_lost * SCORING$fumble_lost + calc_two_pt * SCORING$two_pt
  ) |>
  dplyr::filter(position %in% POSITIONS)

# Pick a primary team for player-seasons that include more than one team row.
primary_team <- stats_raw |>
  dplyr::group_by(player_id, season) |>
  dplyr::arrange(dplyr::desc(games), .by_group = TRUE) |>
  dplyr::summarise(team = first_non_missing(team), .groups = "drop")

stats <- stats_raw |>
  dplyr::group_by(player_id, player_display_name, position, season) |>
  dplyr::summarise(
    games = pmin(18, sum(games, na.rm = TRUE)),
    fantasy_points = sum(fantasy_points, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE), carries = sum(carries, na.rm = TRUE), targets = sum(targets, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE), passing_tds = sum(passing_tds, na.rm = TRUE), interceptions = sum(interceptions, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE), rushing_tds = sum(rushing_tds, na.rm = TRUE),
    receptions = sum(receptions, na.rm = TRUE), receiving_yards = sum(receiving_yards, na.rm = TRUE), receiving_tds = sum(receiving_tds, na.rm = TRUE),
    target_share = max(num(target_share), na.rm = TRUE),
    air_yards_share = max(num(air_yards_share), na.rm = TRUE),
    wopr = max(num(wopr), na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(primary_team, by = c("player_id", "season")) |>
  dplyr::mutate(
    dplyr::across(c(target_share, air_yards_share, wopr), ~ ifelse(is.finite(.x), .x, 0)),
    fppg = safe_div(fantasy_points, games),
    targets_pg = safe_div(targets, games), carries_pg = safe_div(carries, games), pass_attempts_pg = safe_div(attempts, games),
    rec_yd_pg = safe_div(receiving_yards, games), rush_yd_pg = safe_div(rushing_yards, games), pass_yd_pg = safe_div(passing_yards, games),
    catch_rate = safe_div(receptions, targets), yards_per_target = safe_div(receiving_yards, targets),
    yards_per_carry = safe_div(rushing_yards, carries), yards_per_attempt = safe_div(passing_yards, attempts),
    pass_td_rate = safe_div(passing_tds, attempts), rush_td_rate = safe_div(rushing_tds, carries), rec_td_rate = safe_div(receiving_tds, targets)
  )

# Player metadata: real age, size and rookie season.
player_meta <- if (nrow(players) > 0 && "gsis_id" %in% names(players)) {
  players |>
    dplyr::transmute(
      player_id = as.character(gsis_id),
      birth_date = suppressWarnings(as.Date(birth_date)),
      height = num(height), weight = num(weight),
      rookie_season = if ("rookie_season" %in% names(players)) num(rookie_season) else NA_real_,
      latest_team_meta = if ("latest_team" %in% names(players)) as.character(latest_team) else NA_character_
    ) |>
    dplyr::distinct(player_id, .keep_all = TRUE)
} else data.frame(player_id = character())

# Draft capital by GSIS ID.
draft_keep <- if (nrow(draft) > 0 && all(c("gsis_id", "round", "pick") %in% names(draft))) {
  draft |>
    dplyr::transmute(
      player_id = as.character(gsis_id),
      draft_round = num(round), draft_pick = num(pick)
    ) |>
    dplyr::filter(!is.na(player_id), player_id != "") |>
    dplyr::distinct(player_id, .keep_all = TRUE)
} else data.frame(player_id = character(), draft_round = numeric(), draft_pick = numeric())

# Team environment, lagged one year by team. load_team_stats is tiny compared
# with PBP and gives us offense context without a large mobile download.
team_features <- data.frame()
if (nrow(team_raw) > 0 && all(c("season", "team") %in% names(team_raw))) {
  for (nm in c("attempts", "carries", "passing_yards", "rushing_yards", "passing_tds", "rushing_tds")) {
    team_raw <- ensure(team_raw, nm, 0)
  }

  team_features <- team_raw |>
    dplyr::mutate(
      season = as.integer(season), team = as.character(team),
      games = ifelse(season >= 2021, 17, 16),
      team_prior_pass_yd_pg = safe_div(num(passing_yards), games),
      team_prior_rush_yd_pg = safe_div(num(rushing_yards), games),
      team_prior_pass_attempts_pg = safe_div(num(attempts), games),
      team_prior_carries_pg = safe_div(num(carries), games),
      team_prior_points_pg = safe_div(
        num(passing_yards) * SCORING$pass_yd + num(passing_tds) * SCORING$pass_td +
          num(rushing_yards) * SCORING$rush_yd + num(rushing_tds) * SCORING$rush_td,
        games
      ),
      target_season = season + 1
    ) |>
    dplyr::select(target_season, team, dplyr::starts_with("team_prior_")) |>
    dplyr::distinct(target_season, team, .keep_all = TRUE)
}

# Detailed 1.2 context tables (reusing the 1.1 compact PBP feature store) are stored at source season and shifted forward one
# year so every model row only sees information that existed before its target season.
team_context_features <- data.frame()
if (nrow(team_context_raw) > 0 && all(c("season", "team") %in% names(team_context_raw))) {
  team_context_features <- team_context_raw |>
    dplyr::mutate(target_season = as.integer(season) + 1, team = as.character(team)) |>
    dplyr::transmute(
      target_season, team,
      team_prior_pass_rate = num(team_pass_rate),
      team_prior_rush_rate = pmax(0, pmin(1, 1 - num(team_pass_rate))),
      team_prior_neutral_pass_rate = num(team_neutral_pass_rate),
      team_prior_neutral_rush_rate = pmax(0, pmin(1, 1 - num(team_neutral_pass_rate))),
      team_prior_plays_pg = num(team_plays_pg),
      team_prior_redzone_pass_rate = num(team_redzone_pass_rate),
      team_prior_redzone_rush_rate = pmax(0, pmin(1, 1 - num(team_redzone_pass_rate))),
      team_prior_deep_throw_rate = num(team_deep_throw_rate),
      team_prior_left_throw_rate = num(team_left_throw_rate),
      team_prior_middle_throw_rate = num(team_middle_throw_rate),
      team_prior_right_throw_rate = num(team_right_throw_rate),
      team_prior_shotgun_rate = num(team_shotgun_rate),
      team_prior_no_huddle_rate = num(team_no_huddle_rate),
      team_prior_avg_air_yards = num(team_avg_air_yards)
    ) |>
    dplyr::distinct(target_season, team, .keep_all = TRUE)
}

receiver_context_features <- data.frame()
if (nrow(receiver_context_raw) > 0 && all(c("season", "player_id") %in% names(receiver_context_raw))) {
  needed <- c(
    "rec_context_targets", "rec_avg_depth_target", "rec_deep_target_rate", "rec_short_target_rate",
    "rec_left_target_rate", "rec_middle_target_rate", "rec_right_target_rate", "rec_redzone_target_rate",
    "rec_route_vertical", "rec_route_in_break", "rec_route_out_break", "rec_route_screen", "rec_route_hitch",
    "rec_ngs_avg_cushion", "rec_ngs_avg_separation"
  )
  for (nm in needed) receiver_context_raw <- ensure(receiver_context_raw, nm, 0)
  receiver_context_features <- receiver_context_raw |>
    dplyr::mutate(target_season = as.integer(season) + 1, player_id = as.character(player_id)) |>
    dplyr::transmute(
      target_season, player_id,
      receiver_context_available = as.numeric(num(rec_context_targets) > 0),
      rec_prior_avg_depth_target = num(rec_avg_depth_target),
      rec_prior_deep_target_rate = num(rec_deep_target_rate),
      rec_prior_short_target_rate = num(rec_short_target_rate),
      rec_prior_left_target_rate = num(rec_left_target_rate),
      rec_prior_middle_target_rate = num(rec_middle_target_rate),
      rec_prior_right_target_rate = num(rec_right_target_rate),
      rec_prior_redzone_target_rate = num(rec_redzone_target_rate),
      rec_prior_route_vertical = num(rec_route_vertical),
      rec_prior_route_in_break = num(rec_route_in_break),
      rec_prior_route_out_break = num(rec_route_out_break),
      rec_prior_route_screen = num(rec_route_screen),
      rec_prior_route_hitch = num(rec_route_hitch),
      rec_prior_ngs_avg_cushion = num(rec_ngs_avg_cushion),
      rec_prior_ngs_avg_separation = num(rec_ngs_avg_separation)
    ) |>
    dplyr::distinct(target_season, player_id, .keep_all = TRUE)
}

alignment_features <- data.frame()
if (nrow(alignment_raw) > 0 && all(c("season", "player_id") %in% names(alignment_raw))) {
  for (nm in c("slot_rate", "wide_rate", "inline_rate")) alignment_raw <- ensure(alignment_raw, nm, 0)
  alignment_features <- alignment_raw |>
    dplyr::mutate(target_season = as.integer(season) + 1, player_id = as.character(player_id)) |>
    dplyr::transmute(
      target_season, player_id,
      rec_prior_slot_rate = num(slot_rate),
      rec_prior_wide_rate = num(wide_rate),
      rec_prior_inline_rate = num(inline_rate),
      alignment_available = 1
    ) |>
    dplyr::distinct(target_season, player_id, .keep_all = TRUE)
}

team_qb_context_features <- data.frame()
if (nrow(team_qb_context_raw) > 0 && all(c("season", "team") %in% names(team_qb_context_raw))) {
  needed <- c(
    "primary_qb_id", "qb_dropbacks", "qb_avg_air_yards", "qb_deep_throw_rate", "qb_left_throw_rate",
    "qb_middle_throw_rate", "qb_right_throw_rate", "qb_redzone_throw_rate",
    "qb_target_pos_wr", "qb_target_pos_te", "qb_target_pos_rb",
    "qb_route_vertical", "qb_route_in_break", "qb_route_out_break", "qb_route_screen", "qb_route_hitch",
    "qb_ngs_time_to_throw", "qb_ngs_aggressiveness", "qb_ngs_cpoe"
  )
  for (nm in needed) team_qb_context_raw <- ensure(team_qb_context_raw, nm, 0)
  team_qb_context_features <- team_qb_context_raw |>
    dplyr::mutate(target_season = as.integer(season) + 1, team = as.character(team)) |>
    dplyr::transmute(
      target_season, team, prior_primary_qb_id = as.character(primary_qb_id),
      qb_context_available = as.numeric(num(qb_dropbacks) > 0),
      qb_prior_avg_air_yards = num(qb_avg_air_yards),
      qb_prior_deep_throw_rate = num(qb_deep_throw_rate),
      qb_prior_left_throw_rate = num(qb_left_throw_rate),
      qb_prior_middle_throw_rate = num(qb_middle_throw_rate),
      qb_prior_right_throw_rate = num(qb_right_throw_rate),
      qb_prior_redzone_throw_rate = num(qb_redzone_throw_rate),
      qb_prior_wr_target_rate = num(qb_target_pos_wr),
      qb_prior_te_target_rate = num(qb_target_pos_te),
      qb_prior_rb_target_rate = num(qb_target_pos_rb),
      qb_prior_route_vertical = num(qb_route_vertical),
      qb_prior_route_in_break = num(qb_route_in_break),
      qb_prior_route_out_break = num(qb_route_out_break),
      qb_prior_route_screen = num(qb_route_screen),
      qb_prior_route_hitch = num(qb_route_hitch),
      qb_prior_ngs_time_to_throw = num(qb_ngs_time_to_throw),
      qb_prior_ngs_aggressiveness = num(qb_ngs_aggressiveness),
      qb_prior_ngs_cpoe = num(qb_ngs_cpoe)
    ) |>
    dplyr::distinct(target_season, team, .keep_all = TRUE)
}

# Player-specific QB context is separate from the previous team's primary-QB profile.
# This lets QB projections learn the quarterback's own depth/NGS/target tendencies
# while the team-level join continues to describe the offensive environment.
player_qb_context_features <- data.frame()
if (nrow(qb_context_raw) > 0 && all(c("season", "player_id") %in% names(qb_context_raw))) {
  needed <- c(
    "qb_dropbacks", "qb_avg_air_yards", "qb_deep_throw_rate", "qb_left_throw_rate",
    "qb_middle_throw_rate", "qb_right_throw_rate", "qb_redzone_throw_rate",
    "qb_target_pos_wr", "qb_target_pos_te", "qb_target_pos_rb",
    "qb_route_vertical", "qb_route_in_break", "qb_route_out_break", "qb_route_screen", "qb_route_hitch",
    "qb_ngs_time_to_throw", "qb_ngs_aggressiveness", "qb_ngs_cpoe"
  )
  for (nm in needed) qb_context_raw <- ensure(qb_context_raw, nm, 0)
  player_qb_context_features <- qb_context_raw |>
    dplyr::mutate(target_season = as.integer(season) + 1, player_id = as.character(player_id)) |>
    dplyr::group_by(target_season, player_id) |>
    dplyr::arrange(dplyr::desc(num(qb_dropbacks)), .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      target_season, player_id,
      player_qb_context_available = as.numeric(num(qb_dropbacks) > 0),
      player_qb_prior_avg_air_yards = num(qb_avg_air_yards),
      player_qb_prior_deep_throw_rate = num(qb_deep_throw_rate),
      player_qb_prior_left_throw_rate = num(qb_left_throw_rate),
      player_qb_prior_middle_throw_rate = num(qb_middle_throw_rate),
      player_qb_prior_right_throw_rate = num(qb_right_throw_rate),
      player_qb_prior_redzone_throw_rate = num(qb_redzone_throw_rate),
      player_qb_prior_wr_target_rate = num(qb_target_pos_wr),
      player_qb_prior_te_target_rate = num(qb_target_pos_te),
      player_qb_prior_rb_target_rate = num(qb_target_pos_rb),
      player_qb_prior_route_vertical = num(qb_route_vertical),
      player_qb_prior_route_in_break = num(qb_route_in_break),
      player_qb_prior_route_out_break = num(qb_route_out_break),
      player_qb_prior_route_screen = num(qb_route_screen),
      player_qb_prior_route_hitch = num(qb_route_hitch),
      player_qb_prior_ngs_time_to_throw = num(qb_ngs_time_to_throw),
      player_qb_prior_ngs_aggressiveness = num(qb_ngs_aggressiveness),
      player_qb_prior_ngs_cpoe = num(qb_ngs_cpoe)
    ) |>
    dplyr::distinct(target_season, player_id, .keep_all = TRUE)
}

context_feature_names <- c(
  "team_prior_pass_rate", "team_prior_rush_rate", "team_prior_neutral_pass_rate", "team_prior_neutral_rush_rate", "team_prior_plays_pg",
  "team_prior_redzone_pass_rate", "team_prior_redzone_rush_rate", "team_prior_deep_throw_rate", "team_prior_left_throw_rate",
  "team_prior_middle_throw_rate", "team_prior_right_throw_rate", "team_prior_shotgun_rate",
  "team_prior_no_huddle_rate", "team_prior_avg_air_yards",
  "rec_prior_avg_depth_target", "rec_prior_deep_target_rate", "rec_prior_short_target_rate",
  "rec_prior_left_target_rate", "rec_prior_middle_target_rate", "rec_prior_right_target_rate",
  "rec_prior_redzone_target_rate", "rec_prior_route_vertical", "rec_prior_route_in_break",
  "rec_prior_route_out_break", "rec_prior_route_screen", "rec_prior_route_hitch",
  "rec_prior_ngs_avg_cushion", "rec_prior_ngs_avg_separation",
  "rec_prior_slot_rate", "rec_prior_wide_rate", "rec_prior_inline_rate",
  "qb_prior_avg_air_yards", "qb_prior_deep_throw_rate", "qb_prior_left_throw_rate",
  "qb_prior_middle_throw_rate", "qb_prior_right_throw_rate", "qb_prior_redzone_throw_rate",
  "qb_prior_wr_target_rate", "qb_prior_te_target_rate", "qb_prior_rb_target_rate",
  "qb_prior_route_vertical", "qb_prior_route_in_break", "qb_prior_route_out_break",
  "qb_prior_route_screen", "qb_prior_route_hitch", "qb_prior_ngs_time_to_throw",
  "qb_prior_ngs_aggressiveness", "qb_prior_ngs_cpoe",
  "player_qb_prior_avg_air_yards", "player_qb_prior_deep_throw_rate", "player_qb_prior_left_throw_rate",
  "player_qb_prior_middle_throw_rate", "player_qb_prior_right_throw_rate", "player_qb_prior_redzone_throw_rate",
  "player_qb_prior_wr_target_rate", "player_qb_prior_te_target_rate", "player_qb_prior_rb_target_rate",
  "player_qb_prior_route_vertical", "player_qb_prior_route_in_break", "player_qb_prior_route_out_break",
  "player_qb_prior_route_screen", "player_qb_prior_route_hitch",
  "player_qb_prior_ngs_time_to_throw", "player_qb_prior_ngs_aggressiveness", "player_qb_prior_ngs_cpoe"
)

add_context_fit_features <- function(df) {
  df <- ensure(df, "prior_primary_qb_id", NA_character_)
  for (nm in context_feature_names) {
    df <- ensure(df, nm, 0)
    df[[nm]] <- num(df[[nm]])
    df[[nm]][!is.finite(df[[nm]])] <- 0
  }
  for (nm in c("alignment_available", "receiver_context_available", "qb_context_available", "player_qb_context_available")) {
    df <- ensure(df, nm, 0)
    df[[nm]] <- num(df[[nm]])
    df[[nm]][!is.finite(df[[nm]])] <- 0
  }
  df |>
    dplyr::mutate(
      context_available = as.numeric(team_prior_plays_pg > 0),
      context_fit_available = position %in% c("RB", "WR", "TE") & receiver_context_available > 0 & qb_context_available > 0,
      qb_receiver_depth_fit = ifelse(context_fit_available, pmax(0, pmin(1, 1 - abs(rec_prior_deep_target_rate - qb_prior_deep_throw_rate))), 0),
      qb_receiver_location_fit = ifelse(context_fit_available, pmax(0, pmin(1,
        1 - (abs(rec_prior_left_target_rate - qb_prior_left_throw_rate) +
             abs(rec_prior_middle_target_rate - qb_prior_middle_throw_rate) +
             abs(rec_prior_right_target_rate - qb_prior_right_throw_rate)) / 2
      )), 0),
      qb_receiver_route_fit = ifelse(context_fit_available, pmax(0, pmin(1,
        1 - (abs(rec_prior_route_vertical - qb_prior_route_vertical) +
             abs(rec_prior_route_in_break - qb_prior_route_in_break) +
             abs(rec_prior_route_out_break - qb_prior_route_out_break) +
             abs(rec_prior_route_screen - qb_prior_route_screen) +
             abs(rec_prior_route_hitch - qb_prior_route_hitch)) / 2
      )), 0),
      qb_position_target_fit = dplyr::case_when(
        position == "WR" ~ qb_prior_wr_target_rate,
        position == "TE" ~ qb_prior_te_target_rate,
        position == "RB" ~ qb_prior_rb_target_rate,
        TRUE ~ 0
      ),
      team_target_opportunity = pmax(0, team_prior_pass_attempts_pg * prior_target_share),
      rb_prior_team_carry_share = pmax(0, pmin(1.5, safe_div(prior_carries_pg, team_prior_carries_pg))),
      rb_prior_touch_opportunity = pmax(0, prior_carries_pg + prior_targets_pg),
      rb_receiving_fit = pmax(0, pmin(1, 0.65 * qb_prior_rb_target_rate + 0.35 * rec_prior_short_target_rate)),
      wr_context_fit_score = pmax(0, pmin(1,
        0.30 * qb_receiver_depth_fit + 0.25 * qb_receiver_location_fit +
        0.25 * qb_receiver_route_fit + 0.20 * qb_prior_wr_target_rate
      )),
      te_middle_fit = ifelse(context_fit_available, pmax(0, pmin(1, 1 - abs(rec_prior_middle_target_rate - qb_prior_middle_throw_rate))), 0),
      te_redzone_fit = ifelse(context_fit_available, pmax(0, pmin(1, 1 - abs(rec_prior_redzone_target_rate - qb_prior_redzone_throw_rate))), 0),
      te_route_fit = ifelse(context_fit_available, pmax(0, pmin(1,
        1 - (abs(rec_prior_route_in_break - qb_prior_route_in_break) +
             abs(rec_prior_route_hitch - qb_prior_route_hitch) +
             abs(rec_prior_route_screen - qb_prior_route_screen)) / 1.5
      )), 0),
      te_context_fit_score = pmax(0, pmin(1,
        0.30 * te_middle_fit + 0.25 * te_redzone_fit + 0.25 * te_route_fit + 0.20 * qb_prior_te_target_rate
      )),
      qb_team_continuity = as.numeric(position == "QB" & !is.na(prior_primary_qb_id) & player_id == prior_primary_qb_id)
    ) |>
    dplyr::select(-context_fit_available)
}

# Build pre-season features for every historical player-season, INCLUDING rookies.
features <- stats |>
  dplyr::left_join(player_meta, by = "player_id") |>
  dplyr::left_join(draft_keep, by = "player_id") |>
  dplyr::arrange(player_id, season) |>
  dplyr::group_by(player_id) |>
  dplyr::mutate(
    prior_season_raw = dplyr::lag(season, 1),
    two_year_season_raw = dplyr::lag(season, 2),
    prior_fppg_raw = dplyr::lag(fppg, 1), two_year_fppg_raw = dplyr::lag(fppg, 2),
    prior_fantasy_points_raw = dplyr::lag(fantasy_points, 1),
    prior_games_raw = dplyr::lag(games, 1), two_year_games_raw = dplyr::lag(games, 2),
    prior_targets_pg_raw = dplyr::lag(targets_pg, 1), two_year_targets_pg_raw = dplyr::lag(targets_pg, 2),
    prior_carries_pg_raw = dplyr::lag(carries_pg, 1), two_year_carries_pg_raw = dplyr::lag(carries_pg, 2),
    prior_pass_attempts_pg_raw = dplyr::lag(pass_attempts_pg, 1), two_year_pass_attempts_pg_raw = dplyr::lag(pass_attempts_pg, 2),
    prior_rec_yd_pg_raw = dplyr::lag(rec_yd_pg, 1), prior_rush_yd_pg_raw = dplyr::lag(rush_yd_pg, 1), prior_pass_yd_pg_raw = dplyr::lag(pass_yd_pg, 1),
    prior_catch_rate_raw = dplyr::lag(catch_rate, 1), prior_yards_per_target_raw = dplyr::lag(yards_per_target, 1),
    prior_yards_per_carry_raw = dplyr::lag(yards_per_carry, 1), prior_yards_per_attempt_raw = dplyr::lag(yards_per_attempt, 1),
    prior_target_share_raw = dplyr::lag(target_share, 1), prior_air_yards_share_raw = dplyr::lag(air_yards_share, 1), prior_wopr_raw = dplyr::lag(wopr, 1),
    prior_pass_td_rate_raw = dplyr::lag(pass_td_rate, 1), prior_rush_td_rate_raw = dplyr::lag(rush_td_rate, 1), prior_rec_td_rate_raw = dplyr::lag(rec_td_rate, 1),
    career_fppg_before = dplyr::lag(dplyr::cummean(fppg), 1),
    peak_fppg_before = dplyr::lag(cummax(fppg), 1),
    career_games_before = dplyr::lag(cumsum(games), 1),
    seasons_before = dplyr::row_number() - 1
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    consecutive_prior = !is.na(prior_season_raw) & prior_season_raw == season - 1,
    consecutive_two_year = !is.na(two_year_season_raw) & two_year_season_raw == season - 2,
    prior_fppg = ifelse(consecutive_prior, prior_fppg_raw, 0),
    two_year_fppg = ifelse(consecutive_two_year, two_year_fppg_raw, 0),
    prior_fantasy_points = ifelse(consecutive_prior, prior_fantasy_points_raw, 0),
    prior_games = ifelse(consecutive_prior, prior_games_raw, 0), two_year_games = ifelse(consecutive_two_year, two_year_games_raw, 0),
    prior_targets_pg = ifelse(consecutive_prior, prior_targets_pg_raw, 0), two_year_targets_pg = ifelse(consecutive_two_year, two_year_targets_pg_raw, 0),
    prior_carries_pg = ifelse(consecutive_prior, prior_carries_pg_raw, 0), two_year_carries_pg = ifelse(consecutive_two_year, two_year_carries_pg_raw, 0),
    prior_pass_attempts_pg = ifelse(consecutive_prior, prior_pass_attempts_pg_raw, 0), two_year_pass_attempts_pg = ifelse(consecutive_two_year, two_year_pass_attempts_pg_raw, 0),
    prior_rec_yd_pg = ifelse(consecutive_prior, prior_rec_yd_pg_raw, 0), prior_rush_yd_pg = ifelse(consecutive_prior, prior_rush_yd_pg_raw, 0), prior_pass_yd_pg = ifelse(consecutive_prior, prior_pass_yd_pg_raw, 0),
    prior_catch_rate = ifelse(consecutive_prior, prior_catch_rate_raw, 0), prior_yards_per_target = ifelse(consecutive_prior, prior_yards_per_target_raw, 0),
    prior_yards_per_carry = ifelse(consecutive_prior, prior_yards_per_carry_raw, 0), prior_yards_per_attempt = ifelse(consecutive_prior, prior_yards_per_attempt_raw, 0),
    prior_target_share = ifelse(consecutive_prior, prior_target_share_raw, 0), prior_air_yards_share = ifelse(consecutive_prior, prior_air_yards_share_raw, 0), prior_wopr = ifelse(consecutive_prior, prior_wopr_raw, 0),
    prior_pass_td_rate = ifelse(consecutive_prior, prior_pass_td_rate_raw, 0), prior_rush_td_rate = ifelse(consecutive_prior, prior_rush_td_rate_raw, 0), prior_rec_td_rate = ifelse(consecutive_prior, prior_rec_td_rate_raw, 0),
    recent_weighted_fppg = ifelse(consecutive_prior, 0.70 * prior_fppg + 0.30 * ifelse(consecutive_two_year, two_year_fppg, prior_fppg), 0),
    fppg_trend = ifelse(consecutive_prior & consecutive_two_year, prior_fppg - two_year_fppg, 0),
    experience = pmax(0, ifelse(is.finite(rookie_season), season - rookie_season, seasons_before)),
    is_rookie = as.numeric(ifelse(is.finite(rookie_season), season == rookie_season, seasons_before == 0)),
    years_since_last_season = ifelse(is.na(prior_season_raw), 0, pmax(0, season - prior_season_raw - 1)),
    age = ifelse(!is.na(birth_date), as.numeric(as.Date(paste0(season, "-09-01")) - birth_date) / 365.25, 0),
    age_squared = age^2,
    draft_round = tidyr::replace_na(draft_round, 8),
    draft_pick = tidyr::replace_na(draft_pick, 300),
    draft_capital_score = 1 / sqrt(pmax(1, draft_pick)),
    career_fppg_before = tidyr::replace_na(career_fppg_before, 0),
    peak_fppg_before = tidyr::replace_na(peak_fppg_before, 0),
    career_games_before = tidyr::replace_na(career_games_before, 0),
    target_fppg = fppg,
    target_fantasy_points = fantasy_points
  )

if (nrow(team_features) > 0) {
  features <- features |> dplyr::left_join(team_features, by = c("season" = "target_season", "team" = "team"))
}
for (nm in c("team_prior_pass_yd_pg", "team_prior_rush_yd_pg", "team_prior_pass_attempts_pg", "team_prior_carries_pg", "team_prior_points_pg")) {
  features <- ensure(features, nm, 0)
  features[[nm]][!is.finite(features[[nm]])] <- 0
}
if (nrow(team_context_features) > 0) {
  features <- features |> dplyr::left_join(team_context_features, by = c("season" = "target_season", "team" = "team"))
}
if (nrow(receiver_context_features) > 0) {
  features <- features |> dplyr::left_join(receiver_context_features, by = c("season" = "target_season", "player_id" = "player_id"))
}
if (nrow(team_qb_context_features) > 0) {
  features <- features |> dplyr::left_join(team_qb_context_features, by = c("season" = "target_season", "team" = "team"))
}
if (nrow(player_qb_context_features) > 0) {
  features <- features |> dplyr::left_join(player_qb_context_features, by = c("season" = "target_season", "player_id" = "player_id"))
}
if (nrow(alignment_features) > 0) {
  features <- features |> dplyr::left_join(alignment_features, by = c("season" = "target_season", "player_id" = "player_id"))
}
features <- add_context_fit_features(features)

model_table <- features |>
  dplyr::filter(season >= TRAIN_START, season <= TRAIN_END, games >= MIN_GAMES)
readr::write_csv(model_table, "data/processed/model_table.csv")

latest_training_season <- as.integer(readLines("data/raw/latest_training_season.txt", warn = FALSE)[1])
latest_player <- features |>
  dplyr::filter(season == latest_training_season) |>
  dplyr::select(player_id, player_display_name, position, source_season = season, dplyr::everything())

# Current roster lets 1.2 include 2026 rookies and players who changed teams.
if (nrow(roster) > 0 && "gsis_id" %in% names(roster)) {
  for (nm in c("team", "position", "status", "full_name", "birth_date", "height", "weight", "years_exp")) roster <- ensure(roster, nm, NA)
  candidates <- roster |>
    dplyr::transmute(
      player_id = as.character(gsis_id),
      roster_name = as.character(full_name),
      position = toupper(as.character(position)),
      current_team = as.character(team), current_status = as.character(status),
      roster_birth_date = suppressWarnings(as.Date(birth_date)),
      roster_height = num(height), roster_weight = num(weight), roster_years_exp = num(years_exp)
    ) |>
    dplyr::filter(
      position %in% POSITIONS, !is.na(player_id), player_id != "",
      !is.na(current_team), current_team != "",
      is.na(current_status) | current_status %in% PROJECTABLE_ROSTER_STATUSES
    ) |>
    dplyr::mutate(
      status_priority = dplyr::case_when(
        current_status == "ACT" ~ 1,
        current_status == "INA" ~ 2,
        current_status %in% c("PUP", "RES", "SUS", "EXE", "E14") ~ 3,
        TRUE ~ 9
      )
    ) |>
    dplyr::arrange(status_priority) |>
    dplyr::distinct(player_id, .keep_all = TRUE) |>
    dplyr::select(-status_priority)
} else {
  candidates <- latest_player |>
    dplyr::transmute(
      player_id, roster_name = player_display_name, position,
      current_team = team, current_status = NA_character_,
      roster_birth_date = as.Date(NA), roster_height = NA_real_, roster_weight = NA_real_, roster_years_exp = NA_real_
    )
}

hist_for_projection <- features |>
  dplyr::filter(season == latest_training_season) |>
  dplyr::transmute(
    player_id,
    hist_name = player_display_name,
    source_season = season,
    hist_fppg = fppg, hist_fantasy_points = fantasy_points, hist_games = games,
    hist_targets_pg = targets_pg, hist_carries_pg = carries_pg, hist_pass_attempts_pg = pass_attempts_pg,
    hist_rec_yd_pg = rec_yd_pg, hist_rush_yd_pg = rush_yd_pg, hist_pass_yd_pg = pass_yd_pg,
    hist_catch_rate = catch_rate, hist_yards_per_target = yards_per_target, hist_yards_per_carry = yards_per_carry, hist_yards_per_attempt = yards_per_attempt,
    hist_target_share = target_share, hist_air_yards_share = air_yards_share, hist_wopr = wopr,
    hist_pass_td_rate = pass_td_rate, hist_rush_td_rate = rush_td_rate, hist_rec_td_rate = rec_td_rate,
    hist_career_fppg = (career_fppg_before * seasons_before + fppg) / pmax(1, seasons_before + 1),
    hist_peak_fppg = pmax(peak_fppg_before, fppg),
    hist_career_games = career_games_before + games,
    hist_two_year_fppg = prior_fppg,
    hist_two_year_games = prior_games,
    hist_two_year_targets_pg = prior_targets_pg,
    hist_two_year_carries_pg = prior_carries_pg,
    hist_two_year_pass_attempts_pg = prior_pass_attempts_pg,
    hist_experience = experience + 1,
    hist_prior_season_raw = season
  )

projection_table <- candidates |>
  dplyr::left_join(hist_for_projection, by = "player_id") |>
  dplyr::left_join(player_meta, by = "player_id", suffix = c("", "_meta")) |>
  dplyr::left_join(draft_keep, by = "player_id") |>
  dplyr::mutate(
    player_display_name = dplyr::coalesce(roster_name, hist_name),
    prior_fppg = tidyr::replace_na(hist_fppg, 0),
    two_year_fppg = tidyr::replace_na(hist_two_year_fppg, 0),
    recent_weighted_fppg = ifelse(prior_fppg > 0, 0.70 * prior_fppg + 0.30 * ifelse(two_year_fppg > 0, two_year_fppg, prior_fppg), 0),
    fppg_trend = ifelse(prior_fppg > 0 & two_year_fppg > 0, prior_fppg - two_year_fppg, 0),
    prior_fantasy_points = tidyr::replace_na(hist_fantasy_points, 0),
    prior_games = tidyr::replace_na(hist_games, 0), two_year_games = tidyr::replace_na(hist_two_year_games, 0),
    prior_targets_pg = tidyr::replace_na(hist_targets_pg, 0), two_year_targets_pg = tidyr::replace_na(hist_two_year_targets_pg, 0),
    prior_carries_pg = tidyr::replace_na(hist_carries_pg, 0), two_year_carries_pg = tidyr::replace_na(hist_two_year_carries_pg, 0),
    prior_pass_attempts_pg = tidyr::replace_na(hist_pass_attempts_pg, 0), two_year_pass_attempts_pg = tidyr::replace_na(hist_two_year_pass_attempts_pg, 0),
    prior_rec_yd_pg = tidyr::replace_na(hist_rec_yd_pg, 0), prior_rush_yd_pg = tidyr::replace_na(hist_rush_yd_pg, 0), prior_pass_yd_pg = tidyr::replace_na(hist_pass_yd_pg, 0),
    prior_catch_rate = tidyr::replace_na(hist_catch_rate, 0), prior_yards_per_target = tidyr::replace_na(hist_yards_per_target, 0),
    prior_yards_per_carry = tidyr::replace_na(hist_yards_per_carry, 0), prior_yards_per_attempt = tidyr::replace_na(hist_yards_per_attempt, 0),
    prior_target_share = tidyr::replace_na(hist_target_share, 0), prior_air_yards_share = tidyr::replace_na(hist_air_yards_share, 0), prior_wopr = tidyr::replace_na(hist_wopr, 0),
    prior_pass_td_rate = tidyr::replace_na(hist_pass_td_rate, 0), prior_rush_td_rate = tidyr::replace_na(hist_rush_td_rate, 0), prior_rec_td_rate = tidyr::replace_na(hist_rec_td_rate, 0),
    career_fppg_before = tidyr::replace_na(hist_career_fppg, 0), peak_fppg_before = tidyr::replace_na(hist_peak_fppg, 0), career_games_before = tidyr::replace_na(hist_career_games, 0),
    experience = dplyr::coalesce(roster_years_exp, hist_experience, ifelse(is.finite(rookie_season), CURRENT_SEASON - rookie_season, 0)),
    is_rookie = as.numeric(
      (!is.na(roster_years_exp) & roster_years_exp == 0) |
      (!is.na(rookie_season) & rookie_season == CURRENT_SEASON) |
      (is.na(roster_years_exp) & is.na(rookie_season) & is.na(source_season))
    ),
    years_since_last_season = ifelse(is.na(source_season), 0, pmax(0, CURRENT_SEASON - source_season - 1)),
    birth_date_final = dplyr::coalesce(roster_birth_date, birth_date),
    age = ifelse(!is.na(birth_date_final), as.numeric(as.Date(paste0(CURRENT_SEASON, "-09-01")) - birth_date_final) / 365.25, 0),
    age_squared = age^2,
    height = dplyr::coalesce(roster_height, height), weight = dplyr::coalesce(roster_weight, weight),
    draft_round = tidyr::replace_na(draft_round, 8), draft_pick = tidyr::replace_na(draft_pick, 300),
    draft_capital_score = 1 / sqrt(pmax(1, draft_pick))
  )

if (nrow(team_features) > 0) {
  current_team_features <- team_features |>
    dplyr::filter(target_season == CURRENT_SEASON) |>
    dplyr::select(-target_season)
  projection_table <- projection_table |>
    dplyr::left_join(current_team_features, by = c("current_team" = "team"))
}
for (nm in c("team_prior_pass_yd_pg", "team_prior_rush_yd_pg", "team_prior_pass_attempts_pg", "team_prior_carries_pg", "team_prior_points_pg")) {
  projection_table <- ensure(projection_table, nm, 0)
  projection_table[[nm]][!is.finite(projection_table[[nm]])] <- 0
}
if (nrow(team_context_features) > 0) {
  current_team_context <- team_context_features |> dplyr::filter(target_season == CURRENT_SEASON) |> dplyr::select(-target_season)
  projection_table <- projection_table |> dplyr::left_join(current_team_context, by = c("current_team" = "team"))
}
if (nrow(receiver_context_features) > 0) {
  current_receiver_context <- receiver_context_features |> dplyr::filter(target_season == CURRENT_SEASON) |> dplyr::select(-target_season)
  projection_table <- projection_table |> dplyr::left_join(current_receiver_context, by = "player_id")
}
if (nrow(team_qb_context_features) > 0) {
  current_qb_context <- team_qb_context_features |> dplyr::filter(target_season == CURRENT_SEASON) |> dplyr::select(-target_season)
  projection_table <- projection_table |> dplyr::left_join(current_qb_context, by = c("current_team" = "team"))
}
if (nrow(player_qb_context_features) > 0) {
  current_player_qb_context <- player_qb_context_features |> dplyr::filter(target_season == CURRENT_SEASON) |> dplyr::select(-target_season)
  projection_table <- projection_table |> dplyr::left_join(current_player_qb_context, by = "player_id")
}
if (nrow(alignment_features) > 0) {
  current_alignment <- alignment_features |> dplyr::filter(target_season == CURRENT_SEASON) |> dplyr::select(-target_season)
  projection_table <- projection_table |> dplyr::left_join(current_alignment, by = "player_id")
}
if (nrow(players) > 0 && all(c("gsis_id", "display_name") %in% names(players)) && "prior_primary_qb_id" %in% names(projection_table)) {
  qb_names <- players |>
    dplyr::transmute(prior_primary_qb_id = as.character(gsis_id), prior_primary_qb_name = as.character(display_name)) |>
    dplyr::distinct(prior_primary_qb_id, .keep_all = TRUE)
  projection_table <- projection_table |> dplyr::left_join(qb_names, by = "prior_primary_qb_id")
}
projection_table <- add_context_fit_features(projection_table)

projection_table <- projection_table |>
  dplyr::mutate(dplyr::across(dplyr::all_of(intersect(MODEL_FEATURES, names(projection_table))), ~ tidyr::replace_na(.x, 0))) |>
  dplyr::distinct(player_id, .keep_all = TRUE)

readr::write_csv(projection_table, paste0("data/processed/projection_table_", CURRENT_SEASON, ".csv"))

cat("[FEATURES] Training rows: ", nrow(model_table), "\n", sep = "")
cat("[FEATURES] Current projection candidates: ", nrow(projection_table), "\n", sep = "")
cat("[FEATURES] Current rookies included: ", sum(projection_table$is_rookie == 1, na.rm = TRUE), "\n", sep = "")
