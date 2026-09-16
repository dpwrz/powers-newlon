# ============================================================
# FANTASY MODEL 2.1 - WEEKLY PROJECTION ENGINE
# ============================================================
# Strict design rule: every feature used for a player-week must be
# available before that week's kickoff. Current-week outcomes are
# never used to construct current-week rolling or matchup features.

wk_num <- function(x) suppressWarnings(as.numeric(x))
wk_chr <- function(x) as.character(x)
wk_rate <- function(num, den, default = 0) {
  num <- wk_num(num); den <- wk_num(den)
  out <- ifelse(is.finite(den) & den > 0, num / den, default)
  out[!is.finite(out)] <- default
  out
}

first_existing_col21 <- function(df, candidates, default = NA) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(rep(default, nrow(df)))
  df[[hit[1]]]
}

ensure_weekly_col21 <- function(df, nm, default = 0) {
  if (!nm %in% names(df)) df[[nm]] <- default
  df
}

lag_roll_mean21 <- function(x, k) {
  x <- wk_num(x)
  n <- length(x)
  out <- rep(NA_real_, n)
  if (n == 0) return(out)
  for (i in seq_len(n)) {
    hi <- i - 1L
    if (hi < 1L) next
    lo <- max(1L, hi - as.integer(k) + 1L)
    z <- x[lo:hi]
    z <- z[is.finite(z)]
    if (length(z) > 0) out[i] <- mean(z)
  }
  out
}

lag_roll_sd21 <- function(x, k) {
  x <- wk_num(x)
  n <- length(x)
  out <- rep(NA_real_, n)
  if (n == 0) return(out)
  for (i in seq_len(n)) {
    hi <- i - 1L
    if (hi < 2L) next
    lo <- max(1L, hi - as.integer(k) + 1L)
    z <- x[lo:hi]
    z <- z[is.finite(z)]
    if (length(z) >= 2) out[i] <- stats::sd(z)
  }
  out
}

lag_cum_mean21 <- function(x) {
  x <- wk_num(x)
  n <- length(x)
  out <- rep(NA_real_, n)
  if (n == 0) return(out)
  for (i in seq_len(n)) {
    if (i <= 1L) next
    z <- x[seq_len(i - 1L)]
    z <- z[is.finite(z)]
    if (length(z) > 0) out[i] <- mean(z)
  }
  out
}

lag_games_played21 <- function(n) pmax(0, seq_len(n) - 1L)

weekly_fantasy_points21 <- function(df) {
  grab <- function(cands) wk_num(first_existing_col21(df, cands, 0))
  pass_yd <- grab(c("passing_yards", "pass_yards"))
  pass_td <- grab(c("passing_tds", "pass_tds"))
  ints <- grab(c("passing_interceptions", "interceptions"))
  rush_yd <- grab(c("rushing_yards", "rush_yards"))
  rush_td <- grab(c("rushing_tds", "rush_tds"))
  rec <- grab(c("receptions"))
  rec_yd <- grab(c("receiving_yards", "rec_yards"))
  rec_td <- grab(c("receiving_tds", "rec_tds"))
  fum <- grab(c("fumbles_lost"))
  two <- grab(c("passing_2pt_conversions", "rushing_2pt_conversions", "receiving_2pt_conversions", "two_point_conversions"))
  # If the data has split two-point columns, first_existing_col21 intentionally
  # picks only one. Add the known split columns explicitly when present.
  if (all(c("passing_2pt_conversions", "rushing_2pt_conversions", "receiving_2pt_conversions") %in% names(df))) {
    two <- wk_num(df$passing_2pt_conversions) + wk_num(df$rushing_2pt_conversions) + wk_num(df$receiving_2pt_conversions)
  }
  pass_yd * SCORING$pass_yd + pass_td * SCORING$pass_td + ints * SCORING$interception +
    rush_yd * SCORING$rush_yd + rush_td * SCORING$rush_td + rec * SCORING$reception +
    rec_yd * SCORING$rec_yd + rec_td * SCORING$rec_td + fum * SCORING$fumble_lost +
    two * SCORING$two_pt
}

normalize_weekly_stats21 <- function(raw) {
  # Pre-Week-1 / unavailable-current-season safety: return a typed zero-row
  # table so downstream code can safely reference canonical weekly columns.
  if (nrow(raw) == 0) {
    return(data.frame(
      player_id = character(), player_display_name = character(), position = character(),
      team = character(), opponent = character(), season = integer(), week = integer(),
      targets = numeric(), receptions = numeric(), carries = numeric(), pass_attempts = numeric(),
      receiving_yards = numeric(), rushing_yards = numeric(), passing_yards = numeric(),
      receiving_tds = numeric(), rushing_tds = numeric(), passing_tds = numeric(),
      weekly_fppg = numeric(), stringsAsFactors = FALSE
    ))
  }
  out <- raw
  out$player_id <- wk_chr(first_existing_col21(out, c("player_id", "gsis_id"), NA_character_))
  out$player_display_name <- wk_chr(first_existing_col21(out, c("player_display_name", "player_name", "full_name"), "Unknown"))
  out$position <- toupper(wk_chr(first_existing_col21(out, c("position", "position_group"), "")))
  out$team <- wk_chr(first_existing_col21(out, c("recent_team", "team", "team_abbr"), NA_character_))
  out$opponent <- wk_chr(first_existing_col21(out, c("opponent_team", "opponent"), NA_character_))
  out$season <- as.integer(wk_num(first_existing_col21(out, c("season"), NA)))
  out$week <- as.integer(wk_num(first_existing_col21(out, c("week"), NA)))

  out$targets <- wk_num(first_existing_col21(out, c("targets"), 0))
  out$receptions <- wk_num(first_existing_col21(out, c("receptions"), 0))
  out$carries <- wk_num(first_existing_col21(out, c("carries", "rushing_attempts"), 0))
  out$pass_attempts <- wk_num(first_existing_col21(out, c("attempts", "passing_attempts"), 0))
  out$receiving_yards <- wk_num(first_existing_col21(out, c("receiving_yards"), 0))
  out$rushing_yards <- wk_num(first_existing_col21(out, c("rushing_yards"), 0))
  out$passing_yards <- wk_num(first_existing_col21(out, c("passing_yards"), 0))
  out$receiving_tds <- wk_num(first_existing_col21(out, c("receiving_tds"), 0))
  out$rushing_tds <- wk_num(first_existing_col21(out, c("rushing_tds"), 0))
  out$passing_tds <- wk_num(first_existing_col21(out, c("passing_tds"), 0))
  out$weekly_fppg <- weekly_fantasy_points21(out)
  keep <- is.finite(out$season) & is.finite(out$week) & out$week >= 1 & out$week <= 18 & out$position %in% POSITIONS & !is.na(out$player_id) & nzchar(out$player_id)
  if ("season_type" %in% names(out)) keep <- keep & (is.na(out$season_type) | out$season_type == "REG")
  out <- out[keep, , drop = FALSE]
  out
}

schedule_team_rows21 <- function(schedules) {
  # Return a typed empty schedule instead of a columnless data.frame. This keeps
  # downstream filters deterministic when a remote schedule request returns 0 rows.
  if (is.null(schedules) || nrow(schedules) == 0) {
    return(data.frame(
      season = integer(), week = integer(), team = character(), opponent = character(),
      is_home = integer(), rest_days = numeric(), total_line = numeric(),
      team_spread_line = numeric(), game_played = integer(), gameday = character(),
      implied_team_total = numeric(), stringsAsFactors = FALSE
    ))
  }
  for (nm in c("season", "week", "game_type", "home_team", "away_team", "home_rest", "away_rest", "total_line", "spread_line", "home_score", "away_score", "gameday")) {
    schedules <- ensure_weekly_col21(schedules, nm, NA)
  }
  s <- schedules |>
    dplyr::filter(game_type == "REG")
  home <- s |>
    dplyr::transmute(
      season = as.integer(season), week = as.integer(week), team = wk_chr(home_team), opponent = wk_chr(away_team),
      is_home = 1, rest_days = wk_num(home_rest), total_line = wk_num(total_line), team_spread_line = wk_num(spread_line),
      game_played = as.integer(is.finite(wk_num(home_score)) & is.finite(wk_num(away_score))), gameday = as.character(gameday)
    )
  away <- s |>
    dplyr::transmute(
      season = as.integer(season), week = as.integer(week), team = wk_chr(away_team), opponent = wk_chr(home_team),
      is_home = 0, rest_days = wk_num(away_rest), total_line = wk_num(total_line), team_spread_line = -wk_num(spread_line),
      game_played = as.integer(is.finite(wk_num(home_score)) & is.finite(wk_num(away_score))), gameday = as.character(gameday)
    )
  dplyr::bind_rows(home, away) |>
    dplyr::mutate(implied_team_total = dplyr::if_else(is.finite(total_line) & is.finite(team_spread_line),
                                                       (total_line + team_spread_line) / 2, NA_real_)) |>
    dplyr::filter(!is.na(team), team != "") |>
    dplyr::distinct(season, week, team, .keep_all = TRUE)
}

injury_risk21 <- function(status) {
  s <- tolower(trimws(wk_chr(status)))
  dplyr::case_when(
    grepl("out|reserve|pup", s) ~ 1.00,
    grepl("doubt", s) ~ 0.80,
    grepl("question", s) ~ 0.40,
    TRUE ~ 0
  )
}

practice_risk21 <- function(status) {
  s <- tolower(trimws(wk_chr(status)))
  dplyr::case_when(
    grepl("did not|dnp", s) ~ 0.75,
    grepl("limited", s) ~ 0.35,
    grepl("full", s) ~ 0,
    TRUE ~ 0
  )
}

prepare_injuries21 <- function(injuries) {
  if (nrow(injuries) == 0) return(list(player = data.frame(), team = data.frame()))
  for (nm in c("season", "week", "gsis_id", "team", "position", "report_status", "practice_status", "date_modified")) injuries <- ensure_weekly_col21(injuries, nm, NA)
  x <- injuries |>
    dplyr::mutate(
      season = as.integer(wk_num(season)), week = as.integer(wk_num(week)), player_id = wk_chr(gsis_id), team = wk_chr(team),
      position = toupper(wk_chr(position)), injury_risk = injury_risk21(report_status), practice_risk = practice_risk21(practice_status),
      date_modified_chr = wk_chr(date_modified)
    ) |>
    dplyr::arrange(season, week, player_id, date_modified_chr) |>
    dplyr::group_by(season, week, player_id) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()
  player <- x |>
    dplyr::select(season, week, player_id, injury_risk, practice_risk, injury_status = report_status, practice_status)
  team <- x |>
    dplyr::filter(position %in% c("QB", "RB", "WR", "TE")) |>
    dplyr::group_by(season, week, team) |>
    dplyr::summarise(
      team_skill_out_count = sum(injury_risk >= 0.80, na.rm = TRUE),
      team_skill_questionable_count = sum(injury_risk > 0 & injury_risk < 0.80, na.rm = TRUE),
      .groups = "drop"
    )
  list(player = player, team = team)
}

WEEKLY_COMMON_FEATURES_21 <- c(
  "season_week", "is_home", "rest_days", "total_line", "team_spread_line", "implied_team_total",
  "preseason_prior_fppg", "games_played_prior", "prior_game_fppg", "roll3_fppg", "roll5_fppg", "roll3_fppg_sd", "season_to_date_fppg",
  "roll3_offense_pct", "roll5_offense_pct", "roll3_offense_snaps", "snap_trend", "opportunity_per_snap",
  "roll3_targets", "roll3_carries", "roll3_pass_attempts", "roll3_target_share", "roll3_carry_share", "roll3_pass_attempt_share",
  "roll3_team_pass_attempts", "roll3_team_carries",
  "injury_risk", "practice_risk", "team_skill_out_count", "team_skill_questionable_count",
  "opp_pos_residual_roll4", "opp_pos_residual_roll8", "opp_pos_fppg_allowed_roll4",
  "def_pass_epa_allowed_roll4", "def_pass_epa_allowed_roll8", "def_rush_epa_allowed_roll4", "def_rush_epa_allowed_roll8",
  "def_pass_success_allowed_roll4", "def_rush_success_allowed_roll4", "def_explosive_pass_rate_roll4",
  "def_sack_rate_roll4", "def_qb_hit_rate_roll4", "def_redzone_pass_td_rate_roll4", "def_redzone_rush_td_rate_roll4",
  "qb_pass_matchup", "qb_pressure_matchup", "rb_rush_matchup", "rb_receiving_matchup",
  "wr_volume_matchup", "wr_deep_matchup", "wr_redzone_matchup", "te_middle_matchup", "te_redzone_matchup"
)

WEEKLY_POSITION_FEATURES_21 <- list(
  QB = unique(c(WEEKLY_COMMON_FEATURES_21, "roll5_pass_attempts", "roll5_carries", "def_deep_pass_rate_roll4", "def_deep_epa_allowed_roll4")),
  RB = unique(c(WEEKLY_COMMON_FEATURES_21, "roll5_carries", "roll5_targets", "def_redzone_rush_td_rate_roll4")),
  WR = unique(c(WEEKLY_COMMON_FEATURES_21, "roll5_targets", "def_deep_pass_rate_roll4", "def_deep_epa_allowed_roll4", "def_wr_residual_roll4", "preseason_deep_target_rate", "preseason_redzone_target_rate")),
  TE = unique(c(WEEKLY_COMMON_FEATURES_21, "roll5_targets", "def_middle_pass_rate_roll4", "def_middle_epa_allowed_roll4", "def_te_residual_roll4", "preseason_middle_target_rate", "preseason_redzone_target_rate"))
)

get_weekly_features21 <- function(position, available_names = NULL) {
  pos <- toupper(wk_chr(position)[1])
  out <- WEEKLY_POSITION_FEATURES_21[[pos]]
  if (is.null(out)) out <- WEEKLY_COMMON_FEATURES_21
  if (!is.null(available_names)) out <- intersect(out, available_names)
  unique(out)
}

weekly_baseline21 <- function(preseason_prior_fppg, roll3_fppg, games_played_prior) {
  pre <- wk_num(preseason_prior_fppg); r3 <- wk_num(roll3_fppg); g <- wk_num(games_played_prior)
  pre[!is.finite(pre)] <- 0
  r3[!is.finite(r3)] <- pre[!is.finite(r3)]
  # Gradual transition from preseason prior to current role/performance.
  current_weight <- pmin(0.75, pmax(0, g) / 8 * 0.75)
  pmax(0, (1 - current_weight) * pre + current_weight * r3)
}

weekly_matchup_delta21 <- function(row) {
  # Positive means opponent has been friendlier than expectation to the position.
  x <- wk_num(row$opp_pos_residual_roll4)
  ifelse(is.finite(x), x, 0)
}

safe_cor21 <- function(x, y, method = "pearson") {
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 5 || stats::sd(x[keep]) < 1e-10 || stats::sd(y[keep]) < 1e-10) return(NA_real_)
  suppressWarnings(stats::cor(x[keep], y[keep], method = method))
}
