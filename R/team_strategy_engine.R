# ============================================================
# FANTASY MODEL 3.0 - TEAM / LEAGUE STRATEGY ENGINE
# 3.0.5: schema-safe production fallback for roster lineup scoring
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "tibble", "purrr"))
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

fm3_sleeper_league_settings <- function(state) {
  lg <- state$league
  rp <- as.character(unlist(lg$roster_positions %||% character(), use.names = FALSE))
  ss <- lg$scoring_settings %||% list()
  count <- function(keys) sum(rp %in% keys)
  rec <- suppressWarnings(as.numeric(ss$rec %||% 0.5)); if (!is.finite(rec)) rec <- 0.5
  ptd <- suppressWarnings(as.numeric(ss$pass_td %||% 4)); if (!is.finite(ptd)) ptd <- 4
  tep <- suppressWarnings(as.numeric(ss$bonus_rec_te %||% ss$rec_te_bonus %||% 0)); if (!is.finite(tep)) tep <- 0
  list(
    league_name = as.character(lg$name %||% "Sleeper League"),
    teams = suppressWarnings(as.numeric(lg$total_rosters %||% length(unique(state$roster_owners$roster_id)))),
    scoring_preset = if (rec >= 0.9) "PPR" else if (rec <= 0.1) "Standard" else "Half PPR",
    reception_points = rec,
    pass_td_points = ptd,
    te_premium = tep,
    qb_starters = count(c("QB")),
    rb_starters = count(c("RB")),
    wr_starters = count(c("WR")),
    te_starters = count(c("TE")),
    flex_starters = count(c("FLEX", "WRRB_FLEX", "REC_FLEX")),
    superflex_starters = count(c("SUPER_FLEX", "SUPERFLEX", "OP"))
  )
}

fm3_choose_lineup <- function(roster, settings) {
  d <- roster |>
    dplyr::filter(position %in% c("QB", "RB", "WR", "TE"))

  # `year1_fppg` is the preferred dynasty-window production signal, while
  # `projected_fppg` is a production fallback from the season model.  Older
  # cached/portable roster-value frames may legitimately be missing one of
  # these columns.  Do not reference a missing column inside dplyr's data mask:
  # both arguments to coalesce() are evaluated, which caused the live app
  # `object 'projected_fppg' not found` failure even when year1_fppg existed.
  n <- nrow(d)
  safe_num <- function(name) {
    if (!name %in% names(d)) return(rep(NA_real_, n))
    x <- suppressWarnings(as.numeric(d[[name]]))
    if (length(x) != n) return(rep(NA_real_, n))
    x
  }
  year1 <- safe_num("year1_fppg")
  season <- safe_num("projected_fppg")
  prod <- ifelse(is.finite(year1), year1, ifelse(is.finite(season), season, 0))
  d[["prod"]] <- prod
  d[["selected"]] <- FALSE
  selected_ids <- character()
  take <- function(pos, n) {
    if (n <= 0) return(character())
    pool <- d |> dplyr::filter(position == pos, !sleeper_id %in% selected_ids) |> dplyr::arrange(dplyr::desc(prod))
    head(as.character(pool$sleeper_id), n)
  }
  for (pos in c("QB", "RB", "WR", "TE")) {
    n <- as.integer(settings[[paste0(tolower(pos), "_starters")]] %||% 0)
    ids <- take(pos, n); selected_ids <- c(selected_ids, ids)
  }
  flex_n <- as.integer(settings$flex_starters %||% 0)
  if (flex_n > 0) {
    pool <- d |> dplyr::filter(position %in% c("RB","WR","TE"), !sleeper_id %in% selected_ids) |> dplyr::arrange(dplyr::desc(prod))
    selected_ids <- c(selected_ids, head(as.character(pool$sleeper_id), flex_n))
  }
  sf_n <- as.integer(settings$superflex_starters %||% 0)
  if (sf_n > 0) {
    pool <- d |> dplyr::filter(!sleeper_id %in% selected_ids) |> dplyr::arrange(dplyr::desc(prod))
    selected_ids <- c(selected_ids, head(as.character(pool$sleeper_id), sf_n))
  }
  d |> dplyr::mutate(optimal_starter = sleeper_id %in% selected_ids)
}

fm3_future_draft_rounds <- function(state, fallback = 4L) {
  from_league <- suppressWarnings(as.integer(state$league$settings$draft_rounds %||% NA))
  if (is.finite(from_league) && from_league >= 1 && from_league <= 8) return(from_league)
  if (!is.null(state$drafts) && nrow(state$drafts) && "rounds" %in% names(state$drafts)) {
    candidates <- suppressWarnings(as.integer(state$drafts$rounds))
    candidates <- candidates[is.finite(candidates) & candidates >= 1 & candidates <= 8]
    if (length(candidates)) return(as.integer(candidates[1]))
  }
  as.integer(fallback)
}

fm3_all_future_picks <- function(state, seasons = (CURRENT_SEASON + 1):(CURRENT_SEASON + 3), rounds = NULL) {
  if (is.null(rounds)) rounds <- seq_len(fm3_future_draft_rounds(state))
  rosters <- sort(unique(state$roster_owners$roster_id))
  if (!length(rosters)) return(tibble::tibble())
  grid <- expand.grid(original_roster_id = rosters, season = seasons, round = rounds, stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    dplyr::mutate(owner_roster_id = original_roster_id)
  tp <- state$traded_picks
  if (!is.null(tp) && nrow(tp)) {
    key <- paste(tp$original_roster_id, tp$season, tp$round, sep = "|")
    gkey <- paste(grid$original_roster_id, grid$season, grid$round, sep = "|")
    m <- match(gkey, key)
    hit <- !is.na(m)
    grid$owner_roster_id[hit] <- tp$owner_roster_id[m[hit]]
  }
  grid
}

fm3_attach_roster_values <- function(state, dynasty_values) {
  rp <- state$roster_players
  if (!nrow(rp)) return(rp)
  value_cols <- intersect(c("sleeper_id", "gsis_id", "player_display_name", "position", "current_team", "model_dynasty_value",
                            "market_value_proxy", "model_market_gap", "model_market_gap_pct", "value_signal", "year1_fppg", "year3_fppg",
                            "projected_fppg", "age", "is_rookie", "confidence", "breakout_probability", "elite_probability",
                            "week", "opponent", "gameday", "projected_weekly_fppg_24", "weekly_floor", "weekly_ceiling",
                            "projection_confidence", "expected_abs_error", "matchup_grade", "boom_probability", "bust_probability",
                            "injury_status", "practice_status", "availability_factor", "projected_targets_30", "projected_carries_30",
                            "projected_snap_share_30", "role_regime_probability_30", "role_uncertainty_30"), names(dynasty_values))
  out <- rp |>
    dplyr::left_join(dynasty_values |> dplyr::select(dplyr::all_of(value_cols)), by = "sleeper_id", suffix = c("_sleeper", ""))

  # Normalize the handful of columns consumed by every downstream team/draft/
  # trade routine.  Explicit indexing keeps optional fields from becoming
  # zero-length vectors when a cached league state or portable artifact has a
  # slightly older schema.
  n <- nrow(out)
  ensure_chr <- function(name, default = "") {
    if (!name %in% names(out)) out[[name]] <<- rep(default, n)
    x <- as.character(out[[name]])
    if (length(x) != n) x <- rep(default, n)
    x[is.na(x)] <- default
    out[[name]] <<- x
  }
  ensure_num <- function(name, default = NA_real_) {
    if (!name %in% names(out)) out[[name]] <<- rep(default, n)
    x <- suppressWarnings(as.numeric(out[[name]]))
    if (length(x) != n) x <- rep(default, n)
    out[[name]] <<- x
  }

  for (nm in c("sleeper_id", "full_name", "player_display_name", "position", "position_sleeper", "team_name",
                 "opponent", "projection_confidence", "matchup_grade", "injury_status", "practice_status", "value_signal")) ensure_chr(nm, "")
  for (nm in c("model_dynasty_value", "year1_fppg", "year3_fppg", "projected_fppg", "age", "age_sleeper",
                 "market_value_proxy", "model_market_gap", "model_market_gap_pct", "week", "projected_weekly_fppg_24",
                 "weekly_floor", "weekly_ceiling", "expected_abs_error", "boom_probability", "bust_probability",
                 "projected_targets_30", "projected_carries_30", "projected_snap_share_30",
                 "role_regime_probability_30", "role_uncertainty_30")) ensure_num(nm)

  out[["position"]] <- ifelse(nzchar(out[["position"]]), out[["position"]], out[["position_sleeper"]])
  out[["player_display_name"]] <- ifelse(nzchar(out[["player_display_name"]]), out[["player_display_name"]], out[["full_name"]])
  out[["model_dynasty_value"]][!is.finite(out[["model_dynasty_value"]])] <- 0

  # Prefer the multi-year dynasty year-1 projection, then the production model.
  y1 <- out[["year1_fppg"]]
  season <- out[["projected_fppg"]]
  y1[!is.finite(y1) & is.finite(season)] <- season[!is.finite(y1) & is.finite(season)]
  y1[!is.finite(y1)] <- 0
  out[["year1_fppg"]] <- y1
  out[["projected_fppg"]][!is.finite(out[["projected_fppg"]])] <- out[["year1_fppg"]][!is.finite(out[["projected_fppg"]])]

  age <- out[["age"]]
  fallback_age <- out[["age_sleeper"]]
  age[!is.finite(age) & is.finite(fallback_age)] <- fallback_age[!is.finite(age) & is.finite(fallback_age)]
  age[!is.finite(age)] <- 27
  out[["age"]] <- age

  tibble::as_tibble(out)
}

fm3_team_power_table <- function(state, dynasty_values, settings = fm3_sleeper_league_settings(state)) {
  rp <- fm3_attach_roster_values(state, dynasty_values)
  if (!nrow(rp)) return(tibble::tibble())
  teams <- split(rp, rp$roster_id)
  rows <- purrr::imap_dfr(teams, function(roster, id) {
    lineup <- fm3_choose_lineup(roster, settings)
    core <- roster |> dplyr::arrange(dplyr::desc(model_dynasty_value)) |> utils::head(18)
    weighted_age_num <- sum(core$age * pmax(core$model_dynasty_value, 1), na.rm = TRUE)
    weighted_age_den <- sum(pmax(core$model_dynasty_value, 1), na.rm = TRUE)
    age <- if (weighted_age_den > 0) weighted_age_num / weighted_age_den else 27
    tibble::tibble(
      roster_id = as.integer(id),
      team_name = as.character(roster$team_name[1] %||% paste0("Roster ", id)),
      starter_fppg = sum(lineup$year1_fppg[lineup$optimal_starter], na.rm = TRUE),
      starter_dynasty_value = sum(lineup$model_dynasty_value[lineup$optimal_starter], na.rm = TRUE),
      roster_dynasty_value = sum(core$model_dynasty_value, na.rm = TRUE),
      weighted_core_age = age,
      qb_value = sum(core$model_dynasty_value[core$position == "QB"], na.rm = TRUE),
      rb_value = sum(core$model_dynasty_value[core$position == "RB"], na.rm = TRUE),
      wr_value = sum(core$model_dynasty_value[core$position == "WR"], na.rm = TRUE),
      te_value = sum(core$model_dynasty_value[core$position == "TE"], na.rm = TRUE)
    )
  })

  picks <- fm3_all_future_picks(state)
  pick_count <- if (nrow(picks)) picks |> dplyr::count(owner_roster_id, name = "future_pick_count") else tibble::tibble(owner_roster_id = integer(), future_pick_count = integer())
  first_count <- if (nrow(picks)) picks |> dplyr::filter(round == 1) |> dplyr::count(owner_roster_id, name = "future_first_count") else tibble::tibble(owner_roster_id = integer(), future_first_count = integer())

  rows <- rows |>
    dplyr::left_join(pick_count, by = c("roster_id" = "owner_roster_id")) |>
    dplyr::left_join(first_count, by = c("roster_id" = "owner_roster_id")) |>
    dplyr::mutate(
      future_pick_count = dplyr::coalesce(future_pick_count, 0L),
      future_first_count = dplyr::coalesce(future_first_count, 0L),
      production_percentile = dplyr::percent_rank(starter_fppg),
      dynasty_percentile = dplyr::percent_rank(roster_dynasty_value),
      youth_percentile = dplyr::percent_rank(-weighted_core_age),
      pick_percentile = dplyr::percent_rank(future_first_count + 0.25 * future_pick_count),
      contender_index = 100 * (0.58 * production_percentile + 0.24 * dynasty_percentile + 0.10 * youth_percentile + 0.08 * pick_percentile),
      strategy = dplyr::case_when(
        production_percentile >= 0.72 ~ "CONTENDER",
        production_percentile >= 0.52 ~ "FRINGE CONTENDER",
        production_percentile < 0.35 & dynasty_percentile < 0.35 ~ "DEEP REBUILD",
        production_percentile < 0.42 & (youth_percentile >= 0.55 | pick_percentile >= 0.55) ~ "REBUILD",
        TRUE ~ "RETOOL"
      )
    )
  # A transparent strength proxy, not a calibrated championship probability.
  z <- as.numeric(scale(rows$starter_fppg)); z[!is.finite(z)] <- 0
  e <- exp(pmin(5, pmax(-5, 1.15 * z)))
  rows$title_equity_proxy <- e / sum(e)
  rows |> dplyr::arrange(dplyr::desc(contender_index))
}

fm3_team_needs <- function(team_power, roster_id) {
  row <- team_power |> dplyr::filter(roster_id == !!roster_id)
  if (!nrow(row)) return(tibble::tibble())
  pos_cols <- c(QB = "qb_value", RB = "rb_value", WR = "wr_value", TE = "te_value")
  all <- purrr::imap_dfr(pos_cols, function(col, pos) {
    vals <- team_power[[col]]
    val <- row[[col]][1]
    pct <- if (length(unique(vals)) > 1) mean(vals <= val, na.rm = TRUE) else 0.5
    tibble::tibble(position = pos, value = val, league_percentile = pct, need_score = 1 - pct)
  })
  all |> dplyr::arrange(dplyr::desc(need_score))
}

fm3_team_strategy_text <- function(strategy) {
  switch(strategy,
    "CONTENDER" = "Prioritize lineup points and reliable depth. Future picks can be spent when the production gain is meaningful.",
    "FRINGE CONTENDER" = "Improve weak starter spots without paying full contender prices until the roster clearly crosses into the top tier.",
    "REBUILD" = "Accumulate young cornerstone assets and future firsts; sell fragile short-term production when the market pays for it.",
    "DEEP REBUILD" = "Consolidate aging production into picks and young long-horizon assets. Avoid paying for near-term points.",
    "RETOOL" = "Preserve long-term value while fixing one or two structural weaknesses; prefer flexible assets and value-accretive trades.",
    "Balance current production and long-term value."
  )
}
