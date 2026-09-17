# ============================================================
# FANTASY MODEL 3.0 -> WEB 1.1 SNAPSHOT NORMALIZER
# ============================================================
# Moves compatibility work out of the browser. The model exporter can keep its
# production field names, while the persisted JSON receives the stable aliases
# Web 1.1 consumes. Browsers can then parse the snapshot once and render it.

`%||%` <- function(a, b) if (!is.null(a)) a else b

fm_web11_number <- function(value) {
  if (is.null(value) || !length(value)) return(NULL)
  x <- suppressWarnings(as.numeric(value[[1]]))
  if (!length(x) || !is.finite(x)) return(NULL)
  x
}

fm_web11_text <- function(value) {
  if (is.null(value) || !length(value) || is.na(value[[1]])) return("")
  trimws(as.character(value[[1]]))
}

fm_web11_team <- function(value) {
  raw <- toupper(fm_web11_text(value))
  if (!nzchar(raw)) return("")
  aliases <- c(LA = "LAR", STL = "LAR", JAC = "JAX", OAK = "LV", SD = "LAC", WAS = "WSH")
  if (raw %in% names(aliases)) unname(aliases[[raw]]) else raw
}

fm_web11_completed_average <- function(weeks) {
  if (is.null(weeks) || !length(weeks)) return(NULL)
  values <- vapply(weeks, function(w) {
    played <- fm_web11_number(w$game_played)
    actual <- fm_web11_number(w$actual)
    if (!is.null(played) && played > 0 && !is.null(actual)) actual else NA_real_
  }, numeric(1))
  values <- values[is.finite(values)]
  if (!length(values)) NULL else mean(values)
}

fm_web11_current_week_row <- function(weeks, week) {
  if (is.null(weeks) || !length(weeks) || is.null(week)) return(NULL)
  for (w in weeks) {
    wk <- fm_web11_number(w$week)
    if (!is.null(wk) && wk == week) return(w)
  }
  NULL
}

fm_web11_prepare_player <- function(player, projection_week) {
  if (is.null(player) || !is.list(player)) return(player)

  weeks <- player$weekly_projections
  if (is.null(weeks) || !is.list(weeks)) weeks <- list()
  current_week <- fm_web11_number(player$week) %||% fm_web11_number(projection_week)
  current <- fm_web11_current_week_row(weeks, current_week)
  avg_ppg <- fm_web11_completed_average(weeks) %||% fm_web11_number(player$season_fppg)
  current_projection <- fm_web11_number(current$projection) %||% fm_web11_number(player$week_projection)

  player_id <- fm_web11_text(player$sleeper_id)
  if (!nzchar(player_id)) player_id <- fm_web11_text(player$gsis_id)
  if (!nzchar(player_id)) player_id <- fm_web11_text(player$player_id)
  if (nzchar(player_id)) player$player_id <- player_id

  if (!is.null(current_projection)) {
    player$projection <- current_projection
    player$weekly_projection <- current_projection
    player$pregame_projection <- current_projection
  }
  if (!is.null(current)) {
    floor_value <- fm_web11_number(current$floor)
    ceiling_value <- fm_web11_number(current$ceiling)
    if (!is.null(floor_value)) player$floor <- floor_value
    if (!is.null(ceiling_value)) player$ceiling <- ceiling_value
    if (!is.null(current$confidence)) player$confidence <- current$confidence
    if (nzchar(fm_web11_text(current$opponent))) player$opponent <- current$opponent
  }
  if (!is.null(avg_ppg)) {
    player$avg_ppg <- avg_ppg
    player$current_avg_ppg <- avg_ppg
    player$season_ppg <- avg_ppg
  }

  if (length(weeks)) {
    for (i in seq_along(weeks)) {
      week <- weeks[[i]]
      if (is.null(week) || !is.list(week)) next

      if (nzchar(player_id)) week$player_id <- player_id
      sleeper_id <- fm_web11_text(player$sleeper_id)
      gsis_id <- fm_web11_text(player$gsis_id)
      if (nzchar(sleeper_id)) week$sleeper_id <- sleeper_id
      if (nzchar(gsis_id)) week$gsis_id <- gsis_id
      if (nzchar(fm_web11_text(player$player_name))) week$player_name <- player$player_name
      if (nzchar(fm_web11_text(player$position))) week$position <- player$position
      if (nzchar(fm_web11_text(player$team))) week$team <- player$team
      if (!is.null(player$dynasty_value)) week$dynasty_value <- player$dynasty_value

      if (!is.null(avg_ppg)) {
        week$avg_ppg <- avg_ppg
        week$current_avg_ppg <- avg_ppg
        week$season_ppg <- avg_ppg
      }

      projection <- fm_web11_number(week$projection)
      if (!is.null(projection)) {
        week$weekly_projection <- projection
        week$pregame_projection <- projection
      }

      weeks[[i]] <- week
    }
  }

  player$weekly_projections <- weeks
  player
}

fm_web11_repair_opponents <- function(snapshot) {
  players <- snapshot$players
  if (is.null(players) || !is.list(players) || !length(players)) return(snapshot)

  start_week <- fm_web11_number(snapshot$projection_week) %||% 1
  schedule <- new.env(hash = TRUE, parent = emptyenv())

  set_schedule <- function(team, week, opponent) {
    key <- paste(team, as.integer(week), sep = "|")
    existing <- if (exists(key, envir = schedule, inherits = FALSE)) get(key, envir = schedule) else ""
    if (!nzchar(existing) || identical(existing, "BYE")) assign(key, opponent, envir = schedule)
  }

  for (player in players) {
    team <- fm_web11_team(player$team)
    if (!nzchar(team)) next
    weeks <- player$weekly_projections
    if (is.null(weeks) || !length(weeks)) next

    for (week in weeks) {
      week_no <- fm_web11_number(week$week)
      opponent <- fm_web11_team(week$opponent)
      if (is.null(week_no) || week_no < start_week || !nzchar(opponent)) next
      set_schedule(team, week_no, opponent)
      if (!identical(opponent, "BYE")) set_schedule(opponent, week_no, team)
    }
  }

  for (i in seq_along(players)) {
    player <- players[[i]]
    team <- fm_web11_team(player$team)
    weeks <- player$weekly_projections
    if (is.null(weeks) || !is.list(weeks)) weeks <- list()

    if (nzchar(team) && length(weeks)) {
      for (j in seq_along(weeks)) {
        week <- weeks[[j]]
        week_no <- fm_web11_number(week$week)
        if (is.null(week_no) || week_no < start_week || nzchar(fm_web11_text(week$opponent))) next
        key <- paste(team, as.integer(week_no), sep = "|")
        if (exists(key, envir = schedule, inherits = FALSE)) week$opponent <- get(key, envir = schedule)
        weeks[[j]] <- week
      }
    }

    player$weekly_projections <- weeks
    current_week <- fm_web11_number(player$week) %||% fm_web11_number(snapshot$projection_week)
    current <- fm_web11_current_week_row(weeks, current_week)
    if (!is.null(current) && nzchar(fm_web11_text(current$opponent))) player$opponent <- current$opponent
    players[[i]] <- player
  }

  snapshot$players <- players
  snapshot
}

fm_web11_pick_values <- function(snapshot, curve_path = NULL) {
  base_season <- fm_web11_number(snapshot$season)
  if (is.null(base_season)) base_season <- as.integer(format(Sys.Date(), "%Y"))
  future_discount <- 0.88
  round_base <- c(`1` = 4200, `2` = 1800, `3` = 800, `4` = 350)
  source_name <- "fallback_future_pick_curve"

  if (!is.null(curve_path) && file.exists(curve_path)) {
    curve <- tryCatch(read.csv(curve_path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(curve) && nrow(curve) && all(c("round", "pick_value") %in% names(curve))) {
      med <- tapply(suppressWarnings(as.numeric(curve$pick_value)), suppressWarnings(as.integer(curve$round)), median, na.rm = TRUE)
      med <- med[is.finite(med)]
      if (length(med)) {
        round_base <- med
        source_name <- "career_model_rookie_ev_curve"
      }
    }
  }

  existing <- list()
  for (season in seq.int(as.integer(base_season) + 1L, as.integer(base_season) + 6L)) {
    for (round in 1:8) {
      key <- as.character(round)
      if (key %in% names(round_base)) {
        base_value <- as.numeric(round_base[[key]])
      } else {
        last_known <- if (length(round_base)) as.numeric(tail(round_base, 1)) else 100
        max(100, last_known * 0.55 ^ max(1, round - length(round_base)))
      }
      value <- round(base_value * future_discount ^ max(1, season - base_season))
      existing[[length(existing) + 1L]] <- list(
        sleeper_id = NA_character_, asset_type = "future_pick",
        season = season, round = round,
        dynasty_value = value, pick_value = value,
        valuation_source = source_name
      )
    }
  }
  snapshot$pick_values <- existing
  snapshot
}
fm_normalize_web_snapshot <- function(path = "output/model_snapshot.json") {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first: install.packages('jsonlite')")
  if (!file.exists(path)) stop("Snapshot not found: ", path)

  snapshot <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (is.null(snapshot) || !is.list(snapshot)) stop("Snapshot is not a JSON object: ", path)

  players <- snapshot$players
  if (!is.null(players) && is.list(players) && length(players)) {
    snapshot$players <- lapply(players, fm_web11_prepare_player, projection_week = snapshot$projection_week)
    snapshot <- fm_web11_repair_opponents(snapshot)
  }

  snapshot <- fm_web11_pick_values(snapshot, file.path(dirname(path), "dynasty_pick_curve_3_0.csv"))
  snapshot$web_schema_version <- 2L
  snapshot$web_version <- "1.3.0"
  snapshot$web_snapshot_contract <- "web-1.3-career-value-dst"

  jsonlite::write_json(snapshot, path, pretty = FALSE, auto_unbox = TRUE, na = "null", digits = NA)
  message("[WEB 1.1] Normalized snapshot for single-parse browser delivery: ", path)
  invisible(path)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  path <- if (length(args) && nzchar(args[[1]])) args[[1]] else "output/model_snapshot.json"
  fm_normalize_web_snapshot(path)
}
