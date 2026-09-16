# ============================================================
# FANTASY MODEL 3.0 - SLEEPER READ-ONLY INTEGRATION
# ============================================================
# Official API: https://docs.sleeper.com/
# Sleeper's public API is read-only and does not require an API token.

source("config.R")
ensure_packages(c("httr2", "jsonlite", "dplyr", "tibble", "purrr", "readr"))

fm3_null <- function(x, fallback = NULL) {
  if (is.null(x) || length(x) == 0) fallback else x
}

fm3_cache_path <- function(name) {
  dir.create(SLEEPER_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  file.path(SLEEPER_CACHE_DIR, name)
}

fm3_cache_fresh <- function(path, hours = 1) {
  if (!file.exists(path)) return(FALSE)
  age <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "hours"))
  is.finite(age) && age <= hours
}

sleeper_get_json <- function(path, query = list(), timeout_seconds = 30) {
  url <- paste0(SLEEPER_API_BASE, path)
  req <- httr2::request(url) |>
    httr2::req_user_agent(paste0("FantasyModel/", APP_VERSION, " dynasty-intelligence")) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_retry(max_tries = 3)
  if (length(query)) req <- do.call(httr2::req_url_query, c(list(req), query))
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) >= 300) {
    stop("Sleeper API request failed: ", httr2::resp_status(resp), " ", path)
  }
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

sleeper_get_user <- function(username_or_id) {
  sleeper_get_json(paste0("/user/", utils::URLencode(username_or_id, reserved = TRUE)))
}

sleeper_get_user_leagues <- function(user_id, season = CURRENT_SEASON) {
  x <- sleeper_get_json(paste0("/user/", user_id, "/leagues/nfl/", season))
  if (!length(x)) return(tibble::tibble())
  purrr::map_dfr(x, function(z) tibble::tibble(
    league_id = as.character(fm3_null(z$league_id, "")),
    league_name = as.character(fm3_null(z$name, "Unnamed League")),
    season = as.character(fm3_null(z$season, season)),
    status = as.character(fm3_null(z$status, "")),
    total_rosters = suppressWarnings(as.integer(fm3_null(z$total_rosters, NA))),
    league_type = suppressWarnings(as.integer(fm3_null(z$settings$type, NA))),
    draft_id = as.character(fm3_null(z$draft_id, ""))
  ))
}

sleeper_get_league <- function(league_id) sleeper_get_json(paste0("/league/", league_id))
sleeper_get_rosters <- function(league_id) sleeper_get_json(paste0("/league/", league_id, "/rosters"))
sleeper_get_league_users <- function(league_id) sleeper_get_json(paste0("/league/", league_id, "/users"))
sleeper_get_drafts <- function(league_id) sleeper_get_json(paste0("/league/", league_id, "/drafts"))
sleeper_get_draft <- function(draft_id) sleeper_get_json(paste0("/draft/", draft_id))
sleeper_get_draft_picks <- function(draft_id) sleeper_get_json(paste0("/draft/", draft_id, "/picks"))
sleeper_get_draft_traded_picks <- function(draft_id) sleeper_get_json(paste0("/draft/", draft_id, "/traded_picks"))
sleeper_get_traded_picks <- function(league_id) sleeper_get_json(paste0("/league/", league_id, "/traded_picks"))
sleeper_get_nfl_state <- function() sleeper_get_json("/state/nfl")
sleeper_get_transactions <- function(league_id, week) sleeper_get_json(paste0("/league/", league_id, "/transactions/", week))

sleeper_get_players <- function(force = FALSE) {
  path <- fm3_cache_path("sleeper_players_nfl.rds")
  if (!force && fm3_cache_fresh(path, SLEEPER_PLAYER_CACHE_HOURS)) return(readRDS(path))
  x <- sleeper_get_json("/players/nfl", timeout_seconds = 90)
  saveRDS(x, path)
  x
}

fm3_normalize_sleeper_player_frame <- function(d) {
  if (is.null(d) || !is.data.frame(d)) return(tibble::tibble())
  d <- tibble::as_tibble(d)
  n <- nrow(d)

  first_alias <- function(candidates, default = NA) {
    hit <- candidates[candidates %in% names(d)]
    if (!length(hit)) return(rep(default, n))
    x <- d[[hit[[1]]]]
    if (length(x) != n) return(rep(default, n))
    x
  }

  sleeper_id <- as.character(first_alias(c("sleeper_id", "player_id", "id"), ""))
  gsis_id <- as.character(first_alias(c("gsis_id", "player_gsis_id", "nflverse_id"), ""))
  first_name <- as.character(first_alias(c("first_name"), ""))
  last_name <- as.character(first_alias(c("last_name"), ""))
  full_name <- as.character(first_alias(c("full_name", "player_name", "display_name"), ""))
  missing_name <- is.na(full_name) | !nzchar(trimws(full_name))
  full_name[missing_name] <- trimws(paste(first_name[missing_name], last_name[missing_name]))

  out <- tibble::tibble(
    sleeper_id = sleeper_id,
    gsis_id = gsis_id,
    first_name = first_name,
    last_name = last_name,
    full_name = full_name,
    position = as.character(first_alias(c("position", "pos"), "")),
    team = as.character(first_alias(c("team", "current_team"), "FA")),
    age = suppressWarnings(as.numeric(first_alias(c("age"), NA_real_))),
    years_exp = suppressWarnings(as.numeric(first_alias(c("years_exp", "experience"), NA_real_))),
    active = as.logical(first_alias(c("active"), FALSE)),
    injury_status = as.character(first_alias(c("injury_status"), "")),
    practice_participation = as.character(first_alias(c("practice_participation"), "")),
    depth_chart_order = suppressWarnings(as.numeric(first_alias(c("depth_chart_order"), NA_real_))),
    search_rank = suppressWarnings(as.numeric(first_alias(c("search_rank"), NA_real_)))
  )

  out$sleeper_id[is.na(out$sleeper_id)] <- ""
  out$gsis_id[is.na(out$gsis_id)] <- ""
  out$position[is.na(out$position)] <- ""
  out$team[is.na(out$team) | !nzchar(out$team)] <- "FA"
  out
}

sleeper_players_frame <- function(force = FALSE) {
  x <- sleeper_get_players(force)
  if (!length(x)) return(fm3_normalize_sleeper_player_frame(tibble::tibble()))

  # Older Fantasy Model cache files may already contain a flattened player
  # data.frame rather than Sleeper's raw keyed JSON object.  Accept that schema
  # instead of iterating over its columns as if they were players.
  if (is.data.frame(x)) return(fm3_normalize_sleeper_player_frame(x))

  out <- purrr::imap_dfr(x, function(z, id) {
    fantasy_positions <- fm3_null(z$fantasy_positions, character())
    tibble::tibble(
      sleeper_id = as.character(id),
      gsis_id = as.character(fm3_null(z$gsis_id, "")),
      first_name = as.character(fm3_null(z$first_name, "")),
      last_name = as.character(fm3_null(z$last_name, "")),
      full_name = trimws(paste(as.character(fm3_null(z$first_name, "")), as.character(fm3_null(z$last_name, "")))),
      position = as.character(fm3_null(z$position, if (length(fantasy_positions)) fantasy_positions[[1]] else "")),
      team = as.character(fm3_null(z$team, "FA")),
      age = suppressWarnings(as.numeric(fm3_null(z$age, NA))),
      years_exp = suppressWarnings(as.numeric(fm3_null(z$years_exp, NA))),
      active = isTRUE(fm3_null(z$active, FALSE)),
      injury_status = as.character(fm3_null(z$injury_status, "")),
      practice_participation = as.character(fm3_null(z$practice_participation, "")),
      depth_chart_order = suppressWarnings(as.numeric(fm3_null(z$depth_chart_order, NA))),
      search_rank = suppressWarnings(as.numeric(fm3_null(z$search_rank, NA)))
    )
  })
  fm3_normalize_sleeper_player_frame(out)
}

sleeper_rosters_frame <- function(league_id) {
  xs <- sleeper_get_rosters(league_id)
  if (!length(xs)) return(tibble::tibble())
  purrr::map_dfr(xs, function(z) {
    players <- as.character(unlist(fm3_null(z$players, character()), use.names = FALSE))
    starters <- as.character(unlist(fm3_null(z$starters, character()), use.names = FALSE))
    co_owners <- as.character(unlist(fm3_null(z$co_owners, character()), use.names = FALSE))
    if (!length(players)) players <- NA_character_
    tibble::tibble(
      roster_id = suppressWarnings(as.integer(fm3_null(z$roster_id, NA))),
      owner_id = as.character(fm3_null(z$owner_id, "")),
      co_owners = paste(co_owners, collapse = ","),
      sleeper_id = players,
      is_starter = players %in% starters
    )
  })
}

sleeper_users_frame <- function(league_id) {
  xs <- sleeper_get_league_users(league_id)
  if (!length(xs)) return(tibble::tibble())
  purrr::map_dfr(xs, function(z) tibble::tibble(
    user_id = as.character(fm3_null(z$user_id, "")),
    username = as.character(fm3_null(z$username, "")),
    display_name = as.character(fm3_null(z$display_name, "")),
    team_name = as.character(fm3_null(z$metadata$team_name, fm3_null(z$display_name, "")))
  ))
}

sleeper_drafts_frame <- function(league_id) {
  xs <- sleeper_get_drafts(league_id)
  if (!length(xs)) return(tibble::tibble())
  purrr::map_dfr(xs, function(z) tibble::tibble(
    draft_id = as.character(fm3_null(z$draft_id, "")),
    season = suppressWarnings(as.integer(fm3_null(z$season, NA))),
    status = as.character(fm3_null(z$status, "")),
    type = as.character(fm3_null(z$type, "")),
    rounds = suppressWarnings(as.integer(fm3_null(z$settings$rounds, NA))),
    teams = suppressWarnings(as.integer(fm3_null(z$settings$teams, NA))),
    name = as.character(fm3_null(z$metadata$name, "Draft")),
    start_time = suppressWarnings(as.numeric(fm3_null(z$start_time, NA)))
  ))
}

sleeper_draft_picks_frame <- function(draft_id) {
  xs <- sleeper_get_draft_picks(draft_id)
  if (!length(xs)) return(tibble::tibble())
  purrr::map_dfr(xs, function(z) tibble::tibble(
    draft_id = as.character(draft_id),
    sleeper_id = as.character(fm3_null(z$player_id, "")),
    roster_id = suppressWarnings(as.integer(fm3_null(z$roster_id, NA))),
    picked_by = as.character(fm3_null(z$picked_by, "")),
    round = suppressWarnings(as.integer(fm3_null(z$round, NA))),
    draft_slot = suppressWarnings(as.integer(fm3_null(z$draft_slot, NA))),
    pick_no = suppressWarnings(as.integer(fm3_null(z$pick_no, NA))),
    player_name = trimws(paste(as.character(fm3_null(z$metadata$first_name, "")), as.character(fm3_null(z$metadata$last_name, "")))),
    position = as.character(fm3_null(z$metadata$position, "")),
    team = as.character(fm3_null(z$metadata$team, ""))
  )) |>
    dplyr::arrange(pick_no)
}

sleeper_traded_picks_frame <- function(league_id) {
  xs <- sleeper_get_traded_picks(league_id)
  if (!length(xs)) return(tibble::tibble())
  purrr::map_dfr(xs, function(z) tibble::tibble(
    season = suppressWarnings(as.integer(fm3_null(z$season, NA))),
    round = suppressWarnings(as.integer(fm3_null(z$round, NA))),
    original_roster_id = suppressWarnings(as.integer(fm3_null(z$roster_id, NA))),
    previous_owner_id = suppressWarnings(as.integer(fm3_null(z$previous_owner_id, NA))),
    owner_roster_id = suppressWarnings(as.integer(fm3_null(z$owner_id, NA)))
  ))
}

sleeper_save_config <- function(username, user_id, league_id, league_name = "") {
  dir.create(dirname(DYNASTY_SETTINGS_PATH), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(tibble::tibble(
    username = as.character(username), user_id = as.character(user_id),
    league_id = as.character(league_id), league_name = as.character(league_name),
    saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ), DYNASTY_SETTINGS_PATH)
  invisible(DYNASTY_SETTINGS_PATH)
}

sleeper_read_config <- function() {
  if (!file.exists(DYNASTY_SETTINGS_PATH)) return(NULL)
  tryCatch(readr::read_csv(DYNASTY_SETTINGS_PATH, show_col_types = FALSE), error = function(e) NULL)
}



# 3.0.8 MODEL-FIRST MODE -------------------------------------------------
# The live Shiny app never downloads Sleeper's full NFL player catalog.
# A compact identity file is prepared outside Shiny and contains only IDs and
# basic player metadata. Sleeper projections/rankings are not used.

sleeper_sync_compact_players <- function(force = FALSE) {
  if (!isTRUE(force) && file.exists(SLEEPER_COMPACT_PLAYERS_PATH)) {
    x <- tryCatch(readr::read_csv(SLEEPER_COMPACT_PLAYERS_PATH, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
    if (!is.null(x) && nrow(x)) return(fm3_normalize_sleeper_player_frame(x))
  }

  message("[3.0.8 SLEEPER] Syncing player identity catalog outside Shiny...")
  x <- sleeper_players_frame(force = force)
  x <- fm3_normalize_sleeper_player_frame(x)
  x <- x[x[["position"]] %in% c("QB", "RB", "WR", "TE"), , drop = FALSE]
  keep <- intersect(c("sleeper_id", "gsis_id", "first_name", "last_name", "full_name",
                      "position", "team", "age", "years_exp", "active"), names(x))
  x <- x[, keep, drop = FALSE]
  x <- x[!duplicated(x[["sleeper_id"]]) & nzchar(x[["sleeper_id"]]), , drop = FALSE]
  dir.create(dirname(SLEEPER_COMPACT_PLAYERS_PATH), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, SLEEPER_COMPACT_PLAYERS_PATH)
  message("[3.0.8 SLEEPER] Wrote compact identity cache: ", SLEEPER_COMPACT_PLAYERS_PATH,
          " (", nrow(x), " QB/RB/WR/TE players).")
  x
}

sleeper_compact_players_frame <- function(require_ready = TRUE) {
  if (!file.exists(SLEEPER_COMPACT_PLAYERS_PATH)) {
    if (isTRUE(require_ready)) {
      stop("Sleeper compact player identity cache is missing. Run source(\"runners/PREP_DYNASTY_APP_3_0_8.R\") once before launching the app.")
    }
    return(fm3_normalize_sleeper_player_frame(tibble::tibble()))
  }
  x <- tryCatch(readr::read_csv(SLEEPER_COMPACT_PLAYERS_PATH, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
  if (is.null(x)) {
    if (isTRUE(require_ready)) stop("Could not read ", SLEEPER_COMPACT_PLAYERS_PATH, ". Re-run the app preparation script.")
    return(fm3_normalize_sleeper_player_frame(tibble::tibble()))
  }
  fm3_normalize_sleeper_player_frame(x)
}

fm3_clear_sleeper_cache <- function(league_id = NULL, clear_players = FALSE) {
  dir.create(SLEEPER_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(clear_players)) {
    p <- fm3_cache_path("sleeper_players_nfl.rds")
    if (file.exists(p)) unlink(p, force = TRUE)
  }
  if (is.null(league_id) || !nzchar(as.character(league_id))) {
    states <- Sys.glob(file.path(SLEEPER_CACHE_DIR, "league_state_*.rds"))
  } else {
    states <- fm3_cache_path(paste0("league_state_", as.character(league_id), ".rds"))
  }
  states <- states[file.exists(states)]
  if (length(states)) unlink(states, force = TRUE)
  invisible(TRUE)
}

sleeper_build_league_state <- function(league_id, user_id = NULL, force_players = FALSE) {
  league <- sleeper_get_league(league_id)
  rosters <- sleeper_rosters_frame(league_id)
  users <- sleeper_users_frame(league_id)
  # 3.0.8: local compact identity only. No /players/nfl request occurs inside Shiny.
  players <- sleeper_compact_players_frame(require_ready = TRUE)
  drafts <- sleeper_drafts_frame(league_id)
  traded <- sleeper_traded_picks_frame(league_id)

  roster_owners <- rosters |>
    dplyr::distinct(roster_id, owner_id, co_owners) |>
    dplyr::left_join(users, by = c("owner_id" = "user_id")) |>
    dplyr::mutate(team_name = dplyr::coalesce(team_name, display_name, paste0("Roster ", roster_id)))

  roster_players <- rosters |>
    dplyr::left_join(players, by = "sleeper_id") |>
    dplyr::left_join(roster_owners |> dplyr::select(roster_id, team_name), by = "roster_id")

  user_roster_id <- NA_integer_
  if (!is.null(user_id) && nzchar(as.character(user_id))) {
    direct <- roster_owners$roster_id[roster_owners$owner_id == as.character(user_id)]
    if (!length(direct)) {
      co <- roster_owners$roster_id[grepl(paste0("(^|,)", user_id, "(,|$)"), roster_owners$co_owners)]
      direct <- co
    }
    if (length(direct)) user_roster_id <- direct[[1]]
  }

  state <- list(
    schema_version = "3.0.8-model-first",
    synced_at = Sys.time(),
    league_id = as.character(league_id),
    league = league,
    roster_owners = roster_owners,
    roster_players = roster_players,
    players = players,
    drafts = drafts,
    traded_picks = traded,
    user_id = as.character(fm3_null(user_id, "")),
    user_roster_id = user_roster_id
  )
  path <- fm3_cache_path(paste0("league_state_", league_id, ".rds"))
  saveRDS(state, path)
  state
}

sleeper_load_league_state <- function(league_id) {
  path <- fm3_cache_path(paste0("league_state_", league_id, ".rds"))
  if (!file.exists(path)) return(NULL)
  state <- readRDS(path)

  # Migrate cached league states from earlier 3.0 builds.  A stale state should
  # never be allowed to crash the app just because its player frame predates the
  # canonical GSIS-ID column.
  if (!is.null(state$players) && is.data.frame(state$players)) {
    state$players <- fm3_normalize_sleeper_player_frame(state$players)
  }
  if (!is.null(state$roster_players) && is.data.frame(state$roster_players)) {
    rp <- tibble::as_tibble(state$roster_players)
    if (!"sleeper_id" %in% names(rp)) rp$sleeper_id <- rep("", nrow(rp))
    if (!"gsis_id" %in% names(rp)) {
      id_map <- if (!is.null(state$players) && nrow(state$players)) {
        state$players |> dplyr::select(sleeper_id, gsis_id) |> dplyr::distinct(sleeper_id, .keep_all = TRUE)
      } else tibble::tibble(sleeper_id = character(), gsis_id = character())
      rp <- rp |> dplyr::left_join(id_map, by = "sleeper_id")
    }
    state$roster_players <- rp
  }
  state
}
