# ============================================================
# FANTASY MODEL 2.0 - INHERITED TEAM / QB / RECEIVER CONTEXT ENGINE
# ============================================================

safe_num <- function(x) suppressWarnings(as.numeric(x))
safe_rate <- function(num, den) ifelse(is.finite(den) & den > 0, num / den, 0)

ensure_context_col <- function(df, col, default = 0) {
  if (!col %in% names(df)) df[[col]] <- default
  df
}

route_family <- function(route) {
  r <- toupper(trimws(as.character(route)))
  dplyr::case_when(
    r %in% c("GO", "POST", "CORNER", "WHEEL") ~ "vertical",
    r %in% c("SLANT", "IN/DIG", "SHALLOW CROSS/DRAG", "TEXAS/ANGLE") ~ "in_break",
    r %in% c("QUICK OUT", "DEEP OUT") ~ "out_break",
    r %in% c("SCREEN", "SWING") ~ "screen",
    r %in% c("HITCH/CURL") ~ "hitch",
    TRUE ~ "other"
  )
}

write_context_csv <- function(x, path) {
  if (ncol(x) == 0) x <- data.frame(.empty = character())
  readr::write_csv(x, path)
}

# Only these PBP columns are required by the inherited context engine. nflreadr::load_pbp()
# materializes the complete ~372-column table; that is unnecessarily expensive on
# Posit Cloud. The compact reader below downloads one season and projects only the
# fields needed for the fantasy context summaries.
PBP_CONTEXT_COLUMNS <- c(
  "season", "season_type", "game_id", "play_id", "posteam", "qtr", "down", "wp",
  "pass_attempt", "rush_attempt", "qb_dropback", "qb_scramble", "shotgun", "no_huddle",
  "yardline_100", "air_yards", "pass_location", "passer_player_id", "receiver_player_id",
  "complete_pass"
)

load_pbp_compact <- function(season) {
  season <- as.integer(season)
  old_timeout <- getOption("timeout")
  options(timeout = max(900, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)
  parquet_url <- sprintf(
    "https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_%d.parquet",
    season
  )
  csv_url <- sprintf(
    "https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_%d.csv",
    season
  )

  # Arrow is optional. If Posit Cloud already has it, parquet column projection is
  # the most memory-efficient route. We NEVER attempt to install arrow automatically.
  if (requireNamespace("arrow", quietly = TRUE)) {
    tmp <- tempfile(pattern = paste0("pbp_", season, "_"), fileext = ".parquet")
    on.exit(unlink(tmp), add = TRUE)
    cat("[CONTEXT] Downloading official ", season, " PBP parquet...\n", sep = "")
    ok <- tryCatch({
      status <- utils::download.file(parquet_url, tmp, mode = "wb", quiet = TRUE, method = "libcurl")
      identical(status, 0L)
    }, error = function(e) FALSE, warning = function(w) FALSE)

    if (isTRUE(ok) && file.exists(tmp) && file.info(tmp)$size > 0) {
      out <- tryCatch(
        arrow::read_parquet(tmp, col_select = PBP_CONTEXT_COLUMNS, as_data_frame = TRUE),
        error = function(e) NULL
      )
      if (!is.null(out)) {
        cat("[CONTEXT] Compact PBP reader: parquet + selected columns.\n")
        return(as.data.frame(out))
      }
    }
  }

  # Safe fallback: read the season CSV but retain only the ~20 columns needed by
  # the context model. This is slower than parquet but dramatically lighter than
  # materializing the full nflreadr PBP object.
  tmp <- tempfile(pattern = paste0("pbp_", season, "_"), fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  cat("[CONTEXT] Compact PBP reader: CSV + selected columns.\n")
  cat("[CONTEXT] Downloading official ", season, " PBP CSV (only required columns will be retained)...\n", sep = "")
  status <- utils::download.file(csv_url, tmp, mode = "wb", quiet = TRUE, method = "libcurl")
  if (!identical(status, 0L) || !file.exists(tmp) || file.info(tmp)$size <= 0) {
    stop("Could not download the official nflverse PBP file for ", season, ".")
  }
  out <- readr::read_csv(
    tmp,
    col_select = dplyr::all_of(PBP_CONTEXT_COLUMNS),
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )
  as.data.frame(out)
}

summarize_one_pbp_season <- function(season, player_lookup = data.frame()) {
  cat("[CONTEXT] Loading compact play-by-play ", season, "...\n", sep = "")
  pbp <- load_pbp_compact(season)
  if (nrow(pbp) == 0) return(list(team = data.frame(), qb = data.frame(), receiver = data.frame()))

  needed <- PBP_CONTEXT_COLUMNS
  for (nm in needed) pbp <- ensure_context_col(pbp, nm, NA)

  p <- pbp |>
    dplyr::filter(season_type == "REG", !is.na(posteam), posteam != "") |>
    dplyr::mutate(
      season = as.integer(season),
      pass_attempt = dplyr::coalesce(safe_num(pass_attempt), 0), rush_attempt = dplyr::coalesce(safe_num(rush_attempt), 0),
      qb_dropback = dplyr::coalesce(safe_num(qb_dropback), 0), qb_scramble = dplyr::coalesce(safe_num(qb_scramble), 0),
      shotgun = dplyr::coalesce(safe_num(shotgun), 0), no_huddle = dplyr::coalesce(safe_num(no_huddle), 0),
      qtr = safe_num(qtr), down = safe_num(down), wp = safe_num(wp),
      yardline_100 = safe_num(yardline_100), air_yards = safe_num(air_yards),
      complete_pass = safe_num(complete_pass),
      is_dropback = as.numeric(qb_dropback == 1 | pass_attempt == 1),
      is_designed_rush = as.numeric(rush_attempt == 1 & !(qb_scramble == 1)),
      is_off_play = as.numeric(is_dropback == 1 | is_designed_rush == 1),
      is_neutral = as.numeric(qtr <= 3 & down <= 2 & wp >= 0.20 & wp <= 0.80),
      is_redzone = as.numeric(is.finite(yardline_100) & yardline_100 <= 20),
      is_deep = as.numeric(pass_attempt == 1 & is.finite(air_yards) & air_yards >= 15),
      is_short = as.numeric(pass_attempt == 1 & is.finite(air_yards) & air_yards < 5),
      pass_left = as.numeric(pass_attempt == 1 & pass_location == "left"),
      pass_middle = as.numeric(pass_attempt == 1 & pass_location == "middle"),
      pass_right = as.numeric(pass_attempt == 1 & pass_location == "right")
    )

  # Release the raw selected-column frame before any route/participation work.
  rm(pbp)
  invisible(gc(full = TRUE))

  team <- p |>
    dplyr::group_by(season, team = posteam) |>
    dplyr::summarise(
      games = dplyr::n_distinct(game_id),
      plays = sum(is_off_play, na.rm = TRUE),
      dropbacks = sum(is_dropback, na.rm = TRUE),
      designed_rushes = sum(is_designed_rush, na.rm = TRUE),
      neutral_plays = sum(is_off_play * is_neutral, na.rm = TRUE),
      neutral_dropbacks = sum(is_dropback * is_neutral, na.rm = TRUE),
      redzone_plays = sum(is_off_play * is_redzone, na.rm = TRUE),
      redzone_dropbacks = sum(is_dropback * is_redzone, na.rm = TRUE),
      pass_attempts = sum(pass_attempt == 1, na.rm = TRUE),
      deep_attempts = sum(is_deep, na.rm = TRUE),
      left_attempts = sum(pass_left, na.rm = TRUE),
      middle_attempts = sum(pass_middle, na.rm = TRUE),
      right_attempts = sum(pass_right, na.rm = TRUE),
      shotgun_plays = sum(shotgun == 1 & is_off_play == 1, na.rm = TRUE),
      no_huddle_plays = sum(no_huddle == 1 & is_off_play == 1, na.rm = TRUE),
      avg_air_yards = mean(air_yards[pass_attempt == 1 & is.finite(air_yards)], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      team_pass_rate = safe_rate(dropbacks, dropbacks + designed_rushes),
      team_neutral_pass_rate = safe_rate(neutral_dropbacks, neutral_plays),
      team_plays_pg = safe_rate(plays, games),
      team_redzone_pass_rate = safe_rate(redzone_dropbacks, redzone_plays),
      team_deep_throw_rate = safe_rate(deep_attempts, pass_attempts),
      team_left_throw_rate = safe_rate(left_attempts, pass_attempts),
      team_middle_throw_rate = safe_rate(middle_attempts, pass_attempts),
      team_right_throw_rate = safe_rate(right_attempts, pass_attempts),
      team_shotgun_rate = safe_rate(shotgun_plays, plays),
      team_no_huddle_rate = safe_rate(no_huddle_plays, plays),
      avg_air_yards = ifelse(is.finite(avg_air_yards), avg_air_yards, 0)
    ) |>
    dplyr::select(
      season, team, team_pass_rate, team_neutral_pass_rate, team_plays_pg,
      team_redzone_pass_rate, team_deep_throw_rate, team_left_throw_rate,
      team_middle_throw_rate, team_right_throw_rate, team_shotgun_rate,
      team_no_huddle_rate, team_avg_air_yards = avg_air_yards
    )

  qb <- p |>
    dplyr::filter(is_dropback == 1, !is.na(passer_player_id), passer_player_id != "") |>
    dplyr::group_by(season, player_id = as.character(passer_player_id), team = posteam) |>
    dplyr::summarise(
      qb_dropbacks = dplyr::n(),
      qb_attempts = sum(pass_attempt == 1, na.rm = TRUE),
      qb_deep_attempts = sum(is_deep, na.rm = TRUE),
      qb_left_attempts = sum(pass_left, na.rm = TRUE),
      qb_middle_attempts = sum(pass_middle, na.rm = TRUE),
      qb_right_attempts = sum(pass_right, na.rm = TRUE),
      qb_redzone_dropbacks = sum(is_redzone, na.rm = TRUE),
      qb_avg_air_yards = mean(air_yards[pass_attempt == 1 & is.finite(air_yards)], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      qb_deep_throw_rate = safe_rate(qb_deep_attempts, qb_attempts),
      qb_left_throw_rate = safe_rate(qb_left_attempts, qb_attempts),
      qb_middle_throw_rate = safe_rate(qb_middle_attempts, qb_attempts),
      qb_right_throw_rate = safe_rate(qb_right_attempts, qb_attempts),
      qb_redzone_throw_rate = safe_rate(qb_redzone_dropbacks, qb_dropbacks),
      qb_avg_air_yards = ifelse(is.finite(qb_avg_air_yards), qb_avg_air_yards, 0)
    )

  receiver <- p |>
    dplyr::filter(pass_attempt == 1, !is.na(receiver_player_id), receiver_player_id != "") |>
    dplyr::group_by(season, player_id = as.character(receiver_player_id)) |>
    dplyr::summarise(
      rec_context_targets = dplyr::n(),
      rec_avg_depth_target = mean(air_yards[is.finite(air_yards)], na.rm = TRUE),
      rec_deep_targets = sum(is_deep, na.rm = TRUE),
      rec_short_targets = sum(is_short, na.rm = TRUE),
      rec_left_targets = sum(pass_left, na.rm = TRUE),
      rec_middle_targets = sum(pass_middle, na.rm = TRUE),
      rec_right_targets = sum(pass_right, na.rm = TRUE),
      rec_redzone_targets = sum(is_redzone, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      rec_deep_target_rate = safe_rate(rec_deep_targets, rec_context_targets),
      rec_short_target_rate = safe_rate(rec_short_targets, rec_context_targets),
      rec_left_target_rate = safe_rate(rec_left_targets, rec_context_targets),
      rec_middle_target_rate = safe_rate(rec_middle_targets, rec_context_targets),
      rec_right_target_rate = safe_rate(rec_right_targets, rec_context_targets),
      rec_redzone_target_rate = safe_rate(rec_redzone_targets, rec_context_targets),
      rec_avg_depth_target = ifelse(is.finite(rec_avg_depth_target), rec_avg_depth_target, 0)
    )

  # QB positional target preference (WR/TE/RB). This uses only the target ID
  # from PBP and player-position metadata, so it works even when route charting is absent.
  if (nrow(player_lookup) > 0 && all(c("gsis_id", "position") %in% names(player_lookup))) {
    pos_lookup <- player_lookup |>
      dplyr::transmute(receiver_player_id = as.character(gsis_id), receiver_position = toupper(as.character(position))) |>
      dplyr::mutate(receiver_group = dplyr::case_when(
        receiver_position %in% c("RB", "FB") ~ "RB",
        receiver_position == "TE" ~ "TE",
        receiver_position == "WR" ~ "WR",
        TRUE ~ "OTHER"
      )) |>
      dplyr::distinct(receiver_player_id, .keep_all = TRUE)

    qb_pos_pref <- p |>
      dplyr::filter(pass_attempt == 1, !is.na(passer_player_id), passer_player_id != "", !is.na(receiver_player_id), receiver_player_id != "") |>
      dplyr::transmute(
        season, player_id = as.character(passer_player_id), team = posteam,
        receiver_player_id = as.character(receiver_player_id)
      ) |>
      dplyr::left_join(pos_lookup, by = "receiver_player_id") |>
      dplyr::mutate(receiver_group = dplyr::coalesce(receiver_group, "OTHER")) |>
      dplyr::count(season, player_id, team, receiver_group, name = "targets") |>
      dplyr::group_by(season, player_id, team) |>
      dplyr::mutate(total_targets = sum(targets), rate = safe_rate(targets, total_targets)) |>
      dplyr::ungroup() |>
      dplyr::select(-targets, -total_targets) |>
      tidyr::pivot_wider(names_from = receiver_group, values_from = rate, values_fill = 0, names_prefix = "qb_target_pos_")

    qb <- qb |> dplyr::left_join(qb_pos_pref, by = c("season", "player_id", "team"))
  }
  for (nm in c("qb_target_pos_WR", "qb_target_pos_TE", "qb_target_pos_RB")) qb <- ensure_context_col(qb, nm, 0)

  # Join the route chart to targeted passes. nflverse participation provides the
  # route run by the primary receiver on the play, while PBP supplies the target ID.
  cat("[CONTEXT] Loading participation/route chart ", season, "...\n", sep = "")
  participation <- tryCatch(
    nflreadr::load_participation(seasons = season, include_pbp = FALSE),
    error = function(e) {
      warning("Participation/route data unavailable for ", season, ": ", conditionMessage(e))
      data.frame()
    }
  )

  if (nrow(participation) > 0 && all(c("nflverse_game_id", "play_id", "route") %in% names(participation))) {
    route_plays <- participation |>
      dplyr::transmute(
        game_id = as.character(nflverse_game_id),
        play_id = safe_num(play_id),
        route = as.character(route)
      ) |>
      dplyr::filter(!is.na(route), route != "") |>
      dplyr::distinct(game_id, play_id, .keep_all = TRUE)

    targeted <- p |>
      dplyr::filter(pass_attempt == 1, !is.na(receiver_player_id), receiver_player_id != "") |>
      dplyr::transmute(
        game_id = as.character(game_id), play_id = safe_num(play_id), season,
        team = posteam,
        qb_id = as.character(passer_player_id),
        receiver_id = as.character(receiver_player_id)
      ) |>
      dplyr::left_join(route_plays, by = c("game_id", "play_id")) |>
      dplyr::filter(!is.na(route), route != "") |>
      dplyr::mutate(route_family = route_family(route))

    if (nrow(targeted) > 0) {
      rec_routes <- targeted |>
        dplyr::count(season, player_id = receiver_id, route_family, name = "n") |>
        dplyr::group_by(season, player_id) |>
        dplyr::mutate(total = sum(n), rate = safe_rate(n, total)) |>
        dplyr::ungroup() |>
        dplyr::select(-n, -total) |>
        tidyr::pivot_wider(names_from = route_family, values_from = rate, values_fill = 0, names_prefix = "rec_route_")

      qb_routes <- targeted |>
        dplyr::filter(!is.na(qb_id), qb_id != "") |>
        dplyr::count(season, player_id = qb_id, team, route_family, name = "n") |>
        dplyr::group_by(season, player_id, team) |>
        dplyr::mutate(total = sum(n), rate = safe_rate(n, total)) |>
        dplyr::ungroup() |>
        dplyr::select(-n, -total) |>
        tidyr::pivot_wider(names_from = route_family, values_from = rate, values_fill = 0, names_prefix = "qb_route_")

      receiver <- receiver |> dplyr::left_join(rec_routes, by = c("season", "player_id"))
      qb <- qb |> dplyr::left_join(qb_routes, by = c("season", "player_id", "team"))
    }
  }

  route_cols_rec <- paste0("rec_route_", c("vertical", "in_break", "out_break", "screen", "hitch", "other"))
  route_cols_qb <- paste0("qb_route_", c("vertical", "in_break", "out_break", "screen", "hitch", "other"))
  for (nm in route_cols_rec) receiver <- ensure_context_col(receiver, nm, 0)
  for (nm in route_cols_qb) qb <- ensure_context_col(qb, nm, 0)

  list(team = team, qb = qb, receiver = receiver)
}

add_ngs_context <- function(receiver_context, qb_context, seasons) {
  cat("[CONTEXT] Loading compact Next Gen Stats profiles...\n")
  rec_ngs <- tryCatch(
    nflreadr::load_nextgen_stats(seasons = seasons, stat_type = "receiving"),
    error = function(e) { warning("Receiving NGS unavailable: ", conditionMessage(e)); data.frame() }
  )
  pass_ngs <- tryCatch(
    nflreadr::load_nextgen_stats(seasons = seasons, stat_type = "passing"),
    error = function(e) { warning("Passing NGS unavailable: ", conditionMessage(e)); data.frame() }
  )

  if (nrow(rec_ngs) > 0) {
    for (nm in c("player_gsis_id", "season", "week", "team_abbr", "avg_cushion", "avg_separation", "avg_air_distance", "percent_share_of_intended_air_yards")) {
      rec_ngs <- ensure_context_col(rec_ngs, nm, NA)
    }
    rec_keep <- rec_ngs |>
      dplyr::filter(week == 0) |>
      dplyr::transmute(
        season = as.integer(season), player_id = as.character(player_gsis_id),
        rec_ngs_avg_cushion = safe_num(avg_cushion),
        rec_ngs_avg_separation = safe_num(avg_separation),
        rec_ngs_avg_depth = safe_num(avg_air_distance),
        rec_ngs_air_yards_share = safe_num(percent_share_of_intended_air_yards)
      ) |>
      dplyr::distinct(season, player_id, .keep_all = TRUE)
    receiver_context <- receiver_context |> dplyr::left_join(rec_keep, by = c("season", "player_id"))
  }

  if (nrow(pass_ngs) > 0) {
    for (nm in c("player_gsis_id", "season", "week", "team_abbr", "avg_time_to_throw", "avg_intended_air_yards", "aggressiveness", "completion_percentage_above_expectation")) {
      pass_ngs <- ensure_context_col(pass_ngs, nm, NA)
    }
    pass_keep <- pass_ngs |>
      dplyr::filter(week == 0) |>
      dplyr::transmute(
        season = as.integer(season), player_id = as.character(player_gsis_id), team = as.character(team_abbr),
        qb_ngs_time_to_throw = safe_num(avg_time_to_throw),
        qb_ngs_avg_depth = safe_num(avg_intended_air_yards),
        qb_ngs_aggressiveness = safe_num(aggressiveness),
        qb_ngs_cpoe = safe_num(completion_percentage_above_expectation)
      ) |>
      dplyr::distinct(season, player_id, team, .keep_all = TRUE)
    qb_context <- qb_context |> dplyr::left_join(pass_keep, by = c("season", "player_id", "team"))
  }

  list(receiver = receiver_context, qb = qb_context)
}

build_team_context_files <- function(seasons, output_dir = "data/raw", player_lookup = data.frame()) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  checkpoint_dir <- file.path(output_dir, "context_by_season")
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

  team_rows <- list(); qb_rows <- list(); receiver_rows <- list()

  read_checkpoint <- function(path) {
    if (!file.exists(path)) return(NULL)
    tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
  }

  for (s in seasons) {
    team_path <- file.path(checkpoint_dir, paste0("team_context_", s, ".csv"))
    qb_path <- file.path(checkpoint_dir, paste0("qb_context_", s, ".csv"))
    rec_path <- file.path(checkpoint_dir, paste0("receiver_context_", s, ".csv"))

    cached_team <- read_checkpoint(team_path)
    cached_qb <- read_checkpoint(qb_path)
    cached_rec <- read_checkpoint(rec_path)
    complete <- !is.null(cached_team) && !is.null(cached_qb) && !is.null(cached_rec)

    if (complete) {
      cat("[CONTEXT] ", s, " checkpoint found; skipping raw PBP download.\n", sep = "")
      team_rows[[as.character(s)]] <- cached_team
      qb_rows[[as.character(s)]] <- cached_qb
      receiver_rows[[as.character(s)]] <- cached_rec
      next
    }

    one <- tryCatch(
      summarize_one_pbp_season(s, player_lookup = player_lookup),
      error = function(e) {
        warning("Context build failed for ", s, ": ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(one)) {
      stop(
        "Detailed context could not be built for season ", s,
        ". Previous completed seasons were checkpointed. Rerun MOBILE_RUN.R to resume from this season."
      )
    }

    # Checkpoint immediately so a later Posit Cloud restart never discards this work.
    write_context_csv(one$team, team_path)
    write_context_csv(one$qb, qb_path)
    write_context_csv(one$receiver, rec_path)
    cat("[CONTEXT] ", s, " compact context checkpoint saved.\n", sep = "")

    team_rows[[as.character(s)]] <- one$team
    qb_rows[[as.character(s)]] <- one$qb
    receiver_rows[[as.character(s)]] <- one$receiver
    rm(one)
    invisible(gc(full = TRUE))
  }

  team_context <- dplyr::bind_rows(team_rows)
  qb_context <- dplyr::bind_rows(qb_rows)
  receiver_context <- dplyr::bind_rows(receiver_rows)

  ngs <- add_ngs_context(receiver_context, qb_context, seasons)
  receiver_context <- ngs$receiver
  qb_context <- ngs$qb

  # Normalize all optional context values to zero so joins stay model-safe.
  if (nrow(receiver_context) > 0) {
    for (nm in setdiff(names(receiver_context), c("season", "player_id"))) {
      receiver_context[[nm]] <- safe_num(receiver_context[[nm]])
      receiver_context[[nm]][!is.finite(receiver_context[[nm]])] <- 0
    }
  }
  if (nrow(qb_context) > 0) {
    for (nm in setdiff(names(qb_context), c("season", "player_id", "team"))) {
      qb_context[[nm]] <- safe_num(qb_context[[nm]])
      qb_context[[nm]][!is.finite(qb_context[[nm]])] <- 0
    }
  }
  if (nrow(team_context) > 0) {
    for (nm in setdiff(names(team_context), c("season", "team"))) {
      team_context[[nm]] <- safe_num(team_context[[nm]])
      team_context[[nm]][!is.finite(team_context[[nm]])] <- 0
    }
  }

  # Primary-QB team profile: use the previous season QB with the most dropbacks.
  team_qb_context <- if (nrow(qb_context) > 0 && "qb_dropbacks" %in% names(qb_context)) {
    qb_context |>
      dplyr::arrange(season, team, dplyr::desc(qb_dropbacks)) |>
      dplyr::group_by(season, team) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::rename(primary_qb_id = player_id) |>
      dplyr::select(-dplyr::any_of(c("qb_attempts", "qb_deep_attempts", "qb_left_attempts", "qb_middle_attempts", "qb_right_attempts", "qb_redzone_dropbacks")))
  } else data.frame()

  write_context_csv(team_context, file.path(output_dir, "team_context.csv"))
  write_context_csv(qb_context, file.path(output_dir, "qb_context.csv"))
  write_context_csv(receiver_context, file.path(output_dir, "receiver_context.csv"))
  write_context_csv(team_qb_context, file.path(output_dir, "team_qb_context.csv"))

  cat("[CONTEXT] Compact context saved: ", nrow(team_context), " team-seasons, ",
      nrow(qb_context), " QB-seasons, ", nrow(receiver_context), " receiver-seasons.\n", sep = "")
  invisible(list(team = team_context, qb = qb_context, receiver = receiver_context, team_qb = team_qb_context))
}

