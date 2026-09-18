# ============================================================
# FANTASY MODEL 3.1 - PREGAME BENCHMARK ARCHIVE ENGINE
# ============================================================
# Captures the final available pre-kickoff projection from Fantasy Model and
# external providers. Rows may be refreshed while the player's game is still
# in the future, but once kickoff passes the archived player-week/provider row
# is never overwritten. This module is evaluation-only and never feeds the
# current week's results back into a forecast.

source("config.R")

bench31_num <- function(x, default = NA_real_) {
  y <- suppressWarnings(as.numeric(x))
  y[!is.finite(y)] <- default
  y
}

bench31_chr <- function(x, default = "") {
  y <- as.character(x)
  y[is.na(y)] <- default
  y
}

bench31_first <- function(x, candidates, default = NA) {
  hit <- candidates[candidates %in% names(x)]
  if (!length(hit)) return(rep(default, nrow(x)))
  x[[hit[[1]]]]
}

bench31_scalar <- function(x, candidates, default = NA_real_) {
  if (is.null(x)) return(default)
  for (nm in candidates) {
    val <- x[[nm]]
    if (!is.null(val) && length(val)) {
      out <- suppressWarnings(as.numeric(val[[1]]))
      if (is.finite(out)) return(out)
    }
  }
  default
}

bench31_scalar_chr <- function(x, candidates, default = "") {
  if (is.null(x)) return(default)
  for (nm in candidates) {
    val <- x[[nm]]
    if (!is.null(val) && length(val)) {
      out <- as.character(val[[1]])
      if (!is.na(out) && nzchar(out)) return(out)
    }
  }
  default
}

bench31_read_csv <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) tibble::tibble())
}

bench31_atomic_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path), fileext = ".tmp")
  readr::write_csv(x, tmp, na = "")
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}

bench31_parse_kickoff <- function(gameday, gametime) {
  gd <- bench31_chr(gameday)
  gt <- bench31_chr(gametime, "00:00:00")
  gt[!nzchar(gt)] <- "00:00:00"
  short_time <- grepl("^\\d{1,2}:\\d{2}$", gt)
  gt[short_time] <- paste0(gt[short_time], ":00")
  as.POSIXct(paste(gd, gt), format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
}

bench31_schedule <- function(season = CURRENT_SEASON) {
  path <- paste0("data/raw/schedules_live_", season, ".csv")
  s <- bench31_read_csv(path)
  if (!nrow(s)) {
    s <- tryCatch(nflreadr::load_schedules(seasons = season), error = function(e) tibble::tibble())
  }
  if (!nrow(s)) return(tibble::tibble())

  n <- nrow(s)
  if (!"week" %in% names(s)) s$week <- rep(NA_integer_, n)
  if (!"home_team" %in% names(s)) s$home_team <- rep("", n)
  if (!"away_team" %in% names(s)) s$away_team <- rep("", n)
  if (!"gameday" %in% names(s)) s$gameday <- rep("", n)
  if (!"gametime" %in% names(s)) s$gametime <- rep("00:00", n)
  if (!"home_score" %in% names(s)) s$home_score <- rep(NA_real_, n)
  if (!"away_score" %in% names(s)) s$away_score <- rep(NA_real_, n)

  s <- s |>
    dplyr::mutate(
      week = as.integer(bench31_num(week)),
      home_team = bench31_chr(home_team),
      away_team = bench31_chr(away_team),
      .kickoff = bench31_parse_kickoff(gameday, gametime),
      .score_started = is.finite(bench31_num(home_score)) & is.finite(bench31_num(away_score))
    ) |>
    dplyr::filter(is.finite(week), nzchar(home_team), nzchar(away_team), is.finite(as.numeric(.kickoff)))

  if (!nrow(s)) return(tibble::tibble())

  home <- s |>
    dplyr::transmute(
      week, team = home_team, opponent = away_team, kickoff = .kickoff,
      score_started = .score_started
    )
  away <- s |>
    dplyr::transmute(
      week, team = away_team, opponent = home_team, kickoff = .kickoff,
      score_started = .score_started
    )

  dplyr::bind_rows(home, away) |>
    dplyr::distinct(week, team, .keep_all = TRUE)
}

bench31_active_week <- function(weekly, schedule_team = NULL, captured_at = Sys.time()) {
  # Player-level is_actual cannot define the active NFL week: backups/inactives can
  # remain is_actual=0 forever. Prefer the first schedule week with a kickoff that
  # has not happened yet; this also handles Thursday/Sunday split weeks correctly.
  if (!is.null(schedule_team) && nrow(schedule_team) &&
      all(c("week", "kickoff") %in% names(schedule_team))) {
    now_num <- as.numeric(as.POSIXct(captured_at, tz = "UTC"))
    sw <- schedule_team |>
      dplyr::mutate(
        .week = as.integer(bench31_num(week)),
        .kick = as.numeric(as.POSIXct(kickoff, tz = "UTC"))
      ) |>
      dplyr::filter(is.finite(.week), is.finite(.kick), .kick > now_num) |>
      dplyr::pull(.week)
    if (length(sw)) return(as.integer(min(sw, na.rm = TRUE)))
  }

  if (!nrow(weekly) || !all(c("week", "position") %in% names(weekly))) return(NA_integer_)
  if (!"is_actual" %in% names(weekly)) weekly$is_actual <- 0
  w <- weekly |>
    dplyr::mutate(.week = as.integer(bench31_num(week)), .actual = bench31_num(is_actual, 0)) |>
    dplyr::filter(position %in% POSITIONS, .actual == 0, is.finite(.week)) |>
    dplyr::pull(.week)
  if (!length(w)) return(NA_integer_)
  min(w, na.rm = TRUE)
}

bench31_identity_map <- function() {
  p <- tryCatch(nflreadr::load_players(), error = function(e) tibble::tibble())
  if (!nrow(p)) return(tibble::tibble())
  p <- tibble::as_tibble(p)
  n <- nrow(p)
  sleeper_id <- bench31_first(p, c("sleeper_id", "sleeper_player_id"), "")
  gsis_id <- bench31_first(p, c("gsis_id", "player_id", "nflverse_id"), "")
  display_name <- bench31_first(p, c("display_name", "full_name", "player_name"), "")
  position <- bench31_first(p, c("position", "position_group"), "")
  team <- bench31_first(p, c("team_abbr", "team", "latest_team"), "")

  tibble::tibble(
    sleeper_id = bench31_chr(sleeper_id),
    player_id = bench31_chr(gsis_id),
    identity_name = bench31_chr(display_name),
    identity_position = bench31_chr(position),
    identity_team = bench31_chr(team)
  ) |>
    dplyr::filter(nzchar(sleeper_id), nzchar(player_id)) |>
    dplyr::distinct(sleeper_id, .keep_all = TRUE)
}

bench31_half_ppr_from_stats <- function(stats) {
  g <- function(nm) bench31_scalar(stats, nm, 0)
  0.04 * g("pass_yd") + 4 * g("pass_td") - 2 * g("pass_int") +
    2 * g(c("pass_2pt", "pass_2pt_conv")) +
    0.10 * g("rush_yd") + 6 * g("rush_td") + 2 * g(c("rush_2pt", "rush_2pt_conv")) +
    0.50 * g(c("rec", "receptions")) + 0.10 * g("rec_yd") + 6 * g("rec_td") +
    2 * g(c("rec_2pt", "rec_2pt_conv")) - 2 * g(c("fum_lost", "fumbles_lost"))
}

bench31_projection_record <- function(z, key = "", requested_position = "") {
  if (is.null(z) || !is.list(z)) return(tibble::tibble())
  stats <- z$stats
  if (is.null(stats) || !is.list(stats)) stats <- z
  player <- z$player
  if (is.null(player) || !is.list(player)) player <- list()

  sleeper_id <- bench31_scalar_chr(z, c("player_id", "id"), "")
  if (!nzchar(sleeper_id)) sleeper_id <- bench31_scalar_chr(player, c("player_id", "id"), "")
  if (!nzchar(sleeper_id) && nzchar(key) && !grepl("^[0-9]+$", requested_position)) sleeper_id <- as.character(key)

  pos <- bench31_scalar_chr(z, c("position", "pos"), "")
  if (!nzchar(pos)) pos <- bench31_scalar_chr(player, c("position", "pos"), requested_position)
  team <- bench31_scalar_chr(z, c("team", "team_abbr"), "")
  if (!nzchar(team)) team <- bench31_scalar_chr(player, c("team", "team_abbr"), "")
  name <- bench31_scalar_chr(z, c("player_name", "full_name", "name"), "")
  if (!nzchar(name)) name <- bench31_scalar_chr(player, c("full_name", "name"), "")

  half <- bench31_scalar(stats, c("pts_half_ppr", "fantasy_points_half_ppr"), NA_real_)
  if (!is.finite(half)) half <- bench31_half_ppr_from_stats(stats)

  tibble::tibble(
    sleeper_id = sleeper_id,
    sleeper_name = name,
    sleeper_position = toupper(pos),
    sleeper_team = team,
    sleeper_projection = half,
    sleeper_pts_std = bench31_scalar(stats, c("pts_std", "fantasy_points_std"), NA_real_),
    sleeper_pts_half_ppr = half,
    sleeper_pts_ppr = bench31_scalar(stats, c("pts_ppr", "fantasy_points_ppr"), NA_real_),
    pass_yd = bench31_scalar(stats, "pass_yd", NA_real_),
    pass_td = bench31_scalar(stats, "pass_td", NA_real_),
    pass_int = bench31_scalar(stats, "pass_int", NA_real_),
    rush_att = bench31_scalar(stats, "rush_att", NA_real_),
    rush_yd = bench31_scalar(stats, "rush_yd", NA_real_),
    rush_td = bench31_scalar(stats, "rush_td", NA_real_),
    rec_tgt = bench31_scalar(stats, c("rec_tgt", "targets"), NA_real_),
    rec = bench31_scalar(stats, c("rec", "receptions"), NA_real_),
    rec_yd = bench31_scalar(stats, "rec_yd", NA_real_),
    rec_td = bench31_scalar(stats, "rec_td", NA_real_),
    fum_lost = bench31_scalar(stats, c("fum_lost", "fumbles_lost"), NA_real_)
  )
}

bench31_normalize_sleeper_payload <- function(payload, requested_position = "") {
  if (is.null(payload) || !length(payload)) return(tibble::tibble())

  if (is.data.frame(payload)) {
    payload <- split(payload, seq_len(nrow(payload)))
  }
  if (!is.list(payload)) return(tibble::tibble())

  rows <- purrr::imap(payload, function(z, key) {
    if (is.data.frame(z) && nrow(z) == 1) z <- as.list(z[1, , drop = FALSE])
    bench31_projection_record(z, key = as.character(key), requested_position = requested_position)
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) return(out)

  out |>
    dplyr::mutate(
      sleeper_id = bench31_chr(sleeper_id),
      sleeper_position = toupper(bench31_chr(sleeper_position)),
      sleeper_team = bench31_chr(sleeper_team)
    ) |>
    dplyr::filter(nzchar(sleeper_id), sleeper_position %in% POSITIONS, is.finite(bench31_num(sleeper_projection))) |>
    dplyr::distinct(sleeper_id, .keep_all = TRUE)
}

bench31_fetch_sleeper_week <- function(season, week, positions = POSITIONS) {
  base <- Sys.getenv(
    "FM_SLEEPER_PROJECTIONS_BASE",
    unset = "https://api.sleeper.com/projections/nfl/regular"
  )
  all_rows <- list()
  errors <- character()

  for (pos in positions) {
    url <- paste0(base, "/", season, "/", week)
    one <- tryCatch({
      req <- httr2::request(url) |>
        httr2::req_user_agent("FantasyModel/3.1 pregame-benchmark") |>
        httr2::req_url_query(position = pos, order_by = "pts_half_ppr") |>
        httr2::req_timeout(30) |>
        httr2::req_retry(max_tries = 3)
      resp <- httr2::req_perform(req)
      if (httr2::resp_status(resp) >= 300) stop("HTTP ", httr2::resp_status(resp))
      payload <- httr2::resp_body_json(resp, simplifyVector = FALSE)
      bench31_normalize_sleeper_payload(payload, requested_position = pos)
    }, error = function(e) {
      errors <<- c(errors, paste0(pos, ": ", conditionMessage(e)))
      tibble::tibble()
    })
    if (nrow(one)) all_rows[[length(all_rows) + 1]] <- one
  }

  out <- dplyr::bind_rows(all_rows)
  if (nrow(out)) out <- out |> dplyr::distinct(sleeper_id, .keep_all = TRUE)
  attr(out, "errors") <- errors
  attr(out, "source_endpoint") <- paste0(base, "/", season, "/", week)
  out
}

bench31_model_rows <- function(weekly, schedule_team, active_week, captured_at) {
  if (!nrow(weekly)) return(tibble::tibble())
  needed <- c("player_id", "player_display_name", "position", "team", "opponent", "projected_weekly_fppg")
  for (nm in needed) if (!nm %in% names(weekly)) weekly[[nm]] <- NA
  if (!"is_actual" %in% names(weekly)) weekly$is_actual <- 0
  for (nm in c("weekly_position_rank", "weekly_floor", "weekly_ceiling", "expected_abs_error", "production_base_31")) {
    if (!nm %in% names(weekly)) weekly[[nm]] <- NA_real_
  }
  for (nm in c("projection_confidence")) if (!nm %in% names(weekly)) weekly[[nm]] <- ""
  if (!"model31_promoted" %in% names(weekly)) weekly$model31_promoted <- FALSE

  d <- weekly |>
    dplyr::mutate(
      week = as.integer(bench31_num(week)),
      is_actual = bench31_num(is_actual, 0),
      player_id = bench31_chr(player_id),
      position = toupper(bench31_chr(position)),
      team = bench31_chr(team)
    ) |>
    dplyr::filter(week == active_week, is_actual == 0, position %in% POSITIONS, nzchar(player_id)) |>
    dplyr::left_join(schedule_team |> dplyr::select(week, team, schedule_opponent = opponent, kickoff, score_started), by = c("week", "team")) |>
    dplyr::filter(is.finite(as.numeric(kickoff)))

  if (!nrow(d)) return(tibble::tibble())

  now_num <- as.numeric(as.POSIXct(captured_at, tz = "UTC"))
  kick_num <- as.numeric(d$kickoff)
  pregame <- kick_num > now_num & !d$score_started
  d <- d[pregame, , drop = FALSE]
  if (!nrow(d)) return(tibble::tibble())

  version <- Sys.getenv("FM_RELEASE_VERSION", unset = "3.1")
  tibble::tibble(
    season = as.integer(CURRENT_SEASON),
    week = as.integer(d$week),
    player_id = bench31_chr(d$player_id),
    sleeper_id = "",
    player_display_name = bench31_chr(d$player_display_name),
    position = bench31_chr(d$position),
    team = bench31_chr(d$team),
    opponent = dplyr::coalesce(bench31_chr(d$opponent), bench31_chr(d$schedule_opponent)),
    kickoff_utc = format(as.POSIXct(d$kickoff, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    provider = "fantasy_model",
    provider_version = version,
    scoring_id = "half_ppr",
    projection = bench31_num(d$projected_weekly_fppg),
    provider_rank = bench31_num(d$weekly_position_rank),
    floor = bench31_num(d$weekly_floor),
    ceiling = bench31_num(d$weekly_ceiling),
    expected_abs_error = bench31_num(d$expected_abs_error),
    projection_confidence = bench31_chr(d$projection_confidence),
    captured_at_utc = format(as.POSIXct(captured_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    minutes_to_kickoff = (kick_num[pregame] - now_num) / 60,
    source_endpoint = "output/weekly_CURRENT_SEASON_projections.csv",
    model31_promoted = as.logical(d$model31_promoted),
    production_base_31 = bench31_num(d$production_base_31),
    sleeper_pts_std = NA_real_, sleeper_pts_half_ppr = NA_real_, sleeper_pts_ppr = NA_real_,
    pass_yd = NA_real_, pass_td = NA_real_, pass_int = NA_real_, rush_att = NA_real_, rush_yd = NA_real_,
    rush_td = NA_real_, rec_tgt = NA_real_, rec = NA_real_, rec_yd = NA_real_, rec_td = NA_real_, fum_lost = NA_real_
  ) |>
    dplyr::filter(is.finite(projection))
}

bench31_sleeper_rows <- function(sleeper, identity, model_rows, captured_at) {
  if (!nrow(sleeper) || !nrow(identity) || !nrow(model_rows)) return(tibble::tibble())
  source_endpoint <- attr(sleeper, "source_endpoint")
  if (is.null(source_endpoint)) source_endpoint <- "https://api.sleeper.com/projections/nfl/regular"

  s <- sleeper |>
    dplyr::left_join(identity, by = "sleeper_id") |>
    dplyr::filter(nzchar(bench31_chr(player_id))) |>
    dplyr::mutate(
      position = dplyr::if_else(nzchar(bench31_chr(sleeper_position)), sleeper_position, identity_position),
      team = dplyr::if_else(nzchar(bench31_chr(sleeper_team)), sleeper_team, identity_team)
    )

  universe <- s |>
    dplyr::filter(position %in% POSITIONS, is.finite(bench31_num(sleeper_projection))) |>
    dplyr::group_by(position) |>
    dplyr::mutate(provider_rank = rank(-bench31_num(sleeper_projection), ties.method = "min", na.last = "keep")) |>
    dplyr::ungroup()

  context <- model_rows |>
    dplyr::select(season, week, player_id, player_display_name, position_model = position, team_model = team,
                  opponent, kickoff_utc, minutes_to_kickoff) |>
    dplyr::distinct(player_id, .keep_all = TRUE)

  d <- universe |>
    dplyr::inner_join(context, by = "player_id")
  if (!nrow(d)) return(tibble::tibble())

  tibble::tibble(
    season = as.integer(d$season),
    week = as.integer(d$week),
    player_id = bench31_chr(d$player_id),
    sleeper_id = bench31_chr(d$sleeper_id),
    player_display_name = dplyr::if_else(nzchar(bench31_chr(d$identity_name)), bench31_chr(d$identity_name), bench31_chr(d$player_display_name)),
    position = bench31_chr(d$position_model),
    team = bench31_chr(d$team_model),
    opponent = bench31_chr(d$opponent),
    kickoff_utc = bench31_chr(d$kickoff_utc),
    provider = "sleeper",
    provider_version = "app-projection-feed",
    scoring_id = "half_ppr",
    projection = bench31_num(d$sleeper_projection),
    provider_rank = bench31_num(d$provider_rank),
    floor = NA_real_, ceiling = NA_real_, expected_abs_error = NA_real_, projection_confidence = "",
    captured_at_utc = format(as.POSIXct(captured_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    minutes_to_kickoff = bench31_num(d$minutes_to_kickoff),
    source_endpoint = source_endpoint,
    model31_promoted = NA,
    production_base_31 = NA_real_,
    sleeper_pts_std = bench31_num(d$sleeper_pts_std),
    sleeper_pts_half_ppr = bench31_num(d$sleeper_pts_half_ppr),
    sleeper_pts_ppr = bench31_num(d$sleeper_pts_ppr),
    pass_yd = bench31_num(d$pass_yd), pass_td = bench31_num(d$pass_td), pass_int = bench31_num(d$pass_int),
    rush_att = bench31_num(d$rush_att), rush_yd = bench31_num(d$rush_yd), rush_td = bench31_num(d$rush_td),
    rec_tgt = bench31_num(d$rec_tgt), rec = bench31_num(d$rec), rec_yd = bench31_num(d$rec_yd),
    rec_td = bench31_num(d$rec_td), fum_lost = bench31_num(d$fum_lost)
  ) |>
    dplyr::filter(is.finite(projection))
}

bench31_normalize_archive_types <- function(d) {
  if (!nrow(d)) return(tibble::as_tibble(d))
  char_cols <- c("player_id", "sleeper_id", "player_display_name", "position", "team", "opponent",
                 "kickoff_utc", "provider", "provider_version", "scoring_id", "projection_confidence",
                 "captured_at_utc", "source_endpoint")
  num_cols <- c("season", "week", "projection", "provider_rank", "floor", "ceiling", "expected_abs_error",
                "minutes_to_kickoff", "production_base_31", "sleeper_pts_std", "sleeper_pts_half_ppr",
                "sleeper_pts_ppr", "pass_yd", "pass_td", "pass_int", "rush_att", "rush_yd", "rush_td",
                "rec_tgt", "rec", "rec_yd", "rec_td", "fum_lost")
  for (nm in intersect(char_cols, names(d))) d[[nm]] <- bench31_chr(d[[nm]])
  for (nm in intersect(num_cols, names(d))) d[[nm]] <- bench31_num(d[[nm]])
  if ("model31_promoted" %in% names(d)) d$model31_promoted <- as.logical(d$model31_promoted)
  tibble::as_tibble(d)
}

bench31_update_archive <- function(existing, candidates) {
  existing <- bench31_normalize_archive_types(existing)
  candidates <- bench31_normalize_archive_types(candidates)
  if (!nrow(candidates)) return(existing)

  key <- function(d) paste(d$season, d$week, d$player_id, d$provider, d$scoring_id, sep = "|")
  cand_key <- key(candidates)
  if (nrow(existing)) existing <- existing[!key(existing) %in% cand_key, , drop = FALSE]

  dplyr::bind_rows(existing, candidates) |>
    dplyr::arrange(season, week, position, provider, provider_rank, player_display_name)
}

bench31_actual_points <- function(season = CURRENT_SEASON) {
  cache <- paste0("data/raw/player_weekly_stats_", season, ".csv")
  raw <- bench31_read_csv(cache)
  if (!nrow(raw)) raw <- tryCatch(nflreadr::load_player_stats(seasons = season, summary_level = "week"), error = function(e) tibble::tibble())
  if (!nrow(raw)) return(tibble::tibble())

  if (exists("normalize_weekly_stats21", mode = "function")) {
    a <- tryCatch(normalize_weekly_stats21(raw), error = function(e) tibble::tibble())
    if (nrow(a) && all(c("week", "player_id", "weekly_fppg") %in% names(a))) {
      return(a |>
        dplyr::transmute(
          week = as.integer(bench31_num(week)), player_id = bench31_chr(player_id),
          actual_points = bench31_num(weekly_fppg)
        ) |>
        dplyr::filter(is.finite(week), nzchar(player_id), is.finite(actual_points)) |>
        dplyr::distinct(week, player_id, .keep_all = TRUE))
    }
  }

  p <- tibble::as_tibble(raw)
  if (!"week" %in% names(p)) return(tibble::tibble())
  pid <- bench31_first(p, c("player_id", "gsis_id"), "")
  val <- 0.04 * bench31_num(bench31_first(p, c("passing_yards", "pass_yd"), 0), 0) +
    4 * bench31_num(bench31_first(p, c("passing_tds", "pass_td"), 0), 0) -
    2 * bench31_num(bench31_first(p, c("interceptions", "pass_int"), 0), 0) +
    0.10 * bench31_num(bench31_first(p, c("rushing_yards", "rush_yd"), 0), 0) +
    6 * bench31_num(bench31_first(p, c("rushing_tds", "rush_td"), 0), 0) +
    0.50 * bench31_num(bench31_first(p, c("receptions", "rec"), 0), 0) +
    0.10 * bench31_num(bench31_first(p, c("receiving_yards", "rec_yd"), 0), 0) +
    6 * bench31_num(bench31_first(p, c("receiving_tds", "rec_td"), 0), 0) -
    2 * bench31_num(bench31_first(p, c("sack_fumbles_lost", "rushing_fumbles_lost", "receiving_fumbles_lost", "fumbles_lost"), 0), 0)

  tibble::tibble(
    week = as.integer(bench31_num(p$week)),
    player_id = bench31_chr(pid),
    actual_points = bench31_num(val)
  ) |>
    dplyr::filter(is.finite(week), nzchar(player_id), is.finite(actual_points)) |>
    dplyr::distinct(week, player_id, .keep_all = TRUE)
}

bench31_score_archive <- function(archive, season = CURRENT_SEASON) {
  if (!nrow(archive)) return(invisible(NULL))
  actual <- bench31_actual_points(season)
  if (!nrow(actual)) return(invisible(NULL))

  scored <- archive |>
    dplyr::inner_join(actual, by = c("week", "player_id")) |>
    dplyr::mutate(
      error = actual_points - projection,
      abs_error = abs(error),
      sq_error = error^2
    )
  if (!nrow(scored)) return(invisible(NULL))

  paired_keys <- scored |>
    dplyr::distinct(week, player_id, provider) |>
    dplyr::count(week, player_id, name = "provider_n") |>
    dplyr::filter(provider_n >= 2) |>
    dplyr::select(week, player_id)
  scored <- scored |>
    dplyr::left_join(paired_keys |> dplyr::mutate(paired_available = TRUE), by = c("week", "player_id")) |>
    dplyr::mutate(paired_available = dplyr::coalesce(paired_available, FALSE))

  safe_cor <- function(a, b) {
    k <- is.finite(a) & is.finite(b)
    if (sum(k) < 4 || stats::sd(a[k]) == 0 || stats::sd(b[k]) == 0) return(NA_real_)
    suppressWarnings(stats::cor(a[k], b[k]))
  }

  metric_one <- function(d, sample_name) {
    if (!nrow(d)) return(tibble::tibble())
    d |>
      dplyr::group_by(provider, position) |>
      dplyr::summarise(
        sample = sample_name,
        n = dplyr::n(),
        MAE = mean(abs_error, na.rm = TRUE),
        RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
        bias = mean(error, na.rm = TRUE),
        correlation = safe_cor(projection, actual_points),
        .groups = "drop"
      )
  }

  metrics <- dplyr::bind_rows(
    metric_one(scored, "all_available"),
    metric_one(scored |> dplyr::filter(paired_available), "paired_only")
  ) |>
    dplyr::arrange(sample, position, provider)

  paired <- scored |>
    dplyr::filter(paired_available, provider %in% c("fantasy_model", "sleeper")) |>
    dplyr::select(week, player_id, player_display_name, position, team, provider, projection, provider_rank, actual_points) |>
    tidyr::pivot_wider(names_from = provider, values_from = c(projection, provider_rank), names_sep = "__")

  bench31_atomic_write_csv(scored, paste0("output/benchmark_scored_long_", season, ".csv"))
  bench31_atomic_write_csv(metrics, paste0("output/benchmark_metrics_", season, ".csv"))
  if (nrow(paired)) bench31_atomic_write_csv(paired, paste0("output/benchmark_paired_", season, ".csv"))
  invisible(list(scored = scored, metrics = metrics, paired = paired))
}
