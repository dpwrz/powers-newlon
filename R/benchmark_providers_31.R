# ============================================================
# FANTASY MODEL 3.1 - EXTERNAL BENCHMARK PROVIDERS + FAIR SCORER
# ============================================================
# Evaluation-only adapters. Every provider is normalized into the same archive
# schema used by benchmark_archive_31.R. Rank-only providers are allowed: their
# projection is NA and they are scored only on ranking/decision metrics.
#
# Provider policy:
# - Sleeper: numeric Half-PPR app projection feed.
# - ESPN: numeric Half-PPR derived as the midpoint of ESPN's stock Standard and
#   PPR applied totals for the same projected weekly stat line.
# - FantasyPros ECR via nflreadr/DynastyProcess: rank-only unless a separately
#   licensed API feed is configured later.
# - Manual CSV: optional import path for licensed/authorized provider exports.

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x)) y else x
}

bench31_first_nonempty_provider <- function(x) {
  y <- trimws(as.character(x))
  y <- y[!is.na(y) & nzchar(y)]
  if (length(y)) y[[1]] else ""
}

bench31_norm_name <- function(x) {
  y <- iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")
  y[is.na(y)] <- ""
  y <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", y)
  gsub("[^a-z0-9]", "", y)
}

# Override the original identity map with ffverse IDs first. nflreadr::load_players
# is still used for canonical GSIS metadata.
bench31_identity_map <- function() {
  ids <- tryCatch(nflreadr::load_ff_playerids(), error = function(e) tibble::tibble())
  p <- tryCatch(nflreadr::load_players(), error = function(e) tibble::tibble())

  if (nrow(ids)) {
    m <- tibble::tibble(
      player_id = bench31_chr(bench31_first(ids, c("gsis_id", "player_id"), "")),
      sleeper_id = bench31_chr(bench31_first(ids, c("sleeper_id"), "")),
      espn_id = bench31_chr(bench31_first(ids, c("espn_id"), "")),
      fantasypros_id = bench31_chr(bench31_first(ids, c("fantasypros_id", "fp_id"), "")),
      yahoo_id = bench31_chr(bench31_first(ids, c("yahoo_id"), "")),
      identity_name = bench31_chr(bench31_first(ids, c("name", "player_name", "full_name"), "")),
      identity_position = toupper(bench31_chr(bench31_first(ids, c("position", "pos"), ""))),
      identity_team = toupper(bench31_chr(bench31_first(ids, c("team", "tm"), "")))
    )
  } else {
    m <- tibble::tibble()
  }

  if (nrow(p)) {
    meta <- tibble::tibble(
      player_id = bench31_chr(bench31_first(p, c("gsis_id", "player_id", "nflverse_id"), "")),
      meta_name = bench31_chr(bench31_first(p, c("display_name", "full_name", "player_name"), "")),
      meta_position = toupper(bench31_chr(bench31_first(p, c("position", "position_group"), ""))),
      meta_team = toupper(bench31_chr(bench31_first(p, c("team_abbr", "team", "latest_team"), ""))),
      meta_espn_id = bench31_chr(bench31_first(p, c("espn_id"), ""))
    ) |>
      dplyr::filter(nzchar(player_id)) |>
      dplyr::distinct(player_id, .keep_all = TRUE)

    if (!nrow(m)) {
      m <- meta |>
        dplyr::transmute(
          player_id,
          sleeper_id = "",
          espn_id = meta_espn_id,
          fantasypros_id = "",
          yahoo_id = "",
          identity_name = meta_name,
          identity_position = meta_position,
          identity_team = meta_team
        )
    } else {
      m <- m |>
        dplyr::left_join(meta, by = "player_id") |>
        dplyr::mutate(
          identity_name = dplyr::if_else(nzchar(identity_name), identity_name, dplyr::coalesce(meta_name, "")),
          identity_position = dplyr::if_else(nzchar(identity_position), identity_position, dplyr::coalesce(meta_position, "")),
          identity_team = dplyr::if_else(nzchar(identity_team), identity_team, dplyr::coalesce(meta_team, "")),
          espn_id = dplyr::if_else(nzchar(espn_id), espn_id, dplyr::coalesce(meta_espn_id, ""))
        ) |>
        dplyr::select(-dplyr::any_of(c("meta_name", "meta_position", "meta_team", "meta_espn_id")))
    }
  }

  if (!nrow(m)) return(tibble::tibble())

  m |>
    dplyr::mutate(
      player_id = bench31_chr(player_id),
      sleeper_id = bench31_chr(sleeper_id),
      espn_id = bench31_chr(espn_id),
      fantasypros_id = bench31_chr(fantasypros_id),
      yahoo_id = bench31_chr(yahoo_id),
      identity_name = bench31_chr(identity_name),
      identity_position = toupper(bench31_chr(identity_position)),
      identity_team = toupper(bench31_chr(identity_team)),
      normalized_name = bench31_norm_name(identity_name)
    ) |>
    dplyr::filter(nzchar(player_id)) |>
    dplyr::distinct(player_id, .keep_all = TRUE)
}

bench31_external_context <- function(model_rows) {
  model_rows |>
    dplyr::transmute(
      season, week, player_id,
      player_display_name = bench31_chr(player_display_name),
      normalized_name = bench31_norm_name(player_display_name),
      position = toupper(bench31_chr(position)),
      team = bench31_chr(team),
      opponent = bench31_chr(opponent),
      kickoff_utc = bench31_chr(kickoff_utc),
      minutes_to_kickoff = bench31_num(minutes_to_kickoff)
    ) |>
    dplyr::distinct(player_id, .keep_all = TRUE)
}

bench31_attach_external_identity <- function(ext, identity, model_rows, provider_id_col, name_col, pos_col) {
  if (!nrow(ext) || !nrow(model_rows)) return(tibble::tibble())
  ctx <- bench31_external_context(model_rows)

  ext <- tibble::as_tibble(ext)
  ext[[".provider_id"]] <- bench31_chr(ext[[provider_id_col]])
  ext[[".name"]] <- bench31_chr(ext[[name_col]])
  ext[[".pos"]] <- toupper(bench31_chr(ext[[pos_col]]))
  ext[[".norm"]] <- bench31_norm_name(ext[[".name"]])

  id_map <- tibble::tibble()
  if (nrow(identity) && provider_id_col %in% names(identity)) {
    id_map <- identity |>
      dplyr::transmute(
        .provider_id = bench31_chr(.data[[provider_id_col]]),
        player_id = bench31_chr(player_id)
      ) |>
      dplyr::filter(nzchar(.provider_id), nzchar(player_id)) |>
      dplyr::distinct(.provider_id, .keep_all = TRUE)
  }

  direct <- if (nrow(id_map)) {
    ext |>
      dplyr::inner_join(id_map, by = ".provider_id") |>
      dplyr::inner_join(ctx, by = "player_id")
  } else tibble::tibble()

  matched_ids <- if (nrow(direct)) direct[[".provider_id"]] else character()
  remaining <- ext |> dplyr::filter(!.provider_id %in% matched_ids)

  fallback <- tibble::tibble()
  if (nrow(remaining)) {
    ctx_name <- ctx |>
      dplyr::group_by(normalized_name, position) |>
      dplyr::mutate(.candidate_n = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(.candidate_n == 1) |>
      dplyr::select(-.candidate_n)

    fallback <- remaining |>
      dplyr::inner_join(ctx_name, by = c(".norm" = "normalized_name", ".pos" = "position"))
  }

  dplyr::bind_rows(direct, fallback) |>
    dplyr::mutate(
      position = dplyr::if_else(
        nzchar(bench31_chr(position)),
        toupper(bench31_chr(position)),
        toupper(bench31_chr(.pos))
      )
    ) |>
    dplyr::distinct(player_id, .keep_all = TRUE)
}

# ---------------- ESPN -----------------------------------------------------

bench31_espn_position <- function(x) {
  map <- c("1" = "QB", "2" = "RB", "3" = "WR", "4" = "TE")
  y <- unname(map[as.character(x)])
  y[is.na(y)] <- ""
  y
}

bench31_espn_week_total <- function(z, week) {
  if (is.null(z) || !is.list(z)) return(NA_real_)
  stats <- z$playerPoolEntry$stats
  if (is.null(stats)) stats <- z$player$stats
  if (is.null(stats)) stats <- z$stats
  if (is.null(stats) || !length(stats)) return(NA_real_)

  if (is.data.frame(stats)) stats <- split(stats, seq_len(nrow(stats)))
  if (!is.list(stats)) return(NA_real_)

  vals <- purrr::map_dfr(stats, function(s) {
    if (is.data.frame(s) && nrow(s) == 1) s <- as.list(s[1, , drop = FALSE])
    tibble::tibble(
      scoring_period = as.integer(bench31_scalar(s, c("scoringPeriodId"), NA_real_)),
      stat_type = as.integer(bench31_scalar(s, c("statTypeId"), NA_real_)),
      total = bench31_scalar(s, c("appliedTotal"), NA_real_)
    )
  }) |>
    dplyr::filter(scoring_period == as.integer(week), stat_type %in% c(1L, 2L, 3L), is.finite(total))

  if (!nrow(vals)) return(NA_real_)
  priority <- match(vals$stat_type, c(2L, 3L, 1L))
  vals$total[[which.min(priority)]]
}

bench31_parse_espn_payload <- function(payload, week) {
  players <- payload$players
  if (is.null(players) || !length(players)) return(tibble::tibble())
  if (is.data.frame(players)) players <- split(players, seq_len(nrow(players)))

  purrr::map_dfr(players, function(z) {
    if (is.null(z) || !is.list(z)) return(tibble::tibble())
    player <- z$player
    if (is.null(player) || !is.list(player)) player <- list()
    total <- bench31_espn_week_total(z, week)
    if (!is.finite(total)) return(tibble::tibble())
    tibble::tibble(
      espn_id = bench31_scalar_chr(z, c("id", "playerId"), ""),
      espn_name = bench31_scalar_chr(player, c("fullName", "displayName", "name"), ""),
      espn_position = bench31_espn_position(bench31_scalar(player, c("defaultPositionId"), NA_real_)),
      espn_projection = total
    )
  }) |>
    dplyr::filter(nzchar(espn_id), espn_position %in% POSITIONS, is.finite(espn_projection)) |>
    dplyr::distinct(espn_id, .keep_all = TRUE)
}

bench31_fetch_espn_default <- function(season, week, scoring_default_id) {
  url <- paste0(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/",
    season, "/segments/0/leaguedefaults/", scoring_default_id
  )

  filter <- list(players = list(
    filterSlotIds = list(value = c(0, 2, 4, 6)),
    filterStatsForSourceIds = list(value = c(1)),
    filterStatsForSplitTypeIds = list(value = c(0)),
    limit = 1000,
    offset = 0,
    sortPercOwned = list(sortPriority = 1, sortAsc = FALSE)
  ))

  out <- tryCatch({
    req <- httr2::request(url) |>
      httr2::req_url_query(view = "kona_player_info", scoringPeriodId = as.integer(week)) |>
      httr2::req_headers(
        Accept = "application/json",
        `X-Fantasy-Source` = "kona",
        `X-Fantasy-Filter` = jsonlite::toJSON(filter, auto_unbox = TRUE)
      ) |>
      httr2::req_user_agent("FantasyModel/3.1 benchmark ESPN adapter") |>
      httr2::req_timeout(30) |>
      httr2::req_retry(max_tries = 3)
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) >= 300) stop("HTTP ", httr2::resp_status(resp))
    payload <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    parsed <- bench31_parse_espn_payload(payload, week)
    attr(parsed, "source_endpoint") <- paste0(url, "?view=kona_player_info&scoringPeriodId=", week)
    parsed
  }, error = function(e) {
    x <- tibble::tibble()
    attr(x, "error") <- conditionMessage(e)
    attr(x, "source_endpoint") <- paste0(url, "?view=kona_player_info&scoringPeriodId=", week)
    x
  })
  out
}

bench31_merge_espn_defaults <- function(a, b) {
  if (!nrow(a) || !nrow(b)) return(tibble::tibble())
  a |>
    dplyr::rename(
      espn_name_a = espn_name,
      espn_position_a = espn_position,
      projection_a = espn_projection
    ) |>
    dplyr::inner_join(
      b |>
        dplyr::rename(
          espn_name_b = espn_name,
          espn_position_b = espn_position,
          projection_b = espn_projection
        ),
      by = "espn_id"
    ) |>
    dplyr::transmute(
      espn_id,
      espn_name = dplyr::if_else(nzchar(bench31_chr(espn_name_a)), bench31_chr(espn_name_a), bench31_chr(espn_name_b)),
      espn_position = dplyr::if_else(nzchar(bench31_chr(espn_position_a)), bench31_chr(espn_position_a), bench31_chr(espn_position_b)),
      # ESPN stock Standard and PPR scoring differ by reception value. Their
      # midpoint is therefore the exact Half-PPR applied total for the same
      # projected stat line, independent of which preset ID is which.
      espn_projection = (bench31_num(projection_a) + bench31_num(projection_b)) / 2
    ) |>
    dplyr::filter(espn_position %in% POSITIONS, is.finite(espn_projection))
}

bench31_fetch_espn_week <- function(season, week) {
  a <- bench31_fetch_espn_default(season, week, 1)
  b <- bench31_fetch_espn_default(season, week, 3)
  errors <- character()
  if (!nrow(a)) errors <- c(errors, paste0("default 1: ", attr(a, "error") %||% "empty"))
  if (!nrow(b)) errors <- c(errors, paste0("default 3: ", attr(b, "error") %||% "empty"))

  out <- bench31_merge_espn_defaults(a, b)
  if (nrow(out)) {
    out <- out |>
      dplyr::group_by(espn_position) |>
      dplyr::mutate(espn_rank = rank(-espn_projection, ties.method = "min", na.last = "keep")) |>
      dplyr::ungroup()
  }
  attr(out, "errors") <- errors
  attr(out, "source_endpoint") <- paste(
    attr(a, "source_endpoint") %||% "",
    attr(b, "source_endpoint") %||% "",
    sep = " | "
  )
  out
}

bench31_espn_rows <- function(espn, identity, model_rows, captured_at) {
  if (!nrow(espn) || !nrow(model_rows)) return(tibble::tibble())
  d <- bench31_attach_external_identity(
    espn, identity, model_rows,
    provider_id_col = "espn_id",
    name_col = "espn_name",
    pos_col = "espn_position"
  )
  if (!nrow(d)) return(tibble::tibble())

  source_endpoint <- attr(espn, "source_endpoint") %||% "ESPN league-defaults kona_player_info"
  tibble::tibble(
    season = as.integer(d$season),
    week = as.integer(d$week),
    player_id = bench31_chr(d$player_id),
    sleeper_id = "",
    player_display_name = bench31_chr(d$player_display_name),
    position = bench31_chr(d$position),
    team = bench31_chr(d$team),
    opponent = bench31_chr(d$opponent),
    kickoff_utc = bench31_chr(d$kickoff_utc),
    provider = "espn",
    provider_version = "league-defaults-standard-ppr-midpoint",
    scoring_id = "half_ppr",
    projection = bench31_num(d$espn_projection),
    provider_rank = bench31_num(d$espn_rank),
    floor = NA_real_, ceiling = NA_real_, expected_abs_error = NA_real_,
    projection_confidence = "",
    captured_at_utc = format(as.POSIXct(captured_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    minutes_to_kickoff = bench31_num(d$minutes_to_kickoff),
    source_endpoint = source_endpoint,
    capture_mode = "live_pregame",
    model31_promoted = NA,
    production_base_31 = NA_real_,
    sleeper_pts_std = NA_real_, sleeper_pts_half_ppr = NA_real_, sleeper_pts_ppr = NA_real_,
    pass_yd = NA_real_, pass_td = NA_real_, pass_int = NA_real_, rush_att = NA_real_, rush_yd = NA_real_,
    rush_td = NA_real_, rec_tgt = NA_real_, rec = NA_real_, rec_yd = NA_real_, rec_td = NA_real_, fum_lost = NA_real_
  ) |>
    dplyr::filter(is.finite(projection))
}

# ---------------- Optional licensed FantasyPros API -------------------------

bench31_fetch_fantasypros_api <- function(season, week) {
  key <- trimws(Sys.getenv("FM_FANTASYPROS_API_KEY", unset = ""))
  if (!nzchar(key)) {
    x <- tibble::tibble()
    attr(x, "not_configured") <- TRUE
    return(x)
  }

  url <- paste0("https://api.fantasypros.com/public/v2/json/nfl/", season, "/projections")
  out <- tryCatch({
    req <- httr2::request(url) |>
      httr2::req_url_query(
        week = as.integer(week),
        positions = "QB:RB:WR:TE",
        scoring = "HALF"
      ) |>
      httr2::req_headers(`x-api-key` = key) |>
      httr2::req_user_agent("FantasyModel/3.1 licensed FantasyPros benchmark adapter") |>
      httr2::req_timeout(30) |>
      httr2::req_retry(max_tries = 3)
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) >= 300) stop("HTTP ", httr2::resp_status(resp))
    payload <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    players <- payload$players
    if (is.null(players) || !length(players)) return(tibble::tibble())
    if (is.data.frame(players)) players <- split(players, seq_len(nrow(players)))

    parsed <- purrr::map_dfr(players, function(z) {
      if (is.null(z) || !is.list(z)) return(tibble::tibble())
      stats <- z$stats
      if (is.null(stats) || !is.list(stats)) stats <- list()
      pts <- bench31_scalar(z, c("points", "projected_points", "fantasy_points"), NA_real_)
      if (!is.finite(pts)) pts <- bench31_scalar(stats, c("points", "fantasy_points"), NA_real_)
      if (!is.finite(pts)) return(tibble::tibble())
      tibble::tibble(
        fantasypros_id = bench31_scalar_chr(z, c("player_id", "id"), ""),
        fp_name = bench31_scalar_chr(z, c("player_name", "name"), ""),
        fp_position = toupper(bench31_scalar_chr(z, c("position_id", "position", "pos"), "")),
        fp_team = toupper(bench31_scalar_chr(z, c("team_id", "team"), "")),
        fp_projection = pts
      )
    }) |>
      dplyr::filter(nzchar(fantasypros_id), fp_position %in% POSITIONS, is.finite(fp_projection)) |>
      dplyr::group_by(fp_position) |>
      dplyr::mutate(fp_rank = rank(-fp_projection, ties.method = "min", na.last = "keep")) |>
      dplyr::ungroup() |>
      dplyr::distinct(fantasypros_id, .keep_all = TRUE)

    attr(parsed, "source_endpoint") <- paste0(url, "?week=", week, "&positions=QB:RB:WR:TE&scoring=HALF")
    parsed
  }, error = function(e) {
    x <- tibble::tibble()
    attr(x, "error") <- conditionMessage(e)
    attr(x, "source_endpoint") <- url
    x
  })
  out
}

bench31_fantasypros_api_rows <- function(fp, identity, model_rows, captured_at) {
  if (!nrow(fp) || !nrow(model_rows)) return(tibble::tibble())
  d <- bench31_attach_external_identity(
    fp, identity, model_rows,
    provider_id_col = "fantasypros_id",
    name_col = "fp_name",
    pos_col = "fp_position"
  )
  if (!nrow(d)) return(tibble::tibble())

  tibble::tibble(
    season = as.integer(d$season),
    week = as.integer(d$week),
    player_id = bench31_chr(d$player_id),
    sleeper_id = "",
    player_display_name = bench31_chr(d$player_display_name),
    position = bench31_chr(d$position),
    team = bench31_chr(d$team),
    opponent = bench31_chr(d$opponent),
    kickoff_utc = bench31_chr(d$kickoff_utc),
    provider = "fantasypros_api",
    provider_version = "official-api",
    scoring_id = "half_ppr",
    projection = bench31_num(d$fp_projection),
    provider_rank = bench31_num(d$fp_rank),
    floor = NA_real_, ceiling = NA_real_, expected_abs_error = NA_real_,
    projection_confidence = "",
    captured_at_utc = format(as.POSIXct(captured_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    minutes_to_kickoff = bench31_num(d$minutes_to_kickoff),
    source_endpoint = attr(fp, "source_endpoint") %||% "FantasyPros official API",
    capture_mode = "live_pregame",
    model31_promoted = NA,
    production_base_31 = NA_real_,
    sleeper_pts_std = NA_real_, sleeper_pts_half_ppr = NA_real_, sleeper_pts_ppr = NA_real_,
    pass_yd = NA_real_, pass_td = NA_real_, pass_int = NA_real_, rush_att = NA_real_, rush_yd = NA_real_,
    rush_td = NA_real_, rec_tgt = NA_real_, rec = NA_real_, rec_yd = NA_real_, rec_td = NA_real_, fum_lost = NA_real_
  ) |>
    dplyr::filter(is.finite(projection))
}

# ---------------- FantasyPros ECR open-data snapshot -----------------------

bench31_fetch_fantasypros_ecr <- function(captured_at = Sys.time(), max_age_days = 5) {
  x <- tryCatch(nflreadr::load_ff_rankings(type = "week"), error = function(e) {
    z <- tibble::tibble()
    attr(z, "error") <- conditionMessage(e)
    z
  })
  if (!nrow(x)) return(x)

  needed <- c("player_name", "fantasypros_id", "pos", "team", "ecr", "scrape_date")
  for (nm in needed) if (!nm %in% names(x)) x[[nm]] <- NA

  scrape <- suppressWarnings(as.Date(x$scrape_date))
  fresh <- is.na(scrape) | scrape >= as.Date(captured_at) - as.integer(max_age_days)
  x <- x[fresh, , drop = FALSE]

  out <- tibble::tibble(
    fantasypros_id = bench31_chr(x$fantasypros_id),
    fp_name = bench31_chr(x$player_name),
    fp_position = toupper(bench31_chr(x$pos)),
    fp_team = toupper(bench31_chr(x$team)),
    fp_rank = bench31_num(x$ecr),
    scrape_date = as.character(x$scrape_date)
  ) |>
    dplyr::filter(fp_position %in% POSITIONS, is.finite(fp_rank)) |>
    dplyr::arrange(fp_position, fp_rank) |>
    dplyr::distinct(fantasypros_id, fp_name, fp_position, .keep_all = TRUE)

  attr(out, "source_endpoint") <- "nflreadr::load_ff_rankings(type='week') / DynastyProcess fp_latest_weekly"
  if (!nrow(out)) attr(out, "error") <- "FantasyPros ECR snapshot was missing or stale"
  out
}

bench31_fantasypros_rows <- function(fp, identity, model_rows, captured_at) {
  if (!nrow(fp) || !nrow(model_rows)) return(tibble::tibble())
  d <- bench31_attach_external_identity(
    fp, identity, model_rows,
    provider_id_col = "fantasypros_id",
    name_col = "fp_name",
    pos_col = "fp_position"
  )
  if (!nrow(d)) return(tibble::tibble())

  source_endpoint <- attr(fp, "source_endpoint") %||% "DynastyProcess FantasyPros ECR"
  version <- bench31_first_nonempty_provider(d$scrape_date)
  if (!nzchar(version)) version <- "weekly-open-data"

  tibble::tibble(
    season = as.integer(d$season),
    week = as.integer(d$week),
    player_id = bench31_chr(d$player_id),
    sleeper_id = "",
    player_display_name = bench31_chr(d$player_display_name),
    position = bench31_chr(d$position),
    team = bench31_chr(d$team),
    opponent = bench31_chr(d$opponent),
    kickoff_utc = bench31_chr(d$kickoff_utc),
    provider = "fantasypros_ecr",
    provider_version = version,
    # This open-data weekly snapshot does not expose the scoring setting.
    # Treat it as rank-only and never calculate point-error metrics from it.
    scoring_id = "rank_only_unspecified",
    projection = NA_real_,
    provider_rank = bench31_num(d$fp_rank),
    floor = NA_real_, ceiling = NA_real_, expected_abs_error = NA_real_,
    projection_confidence = "",
    captured_at_utc = format(as.POSIXct(captured_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    minutes_to_kickoff = bench31_num(d$minutes_to_kickoff),
    source_endpoint = source_endpoint,
    capture_mode = "live_pregame",
    model31_promoted = NA,
    production_base_31 = NA_real_,
    sleeper_pts_std = NA_real_, sleeper_pts_half_ppr = NA_real_, sleeper_pts_ppr = NA_real_,
    pass_yd = NA_real_, pass_td = NA_real_, pass_int = NA_real_, rush_att = NA_real_, rush_yd = NA_real_,
    rush_td = NA_real_, rec_tgt = NA_real_, rec = NA_real_, rec_yd = NA_real_, rec_td = NA_real_, fum_lost = NA_real_
  ) |>
    dplyr::filter(is.finite(provider_rank))
}

# ---------------- Manual licensed/authorized imports -----------------------

bench31_manual_rows <- function(season, week, model_rows, captured_at, dir = "data/benchmarks/manual") {
  if (!dir.exists(dir) || !nrow(model_rows)) return(tibble::tibble())
  files <- Sys.glob(file.path(dir, "*.csv"))
  if (!length(files)) return(tibble::tibble())

  raw <- purrr::map_dfr(files, function(path) {
    d <- bench31_read_csv(path)
    if (!nrow(d)) return(tibble::tibble())
    d$source_file <- path
    d
  })
  if (!nrow(raw)) return(tibble::tibble())

  for (nm in c("season", "week", "provider", "scoring_id", "player_id", "player_name", "position", "projection", "provider_rank")) {
    if (!nm %in% names(raw)) raw[[nm]] <- NA
  }

  target_season <- as.integer(season)
  target_week <- as.integer(week)

  raw <- raw |>
    dplyr::mutate(
      season = as.integer(bench31_num(season)),
      week = as.integer(bench31_num(week)),
      provider = bench31_chr(provider),
      scoring_id = bench31_chr(scoring_id),
      player_id = bench31_chr(player_id),
      player_name = bench31_chr(player_name),
      position = toupper(bench31_chr(position)),
      projection = bench31_num(projection),
      provider_rank = bench31_num(provider_rank),
      source_file = bench31_chr(source_file)
    ) |>
    dplyr::filter(season == target_season, week == target_week, nzchar(provider), position %in% POSITIONS)

  if (!nrow(raw)) return(tibble::tibble())
  ctx <- bench31_external_context(model_rows)

  direct <- raw |>
    dplyr::filter(nzchar(player_id)) |>
    dplyr::inner_join(ctx, by = c("season", "week", "player_id", "position"), suffix = c("", "_ctx"))

  remaining <- raw |>
    dplyr::filter(!nzchar(player_id)) |>
    dplyr::mutate(normalized_name = bench31_norm_name(player_name))
  ctx_name <- ctx |>
    dplyr::group_by(normalized_name, position) |>
    dplyr::mutate(.candidate_n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(.candidate_n == 1) |>
    dplyr::select(-.candidate_n)
  fallback <- remaining |>
    dplyr::inner_join(ctx_name, by = c("season", "week", "normalized_name", "position"))

  d <- dplyr::bind_rows(direct, fallback)
  if (!nrow(d)) return(tibble::tibble())

  d |>
    dplyr::transmute(
      season = as.integer(season),
      week = as.integer(week),
      player_id = bench31_chr(player_id),
      sleeper_id = "",
      player_display_name = bench31_chr(player_display_name),
      position = bench31_chr(position),
      team = bench31_chr(team),
      opponent = bench31_chr(opponent),
      kickoff_utc = bench31_chr(kickoff_utc),
      provider = bench31_chr(provider),
      provider_version = "manual-authorized-import",
      scoring_id = dplyr::if_else(
        nzchar(scoring_id), scoring_id,
        dplyr::if_else(is.finite(projection), "unknown_scoring", "rank_only_unspecified")
      ),
      projection = bench31_num(projection),
      provider_rank = bench31_num(provider_rank),
      floor = NA_real_, ceiling = NA_real_, expected_abs_error = NA_real_,
      projection_confidence = "",
      captured_at_utc = format(as.POSIXct(captured_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      minutes_to_kickoff = bench31_num(minutes_to_kickoff),
      source_endpoint = bench31_chr(source_file),
      capture_mode = "live_pregame",
      model31_promoted = NA,
      production_base_31 = NA_real_,
      sleeper_pts_std = NA_real_, sleeper_pts_half_ppr = NA_real_, sleeper_pts_ppr = NA_real_,
      pass_yd = NA_real_, pass_td = NA_real_, pass_int = NA_real_, rush_att = NA_real_, rush_yd = NA_real_,
      rush_td = NA_real_, rec_tgt = NA_real_, rec = NA_real_, rec_yd = NA_real_, rec_td = NA_real_, fum_lost = NA_real_
    ) |>
    dplyr::filter(is.finite(projection) | is.finite(provider_rank))
}

# ---------------- Unified capture bundle ----------------------------------

bench31_capture_external_providers <- function(season, week, identity, model_rows, captured_at) {
  rows <- list()
  status <- list()

  add_status <- function(provider, n, ok, message) {
    status[[length(status) + 1]] <<- tibble::tibble(
      provider = provider,
      eligible_pregame_rows = nrow(model_rows),
      rows_captured = as.integer(n),
      status = if (n > 0) "ok" else if (ok) "no_matched_rows" else "unavailable",
      message = message
    )
  }

  sleeper <- bench31_fetch_sleeper_week(season, week)
  sr <- bench31_sleeper_rows(sleeper, identity, model_rows, captured_at)
  if (nrow(sr)) {
    sr$capture_mode <- "live_pregame"
    rows[[length(rows) + 1]] <- sr
  }
  se <- attr(sleeper, "errors")
  add_status(
    "sleeper", nrow(sr), !length(se),
    if (nrow(sr)) paste0("Captured Sleeper Half-PPR projections from ", attr(sleeper, "source_endpoint") %||% "projection feed")
    else if (length(se)) paste(se, collapse = " | ")
    else "Sleeper feed returned no matched rows"
  )

  espn <- bench31_fetch_espn_week(season, week)
  er <- bench31_espn_rows(espn, identity, model_rows, captured_at)
  if (nrow(er)) rows[[length(rows) + 1]] <- er
  ee <- attr(espn, "errors")
  add_status(
    "espn", nrow(er), !length(ee),
    if (nrow(er)) "Captured ESPN Standard/PPR midpoint projections as derived Half-PPR"
    else if (length(ee)) paste(ee, collapse = " | ")
    else "ESPN feed returned no matched rows"
  )

  fp_api <- bench31_fetch_fantasypros_api(season, week)
  if (!isTRUE(attr(fp_api, "not_configured"))) {
    far <- bench31_fantasypros_api_rows(fp_api, identity, model_rows, captured_at)
    if (nrow(far)) rows[[length(rows) + 1]] <- far
    fae <- attr(fp_api, "error")
    add_status(
      "fantasypros_api", nrow(far), is.null(fae) || !nzchar(fae),
      if (nrow(far)) "Captured official FantasyPros Half-PPR projections"
      else if (!is.null(fae) && nzchar(fae)) fae
      else "FantasyPros API returned no matched rows"
    )
  }

  fp <- bench31_fetch_fantasypros_ecr(captured_at)
  fr <- bench31_fantasypros_rows(fp, identity, model_rows, captured_at)
  if (nrow(fr)) rows[[length(rows) + 1]] <- fr
  fe <- attr(fp, "error")
  add_status(
    "fantasypros_ecr", nrow(fr), is.null(fe) || !nzchar(fe),
    if (nrow(fr)) "Captured current FantasyPros ECR open-data snapshot (rank-only)"
    else if (!is.null(fe) && nzchar(fe)) fe
    else "FantasyPros ECR returned no matched rows"
  )

  manual <- bench31_manual_rows(season, week, model_rows, captured_at)
  if (nrow(manual)) {
    rows[[length(rows) + 1]] <- manual
    for (pv in unique(manual$provider)) {
      add_status(
        pv,
        sum(manual$provider == pv),
        TRUE,
        "Captured licensed/authorized manual benchmark import"
      )
    }
  }

  list(
    rows = dplyr::bind_rows(rows),
    status = dplyr::bind_rows(status)
  )
}

# ---------------- Historical week-addressable backfill ---------------------

bench31_capture_backfill_providers <- function(season, week, identity, model_rows, captured_at = Sys.time()) {
  rows <- list()
  status <- list()

  add_status <- function(provider, n, message) {
    status[[length(status) + 1]] <<- tibble::tibble(
      provider = provider,
      week = as.integer(week),
      rows_captured = as.integer(n),
      status = if (n > 0) "ok" else "unavailable",
      message = message
    )
  }

  sleeper <- bench31_fetch_sleeper_week(season, week)
  sr <- bench31_sleeper_rows(sleeper, identity, model_rows, captured_at)
  if (nrow(sr)) {
    sr$capture_mode <- "historical_backfill"
    rows[[length(rows) + 1]] <- sr
  }
  se <- attr(sleeper, "errors")
  add_status(
    "sleeper", nrow(sr),
    if (nrow(sr)) "Historical week-addressed Sleeper projection backfill"
    else if (length(se)) paste(se, collapse = " | ")
    else "No historical Sleeper rows matched"
  )

  espn <- bench31_fetch_espn_week(season, week)
  er <- bench31_espn_rows(espn, identity, model_rows, captured_at)
  if (nrow(er)) {
    er$capture_mode <- "historical_backfill"
    rows[[length(rows) + 1]] <- er
  }
  ee <- attr(espn, "errors")
  add_status(
    "espn", nrow(er),
    if (nrow(er)) "Historical week-addressed ESPN projection backfill"
    else if (length(ee)) paste(ee, collapse = " | ")
    else "No historical ESPN rows matched"
  )

  list(rows = dplyr::bind_rows(rows), status = dplyr::bind_rows(status))
}

bench31_append_missing_archive <- function(existing, candidates) {
  existing <- bench31_normalize_archive_types(existing)
  candidates <- bench31_normalize_archive_types(candidates)
  if (!nrow(candidates)) return(existing)
  key <- function(d) paste(d$season, d$week, d$player_id, d$provider, d$scoring_id, sep = "|")
  existing_keys <- if (nrow(existing)) key(existing) else character()
  candidates <- candidates[!key(candidates) %in% existing_keys, , drop = FALSE]
  dplyr::bind_rows(existing, candidates) |>
    dplyr::arrange(season, week, position, provider, provider_rank, player_display_name)
}

# ---------------- Provider-neutral scoring --------------------------------

bench31_safe_cor <- function(a, b, method = "pearson") {
  a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
  k <- is.finite(a) & is.finite(b)
  if (sum(k) < 4 || stats::sd(a[k]) == 0 || stats::sd(b[k]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(a[k], b[k], method = method))
}

bench31_pair_accuracy <- function(pred_rank, actual_rank) {
  k <- is.finite(pred_rank) & is.finite(actual_rank)
  pred_rank <- pred_rank[k]; actual_rank <- actual_rank[k]
  n <- length(pred_rank)
  if (n < 2) return(c(correct = 0, pairs = 0, accuracy = NA_real_))
  correct <- 0; pairs <- 0
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      if (actual_rank[[i]] == actual_rank[[j]] || pred_rank[[i]] == pred_rank[[j]]) next
      pairs <- pairs + 1
      if ((pred_rank[[i]] - pred_rank[[j]]) * (actual_rank[[i]] - actual_rank[[j]]) > 0) correct <- correct + 1
    }
  }
  c(correct = correct, pairs = pairs, accuracy = if (pairs) correct / pairs else NA_real_)
}

bench31_cutoff <- function(position, kind = c("starter", "relevant", "topn")) {
  kind <- match.arg(kind)
  maps <- list(
    starter = c(QB = 12, RB = 24, WR = 36, TE = 12),
    relevant = c(QB = 24, RB = 36, WR = 48, TE = 24),
    topn = c(QB = 12, RB = 24, WR = 24, TE = 12)
  )
  unname(maps[[kind]][[as.character(position)]])
}

bench31_metric_group <- function(d) {
  pos <- d$position[[1]]
  starter_cut <- bench31_cutoff(pos, "starter")
  relevant_cut <- bench31_cutoff(pos, "relevant")
  topn <- bench31_cutoff(pos, "topn")

  projection_n <- sum(is.finite(d$projection))
  rank_n <- sum(is.finite(d$effective_rank))
  point <- d[is.finite(d$projection), , drop = FALSE]
  starter <- point[is.finite(point$effective_rank) & point$effective_rank <= starter_cut, , drop = FALSE]
  relevant <- point[is.finite(point$effective_rank) & point$effective_rank <= relevant_cut, , drop = FALSE]

  rel_rank <- d[is.finite(d$effective_rank) & d$effective_rank <= relevant_cut, , drop = FALSE]
  pa <- bench31_pair_accuracy(rel_rank$effective_rank, rel_rank$actual_rank)

  top_hit <- NA_real_
  if (rank_n >= topn && nrow(d) >= topn) {
    pred <- which(is.finite(d$effective_rank) & d$effective_rank <= topn)
    act <- which(is.finite(d$actual_rank) & d$actual_rank <= topn)
    top_hit <- length(intersect(pred, act)) / topn
  }

  tibble::tibble(
    n = nrow(d),
    projection_n = projection_n,
    rank_n = rank_n,
    MAE = if (projection_n) mean(abs(point$actual_points - point$projection)) else NA_real_,
    RMSE = if (projection_n) sqrt(mean((point$actual_points - point$projection)^2)) else NA_real_,
    bias = if (projection_n) mean(point$actual_points - point$projection) else NA_real_,
    correlation = if (projection_n) bench31_safe_cor(point$projection, point$actual_points) else NA_real_,
    rank_correlation = if (rank_n) bench31_safe_cor(d$effective_rank, d$actual_rank, "spearman") else NA_real_,
    starter_n = nrow(starter),
    starter_MAE = if (nrow(starter)) mean(abs(starter$actual_points - starter$projection)) else NA_real_,
    relevant_n = nrow(relevant),
    relevant_MAE = if (nrow(relevant)) mean(abs(relevant$actual_points - relevant$projection)) else NA_real_,
    top_n = topn,
    top_n_hit_rate = top_hit,
    start_sit_pairs = as.integer(pa[["pairs"]]),
    start_sit_accuracy = unname(pa[["accuracy"]])
  )
}

bench31_pairwise_group <- function(d) {
  pos <- d$position[[1]]
  starter_cut <- bench31_cutoff(pos, "starter")
  relevant_cut <- bench31_cutoff(pos, "relevant")
  topn <- bench31_cutoff(pos, "topn")

  d <- d |>
    dplyr::mutate(
      model_rank = rank(-model_projection, ties.method = "min", na.last = "keep"),
      external_rank = dplyr::if_else(
        is.finite(external_provider_rank),
        rank(external_provider_rank, ties.method = "min", na.last = "keep"),
        rank(-external_projection, ties.method = "min", na.last = "keep")
      ),
      actual_rank = rank(-actual_points, ties.method = "min", na.last = "keep")
    )

  point <- d[is.finite(d$model_projection) & is.finite(d$external_projection), , drop = FALSE]
  starter_union <- d[
    (is.finite(d$model_rank) & d$model_rank <= starter_cut) |
      (is.finite(d$external_rank) & d$external_rank <= starter_cut),
    , drop = FALSE
  ]
  relevant_union <- d[
    (is.finite(d$model_rank) & d$model_rank <= relevant_cut) |
      (is.finite(d$external_rank) & d$external_rank <= relevant_cut),
    , drop = FALSE
  ]

  starter_point <- starter_union[
    is.finite(starter_union$model_projection) & is.finite(starter_union$external_projection),
    , drop = FALSE
  ]
  relevant_point <- relevant_union[
    is.finite(relevant_union$model_projection) & is.finite(relevant_union$external_projection),
    , drop = FALSE
  ]

  mpa <- bench31_pair_accuracy(relevant_union$model_rank, relevant_union$actual_rank)
  epa <- bench31_pair_accuracy(relevant_union$external_rank, relevant_union$actual_rank)

  model_top <- external_top <- NA_real_
  if (nrow(d) >= topn && sum(is.finite(d$external_rank)) >= topn) {
    actual_idx <- which(d$actual_rank <= topn)
    model_idx <- which(d$model_rank <= topn)
    external_idx <- which(d$external_rank <= topn)
    model_top <- length(intersect(actual_idx, model_idx)) / topn
    external_top <- length(intersect(actual_idx, external_idx)) / topn
  }

  model_mae <- if (nrow(point)) mean(abs(point$actual_points - point$model_projection)) else NA_real_
  ext_mae <- if (nrow(point)) mean(abs(point$actual_points - point$external_projection)) else NA_real_
  model_rmse <- if (nrow(point)) sqrt(mean((point$actual_points - point$model_projection)^2)) else NA_real_
  ext_rmse <- if (nrow(point)) sqrt(mean((point$actual_points - point$external_projection)^2)) else NA_real_
  model_starter_mae <- if (nrow(starter_point)) mean(abs(starter_point$actual_points - starter_point$model_projection)) else NA_real_
  ext_starter_mae <- if (nrow(starter_point)) mean(abs(starter_point$actual_points - starter_point$external_projection)) else NA_real_
  model_relevant_mae <- if (nrow(relevant_point)) mean(abs(relevant_point$actual_points - relevant_point$model_projection)) else NA_real_
  ext_relevant_mae <- if (nrow(relevant_point)) mean(abs(relevant_point$actual_points - relevant_point$external_projection)) else NA_real_

  tibble::tibble(
    n_common = nrow(d),
    point_n_common = nrow(point),
    starter_union_n = nrow(starter_union),
    relevant_union_n = nrow(relevant_union),
    model_MAE = model_mae,
    provider_MAE = ext_mae,
    model_MAE_advantage = ext_mae - model_mae,
    model_RMSE = model_rmse,
    provider_RMSE = ext_rmse,
    model_RMSE_advantage = ext_rmse - model_rmse,
    model_bias = if (nrow(point)) mean(point$actual_points - point$model_projection) else NA_real_,
    provider_bias = if (nrow(point)) mean(point$actual_points - point$external_projection) else NA_real_,
    model_correlation = if (nrow(point)) bench31_safe_cor(point$model_projection, point$actual_points) else NA_real_,
    provider_correlation = if (nrow(point)) bench31_safe_cor(point$external_projection, point$actual_points) else NA_real_,
    model_rank_correlation = bench31_safe_cor(d$model_rank, d$actual_rank, "spearman"),
    provider_rank_correlation = bench31_safe_cor(d$external_rank, d$actual_rank, "spearman"),
    model_starter_MAE = model_starter_mae,
    provider_starter_MAE = ext_starter_mae,
    model_starter_MAE_advantage = ext_starter_mae - model_starter_mae,
    model_relevant_MAE = model_relevant_mae,
    provider_relevant_MAE = ext_relevant_mae,
    model_relevant_MAE_advantage = ext_relevant_mae - model_relevant_mae,
    top_n = topn,
    model_top_n_hit_rate = model_top,
    provider_top_n_hit_rate = external_top,
    model_start_sit_pairs = as.integer(mpa[["pairs"]]),
    provider_start_sit_pairs = as.integer(epa[["pairs"]]),
    model_start_sit_accuracy = unname(mpa[["accuracy"]]),
    provider_start_sit_accuracy = unname(epa[["accuracy"]]),
    model_start_sit_advantage = unname(mpa[["accuracy"]] - epa[["accuracy"]])
  )
}

# Override original scorer with provider-neutral metrics and pairwise common-cohort
# comparisons against Fantasy Model.
bench31_score_archive <- function(archive, season = CURRENT_SEASON) {
  if (!nrow(archive)) return(invisible(NULL))
  actual <- bench31_actual_points(season)
  if (!nrow(actual)) return(invisible(NULL))

  if (!"capture_mode" %in% names(archive)) archive$capture_mode <- ""
  archive$capture_mode <- bench31_chr(archive$capture_mode)
  archive$capture_mode[!nzchar(archive$capture_mode) & archive$provider == "fantasy_model"] <- "live_pregame"
  archive$capture_mode[!nzchar(archive$capture_mode)] <- "unknown"

  scored <- archive |>
    dplyr::inner_join(actual, by = c("week", "player_id")) |>
    dplyr::mutate(
      projection = bench31_num(projection),
      provider_rank = bench31_num(provider_rank),
      error = dplyr::if_else(is.finite(projection), actual_points - projection, NA_real_),
      abs_error = abs(error),
      sq_error = error^2
    ) |>
    dplyr::group_by(week, provider, scoring_id, capture_mode, position) |>
    dplyr::mutate(
      effective_rank = dplyr::if_else(
        is.finite(provider_rank),
        provider_rank,
        rank(-projection, ties.method = "min", na.last = "keep")
      ),
      actual_rank = rank(-actual_points, ties.method = "min", na.last = "keep")
    ) |>
    dplyr::ungroup()

  metrics <- scored |>
    dplyr::group_by(week, provider, scoring_id, capture_mode, position) |>
    dplyr::group_modify(~bench31_metric_group(.x), .keep = TRUE) |>
    dplyr::ungroup() |>
    dplyr::arrange(week, position, provider, scoring_id)

  model <- scored |>
    dplyr::filter(provider == "fantasy_model", scoring_id == "half_ppr") |>
    dplyr::select(
      week, player_id, position, actual_points,
      model_projection = projection,
      model_provider_rank = effective_rank
    )

  external <- scored |>
    dplyr::filter(provider != "fantasy_model") |>
    dplyr::select(
      week, player_id, position, provider, scoring_id, capture_mode,
      external_projection = projection,
      external_provider_rank = effective_rank
    )

  paired_players <- external |>
    dplyr::inner_join(model, by = c("week", "player_id", "position"))

  pairwise <- if (nrow(paired_players)) {
    paired_players |>
      dplyr::group_by(week, provider, scoring_id, capture_mode, position) |>
      dplyr::group_modify(~bench31_pairwise_group(.x), .keep = TRUE) |>
      dplyr::ungroup() |>
      dplyr::arrange(week, position, provider, scoring_id)
  } else tibble::tibble()

  bench31_atomic_write_csv(scored, paste0("output/benchmark_scored_long_", season, ".csv"))
  bench31_atomic_write_csv(metrics, paste0("output/benchmark_metrics_", season, ".csv"))
  if (nrow(paired_players)) bench31_atomic_write_csv(paired_players, paste0("output/benchmark_pairwise_players_", season, ".csv"))
  if (nrow(pairwise)) bench31_atomic_write_csv(pairwise, paste0("output/benchmark_pairwise_", season, ".csv"))

  invisible(list(scored = scored, metrics = metrics, pairwise_players = paired_players, pairwise = pairwise))
}
