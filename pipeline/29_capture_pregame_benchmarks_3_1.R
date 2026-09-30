# ============================================================
# FANTASY MODEL 3.1 - CAPTURE PREGAME BENCHMARKS
# ============================================================
# Evaluation-only pipeline. Captures the latest still-pregame Fantasy Model
# and Sleeper projections, freezes them at kickoff, and scores completed rows.
#
# Integrity contract:
#   * rows may refresh only while that player's game is still pre-kickoff;
#   * started-game archive rows are never replaced;
#   * external-provider failure never blocks the Fantasy Model archive;
#   * if eligible Fantasy Model rows exist but none can be archived, fail loudly.

source("config.R")
ensure_packages(c("dplyr", "readr", "tibble", "tidyr", "purrr", "httr2", "jsonlite", "nflreadr"))
source("R/weekly_engine.R")
source("R/benchmark_archive_31.R")
source("R/benchmark_providers_31.R")

dir.create("output", recursive = TRUE, showWarnings = FALSE)

season <- CURRENT_SEASON
weekly_path <- paste0("output/weekly_", season, "_projections.csv")
archive_path <- paste0("output/benchmark_pregame_archive_", season, ".csv")
status_path <- paste0("output/benchmark_provider_status_", season, ".csv")
manifest_path <- paste0("output/benchmark_snapshot_manifest_", season, ".csv")

if (!file.exists(weekly_path)) stop("Missing current weekly projection file: ", weekly_path)
weekly <- bench31_read_csv(weekly_path)
if (!nrow(weekly)) stop("Current weekly projection file is empty: ", weekly_path)

# Return an existing column without inventing a value. These aliases let the
# archive survive harmless upstream renames while keeping provenance explicit.
bench31_pick_context <- function(d, aliases, type = c("character", "numeric")) {
  type <- match.arg(type)
  hit <- aliases[aliases %in% names(d)]
  if (!length(hit)) {
    return(if (type == "numeric") rep(NA_real_, nrow(d)) else rep("", nrow(d)))
  }
  x <- d[[hit[[1]]]]
  if (type == "numeric") return(suppressWarnings(as.numeric(x)))
  out <- as.character(x)
  out[is.na(out)] <- ""
  out
}

bench31_first_nonempty <- function(x) {
  y <- trimws(as.character(x))
  y <- y[!is.na(y) & nzchar(y)]
  if (length(y)) y[[1]] else ""
}

bench31_injury_context <- function(weekly, active_week, season) {
  if (!all(c("week", "player_id") %in% names(weekly))) return(tibble::tibble())

  w <- weekly |>
    dplyr::mutate(
      week = suppressWarnings(as.integer(week)),
      player_id = as.character(player_id)
    ) |>
    dplyr::filter(week == active_week, nzchar(player_id))

  if (!nrow(w)) return(tibble::tibble())

  ctx <- tibble::tibble(
    week = w$week,
    player_id = w$player_id,
    availability_probability = bench31_pick_context(
      w,
      c("availability_probability", "availability_prob", "playing_probability", "active_probability"),
      "numeric"
    ),
    injury_status = bench31_pick_context(
      w,
      c("injury_status", "report_status", "game_status"),
      "character"
    ),
    practice_status = bench31_pick_context(
      w,
      c("practice_status", "practice_participation", "practice"),
      "character"
    ),
    injury_risk = bench31_pick_context(w, c("injury_risk"), "numeric"),
    practice_risk = bench31_pick_context(w, c("practice_risk"), "numeric"),
    injury_pressure = bench31_pick_context(
      w,
      c("injury_pressure", "role_injury_pressure", "unavailable_snap_share_30"),
      "numeric"
    )
  ) |>
    dplyr::distinct(week, player_id, .keep_all = TRUE)

  # Supplement only literal report/practice fields from the raw weekly injury
  # source. We do not derive an availability probability from a status label.
  injury_path <- "data/raw/injuries_weekly_model.csv"
  if (file.exists(injury_path)) {
    inj <- tryCatch(
      readr::read_csv(injury_path, show_col_types = FALSE, progress = FALSE),
      error = function(e) tibble::tibble()
    )
    if (nrow(inj)) {
      id_col <- c("gsis_id", "player_id")[c("gsis_id", "player_id") %in% names(inj)]
      if (length(id_col) && all(c("season", "week") %in% names(inj))) {
        inj_player <- as.character(inj[[id_col[[1]]]])
        inj_report <- bench31_pick_context(inj, c("report_status", "injury_status", "game_status"), "character")
        inj_practice <- bench31_pick_context(inj, c("practice_status", "practice_participation", "practice"), "character")
        inj2 <- tibble::tibble(
          season = suppressWarnings(as.integer(inj$season)),
          week = suppressWarnings(as.integer(inj$week)),
          player_id = inj_player,
          injury_status_raw = inj_report,
          practice_status_raw = inj_practice
        ) |>
          dplyr::filter(season == !!season, week == !!active_week, nzchar(player_id)) |>
          dplyr::group_by(week, player_id) |>
          dplyr::summarise(
            injury_status_raw = bench31_first_nonempty(injury_status_raw),
            practice_status_raw = bench31_first_nonempty(practice_status_raw),
            .groups = "drop"
          )

        if (nrow(inj2)) {
          ctx <- ctx |>
            dplyr::left_join(inj2, by = c("week", "player_id")) |>
            dplyr::mutate(
              injury_status = dplyr::if_else(
                nzchar(injury_status), injury_status,
                dplyr::coalesce(injury_status_raw, "")
              ),
              practice_status = dplyr::if_else(
                nzchar(practice_status), practice_status,
                dplyr::coalesce(practice_status_raw, "")
              )
            ) |>
            dplyr::select(-injury_status_raw, -practice_status_raw)
        }
      }
    }
  }

  ctx
}

bench31_add_capture_context <- function(candidates, weekly, schedule_team, active_week, season, weekly_path) {
  if (!nrow(candidates)) return(candidates)

  # Correct/complete opponent from the kickoff schedule. Blank strings are not
  # treated as valid opponent values.
  sched <- schedule_team |>
    dplyr::filter(week == active_week) |>
    dplyr::select(week, team, schedule_opponent = opponent) |>
    dplyr::distinct(week, team, .keep_all = TRUE)

  candidates <- candidates |>
    dplyr::left_join(sched, by = c("week", "team")) |>
    dplyr::mutate(
      opponent = dplyr::if_else(
        nzchar(trimws(dplyr::coalesce(as.character(opponent), ""))),
        as.character(opponent),
        dplyr::coalesce(as.character(schedule_opponent), "")
      ),
      source_endpoint = dplyr::if_else(
        provider == "fantasy_model",
        weekly_path,
        as.character(source_endpoint)
      )
    ) |>
    dplyr::select(-schedule_opponent)

  inj_ctx <- bench31_injury_context(weekly, active_week, season)
  if (nrow(inj_ctx)) {
    candidates <- candidates |>
      dplyr::left_join(inj_ctx, by = c("week", "player_id"))
  } else {
    candidates$availability_probability <- NA_real_
    candidates$injury_status <- ""
    candidates$practice_status <- ""
    candidates$injury_risk <- NA_real_
    candidates$practice_risk <- NA_real_
    candidates$injury_pressure <- NA_real_
  }

  candidates
}

captured_at <- Sys.time()
schedule_team <- bench31_schedule(season)
if (!nrow(schedule_team)) stop("Could not load a kickoff schedule for benchmark protection.")
active_week <- bench31_active_week(weekly, schedule_team, captured_at)

if (!is.finite(active_week)) {
  cat("[3.1 BENCHMARK] No future kickoff remains. Nothing to archive.\n")
} else {

  now_num <- as.numeric(as.POSIXct(captured_at, tz = "UTC"))
  future_schedule <- schedule_team |>
    dplyr::filter(
      week == active_week,
      is.finite(as.numeric(kickoff)),
      as.numeric(kickoff) > now_num,
      !score_started
    ) |>
    dplyr::distinct(week, team, .keep_all = TRUE)

  weekly_actual <- if ("is_actual" %in% names(weekly)) suppressWarnings(as.numeric(weekly$is_actual)) else rep(0, nrow(weekly))
  weekly_week <- suppressWarnings(as.integer(weekly$week))
  weekly_team <- if ("team" %in% names(weekly)) as.character(weekly$team) else rep("", nrow(weekly))
  weekly_pos <- if ("position" %in% names(weekly)) toupper(as.character(weekly$position)) else rep("", nrow(weekly))
  weekly_pid <- if ("player_id" %in% names(weekly)) as.character(weekly$player_id) else rep("", nrow(weekly))
  eligible_model_rows <- sum(
    weekly_week == active_week &
      dplyr::coalesce(weekly_actual, 0) == 0 &
      weekly_pos %in% POSITIONS &
      nzchar(weekly_pid) &
      weekly_team %in% future_schedule$team,
    na.rm = TRUE
  )

  model_rows <- bench31_model_rows(weekly, schedule_team, active_week, captured_at)
  identity <- bench31_identity_map()

  external <- bench31_capture_external_providers(
    season = season,
    week = active_week,
    identity = identity,
    model_rows = model_rows,
    captured_at = captured_at
  )
  external_rows <- external$rows
  external_status <- external$status

  candidates <- dplyr::bind_rows(model_rows, external_rows)
  candidates <- bench31_add_capture_context(
    candidates, weekly, schedule_team, active_week, season, weekly_path
  )

  existing <- bench31_read_csv(archive_path)
  archive <- bench31_update_archive(existing, candidates)
  if (nrow(archive)) bench31_atomic_write_csv(archive, archive_path)

  fatal_model_capture <- eligible_model_rows > 0 && nrow(model_rows) == 0
  now_txt <- format(captured_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  model_status <- tibble::tibble(
    provider = "fantasy_model",
    eligible_pregame_rows = eligible_model_rows,
    rows_captured = nrow(model_rows),
    status = if (fatal_model_capture) "error_no_rows" else if (nrow(model_rows)) "ok" else "no_pregame_rows",
    message = if (fatal_model_capture) {
      paste0("Expected ", eligible_model_rows, " eligible Fantasy Model rows but captured 0")
    } else if (nrow(model_rows)) {
      paste0("Captured current production projections from ", weekly_path)
    } else {
      "No still-pregame Fantasy Model rows at this capture time"
    }
  )

  status <- dplyr::bind_rows(model_status, external_status) |>
    dplyr::mutate(
      captured_at_utc = now_txt,
      season = season,
      week = active_week,
      archive_rows_after_capture = nrow(archive)
    ) |>
    dplyr::select(
      captured_at_utc, season, week, provider,
      eligible_pregame_rows, rows_captured,
      archive_rows_after_capture, status, message
    )

  bench31_atomic_write_csv(status, status_path)

  manifest_old <- bench31_read_csv(manifest_path)
  manifest_new <- status |>
    dplyr::select(
      captured_at_utc, season, week, provider, eligible_pregame_rows,
      rows_captured, archive_rows_after_capture, status
    )

  # readr may auto-parse an existing ISO timestamp as POSIXct while the new
  # status row is character. Normalize the persisted contract before binding.
  if (nrow(manifest_old) && "captured_at_utc" %in% names(manifest_old)) {
    manifest_old$captured_at_utc <- as.character(manifest_old$captured_at_utc)
  }
  manifest_new$captured_at_utc <- as.character(manifest_new$captured_at_utc)

  manifest <- dplyr::bind_rows(manifest_old, manifest_new) |>
    dplyr::distinct(captured_at_utc, season, week, provider, .keep_all = TRUE) |>
    dplyr::arrange(captured_at_utc, provider)
  bench31_atomic_write_csv(manifest, manifest_path)

  bench31_score_archive(archive, season)

  provider_counts <- if (nrow(external_status)) {
    paste0(external_status$provider, "=", external_status$rows_captured, collapse = ", ")
  } else {
    "none"
  }

  cat("[3.1 BENCHMARK] Week ", active_week,
      " capture complete: eligible_model=", eligible_model_rows,
      ", model=", nrow(model_rows),
      ", external={", provider_counts, "}",
      ", archive=", nrow(archive), " rows.\n", sep = "")

  warnings <- external_status |>
    dplyr::filter(status == "unavailable")
  if (nrow(warnings)) {
    cat(
      "[3.1 BENCHMARK] Provider warnings: ",
      paste0(warnings$provider, ": ", warnings$message, collapse = " | "),
      "\n", sep = ""
    )
  }

  if (fatal_model_capture) {
    stop(
      "Benchmark integrity failure: eligible Fantasy Model pregame rows existed, ",
      "but none were archived. See ", status_path, "."
    )
  }
}
