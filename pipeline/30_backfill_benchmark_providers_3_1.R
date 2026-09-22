# ============================================================
# FANTASY MODEL 3.1 - HISTORICAL BENCHMARK PROVIDER BACKFILL
# ============================================================
# Backfills only week-addressable external provider projections for completed
# weeks that already have a valid Fantasy Model pregame archive. Historical
# rows are explicitly tagged and never overwrite a live pregame provider row.

source("config.R")
ensure_packages(c("dplyr", "readr", "tibble", "tidyr", "purrr", "httr2", "jsonlite", "nflreadr"))
source("R/weekly_engine.R")
source("R/benchmark_archive_31.R")
source("R/benchmark_providers_31.R")

season <- CURRENT_SEASON
archive_path <- paste0("output/benchmark_pregame_archive_", season, ".csv")
status_path <- paste0("output/benchmark_backfill_status_", season, ".csv")

if (!file.exists(archive_path)) {
  cat("[3.1 BENCHMARK BACKFILL] No benchmark archive exists yet. Nothing to backfill.\n")
} else {
  archive <- bench31_read_csv(archive_path)
  if (!nrow(archive)) {
    cat("[3.1 BENCHMARK BACKFILL] Benchmark archive is empty. Nothing to backfill.\n")
  } else {
    now <- Sys.time()
    now_num <- as.numeric(as.POSIXct(now, tz = "UTC"))

    if (!"capture_mode" %in% names(archive)) archive$capture_mode <- ""
    archive$capture_mode <- bench31_chr(archive$capture_mode)
    archive$capture_mode[archive$provider == "fantasy_model" & !nzchar(archive$capture_mode)] <- "live_pregame"

    model <- archive |>
      dplyr::filter(
        provider == "fantasy_model",
        scoring_id == "half_ppr",
        is.finite(bench31_num(minutes_to_kickoff)),
        bench31_num(minutes_to_kickoff) > 0
      ) |>
      dplyr::mutate(
        .kickoff = as.POSIXct(kickoff_utc, tz = "UTC"),
        .kickoff_num = as.numeric(.kickoff)
      ) |>
      dplyr::filter(is.finite(.kickoff_num))

    if (!nrow(model)) {
      cat("[3.1 BENCHMARK BACKFILL] No valid Fantasy Model pregame rows are eligible.\n")
    } else {
      completed_weeks <- model |>
        dplyr::group_by(week) |>
        dplyr::summarise(last_kickoff = max(.kickoff_num, na.rm = TRUE), .groups = "drop") |>
        dplyr::filter(last_kickoff < now_num) |>
        dplyr::pull(week) |>
        sort()

      if (!length(completed_weeks)) {
        cat("[3.1 BENCHMARK BACKFILL] No completed archived weeks are eligible.\n")
      } else {
        identity <- bench31_identity_map()
        status_rows <- list()

        for (w in completed_weeks) {
          model_rows <- model |>
            dplyr::filter(week == w) |>
            dplyr::select(-dplyr::any_of(c(".kickoff", ".kickoff_num"))) |>
            dplyr::distinct(player_id, .keep_all = TRUE)

          backfill <- bench31_capture_backfill_providers(
            season = season,
            week = w,
            identity = identity,
            model_rows = model_rows,
            captured_at = now
          )

          before_n <- nrow(archive)
          archive <- bench31_append_missing_archive(archive, backfill$rows)
          added_n <- nrow(archive) - before_n

          if (nrow(backfill$status)) {
            status_rows[[length(status_rows) + 1]] <- backfill$status |>
              dplyr::mutate(
                season = season,
                captured_at_utc = format(as.POSIXct(now, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                rows_added_to_archive = added_n
              ) |>
              dplyr::select(
                captured_at_utc, season, week, provider,
                rows_captured, rows_added_to_archive, status, message
              )
          }

          cat(
            "[3.1 BENCHMARK BACKFILL] Week ", w,
            ": fetched=", nrow(backfill$rows),
            ", newly_added=", added_n, "\n", sep = ""
          )
        }

        bench31_atomic_write_csv(archive, archive_path)
        statuses <- dplyr::bind_rows(status_rows)
        if (nrow(statuses)) bench31_atomic_write_csv(statuses, status_path)

        bench31_score_archive(archive, season)
        cat("[3.1 BENCHMARK BACKFILL] Complete. Archive rows=", nrow(archive), ".\n", sep = "")
      }
    }
  }
}
