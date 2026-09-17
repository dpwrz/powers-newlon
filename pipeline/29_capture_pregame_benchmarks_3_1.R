# ============================================================
# FANTASY MODEL 3.1 - CAPTURE PREGAME BENCHMARKS
# ============================================================
# Evaluation-only pipeline. Captures the latest still-pregame Fantasy Model
# and Sleeper projections, freezes them at kickoff, and scores completed rows.

source("config.R")
ensure_packages(c("dplyr", "readr", "tibble", "tidyr", "purrr", "httr2", "jsonlite", "nflreadr"))
source("R/weekly_engine.R")
source("R/benchmark_archive_31.R")

dir.create("output", recursive = TRUE, showWarnings = FALSE)

season <- CURRENT_SEASON
weekly_path <- paste0("output/weekly_", season, "_projections.csv")
archive_path <- paste0("output/benchmark_pregame_archive_", season, ".csv")
status_path <- paste0("output/benchmark_provider_status_", season, ".csv")
manifest_path <- paste0("output/benchmark_snapshot_manifest_", season, ".csv")

if (!file.exists(weekly_path)) stop("Missing current weekly projection file: ", weekly_path)
weekly <- bench31_read_csv(weekly_path)
if (!nrow(weekly)) stop("Current weekly projection file is empty: ", weekly_path)

active_week <- bench31_active_week(weekly)
if (!is.finite(active_week)) {
  cat("[3.1 BENCHMARK] No unplayed player-weeks remain. Nothing to archive.\n")
} else {
  captured_at <- Sys.time()
  schedule_team <- bench31_schedule(season)
  if (!nrow(schedule_team)) stop("Could not load a kickoff schedule for benchmark protection.")

  model_rows <- bench31_model_rows(weekly, schedule_team, active_week, captured_at)
  identity <- bench31_identity_map()
  sleeper <- bench31_fetch_sleeper_week(season, active_week)
  sleeper_errors <- attr(sleeper, "errors")
  sleeper_rows <- bench31_sleeper_rows(sleeper, identity, model_rows, captured_at)

  candidates <- dplyr::bind_rows(model_rows, sleeper_rows)
  existing <- bench31_read_csv(archive_path)
  archive <- bench31_update_archive(existing, candidates)
  if (nrow(archive)) bench31_atomic_write_csv(archive, archive_path)

  now_txt <- format(captured_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  status <- tibble::tibble(
    captured_at_utc = now_txt,
    season = season,
    week = active_week,
    provider = c("fantasy_model", "sleeper"),
    rows_captured = c(nrow(model_rows), nrow(sleeper_rows)),
    status = c(
      if (nrow(model_rows)) "ok" else "no_pregame_rows",
      if (nrow(sleeper_rows)) "ok" else "unavailable"
    ),
    message = c(
      "Current production projection file",
      if (length(sleeper_errors)) paste(sleeper_errors, collapse = " | ") else if (nrow(sleeper_rows)) "Sleeper projection feed" else "No matched Sleeper rows"
    )
  )
  bench31_atomic_write_csv(status, status_path)

  manifest_old <- bench31_read_csv(manifest_path)
  manifest_new <- status |>
    dplyr::transmute(
      captured_at_utc, season, week, provider, rows_captured, status,
      archive_rows_after_capture = nrow(archive)
    )
  manifest <- dplyr::bind_rows(manifest_old, manifest_new) |>
    dplyr::distinct(captured_at_utc, season, week, provider, .keep_all = TRUE) |>
    dplyr::arrange(captured_at_utc, provider)
  bench31_atomic_write_csv(manifest, manifest_path)

  bench31_score_archive(archive, season)

  cat("[3.1 BENCHMARK] Week ", active_week,
      " capture complete: model=", nrow(model_rows),
      ", sleeper=", nrow(sleeper_rows),
      ", archive=", nrow(archive), " rows.\n", sep = "")
  if (length(sleeper_errors)) {
    cat("[3.1 BENCHMARK] Sleeper warnings: ", paste(sleeper_errors, collapse = " | "), "\n", sep = "")
  }
}
