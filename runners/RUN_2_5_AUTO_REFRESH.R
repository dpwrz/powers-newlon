# ============================================================
# FANTASY MODEL 2.5 - AUTOMATIC LIVE REFRESH
# ============================================================
# Intended for local use OR GitHub Actions. It does not retrain Model 2.5.

.fm25_live_root <- function() {
  script_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  candidates <- c(getwd(), dirname(getwd()))
  if (!is.null(script_path) && length(script_path) == 1 && nzchar(script_path)) {
    d <- dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
    candidates <- c(d, dirname(d), candidates)
  }
  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  for (p in candidates) if (file.exists(file.path(p, "config.R")) && dir.exists(file.path(p, "R")) && dir.exists(file.path(p, "pipeline"))) return(p)
  stop("Could not locate Fantasy Model project root. Run from the project or set the working directory first.")
}

run_auto_refresh25 <- function() {
  root <- .fm25_live_root()
  setwd(root)
  source("config.R")
  ensure_packages(c("dplyr", "readr", "nflreadr", "jsonlite", "xgboost"))
  source("R/live_refresh_engine_25.R")

  dir.create("data/state", recursive = TRUE, showWarnings = FALSE)
  dir.create("output", recursive = TRUE, showWarnings = FALSE)
  state_path <- file.path("data/state", paste0("live_refresh_state_", CURRENT_SEASON, ".json"))
  log_path <- file.path("output", paste0("live_refresh_log_", CURRENT_SEASON, ".csv"))
  started <- Sys.time()
  release_version <- Sys.getenv("FM_RELEASE_VERSION", "2.5-live")

  cat("\n========================================\n")
  cat(" FANTASY MODEL ", release_version, " - AUTO REFRESH\n", sep = "")
  cat("========================================\n")
  cat("[AUTO] Project root: ", root, "\n", sep = "")

  current_stats <- tryCatch(nflreadr::load_player_stats(seasons = CURRENT_SEASON, summary_level = "week"), error = function(e) {
    warning("Could not poll current player stats: ", conditionMessage(e)); data.frame()
  })
  if (!nrow(current_stats)) {
    stats_cache <- paste0("data/raw/player_weekly_stats_", CURRENT_SEASON, ".csv")
    if (file.exists(stats_cache)) {
      current_stats <- tryCatch(readr::read_csv(stats_cache, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
      if (nrow(current_stats)) cat("[AUTO] Using cached current player stats after remote poll failure.\n")
    }
  }

  schedule <- tryCatch(nflreadr::load_schedules(seasons = CURRENT_SEASON), error = function(e) {
    warning("Could not poll current schedule: ", conditionMessage(e)); data.frame()
  })
  if (!nrow(schedule)) {
    schedule_candidates <- c(
      paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv"),
      "data/raw/schedules_weekly_model.csv"
    )
    for (sp in schedule_candidates) {
      if (!file.exists(sp)) next
      z <- tryCatch(readr::read_csv(sp, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
      if (!nrow(z) || !"season" %in% names(z)) next
      z <- z[suppressWarnings(as.integer(z$season)) == CURRENT_SEASON, , drop = FALSE]
      if (nrow(z)) {
        schedule <- z
        cat("[AUTO] Using cached current-season schedule after remote poll failure: ", sp, "\n", sep = "")
        break
      }
    }
  }

  stats_compact <- live25_compact_stats(current_stats)
  schedule_compact <- live25_compact_schedule(schedule)
  fingerprint <- live25_md5_objects(stats_compact, schedule_compact)
  completed_games <- live25_completed_games(schedule_compact)
  latest_actual_week <- live25_latest_actual_week(stats_compact)
  previous <- live25_read_json(state_path)
  previous_fingerprint <- if (!is.null(previous$fingerprint)) as.character(previous$fingerprint) else ""
  previous_completed <- if (!is.null(previous$completed_games)) suppressWarnings(as.integer(previous$completed_games)) else 0L
  force <- live25_force_refresh()
  changed <- !identical(fingerprint, previous_fingerprint)

  cat("[AUTO] Fingerprint changed: ", changed, " | force: ", force, " | completed games: ", completed_games, " | latest stats week: ", latest_actual_week, "\n", sep = "")

  if (!changed && !force) {
    live25_append_log(log_path, data.frame(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), status = "no_change",
      fingerprint = fingerprint, completed_games = completed_games, latest_actual_week = latest_actual_week,
      duration_sec = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1), stringsAsFactors = FALSE
    ))
    cat("[AUTO] No public-state change detected. Nothing to rebuild.\n")
    return(invisible(FALSE))
  }

  if (nrow(current_stats)) readr::write_csv(current_stats, paste0("data/raw/player_weekly_stats_", CURRENT_SEASON, ".csv"))
  if (nrow(schedule)) readr::write_csv(schedule, paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv"))

  if (completed_games > previous_completed || tolower(Sys.getenv("FM_REFRESH_DEFENSE", "false")) %in% c("1", "true", "yes")) {
    cat("[AUTO] Completed-game count increased; refreshing current-season defensive PBP context.\n")
    tryCatch(source("maintenance/REFRESH_WEEKLY_DEFENSE_CURRENT.R", local = FALSE), error = function(e) warning("Defense refresh skipped: ", conditionMessage(e)))
  }

  required25 <- c(
    "output/weekly_2_5_champion_manifest.csv", "output/weekly_2_5_residual_pool.csv",
    paste0("models/weekly_2_5_direct_", POSITIONS, ".json"), paste0("models/weekly_2_5_direct_", POSITIONS, "_meta.rds")
  )
  missing25 <- required25[!file.exists(required25)]
  if (length(missing25)) stop("Model 2.5 live artifacts are missing. Run the accuracy tournament once locally first. Missing: ", paste(missing25, collapse = ", "))

  # Score newly completed games BEFORE the next projection pass. This makes the
  # final 2.5 forecast error available as lagged state for future weeks on the
  # same refresh, rather than waiting for another scheduled job.
  .weekly_existing25 <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
  .pregame_existing25 <- paste0("output/pregame_projection_archive_", CURRENT_SEASON, ".csv")
  if (file.exists(.weekly_existing25) && file.exists(.pregame_existing25)) {
    cat("[AUTO] Scoring newly completed games first so final-model error can feed the next projection...\n")
    tryCatch(
      source("pipeline/24_score_live_accuracy_2_5.R", local = FALSE),
      error = function(e) warning("Pre-projection accuracy scoring skipped: ", conditionMessage(e))
    )
  }

  cat("[AUTO] Forward-only refresh: finalizing new actuals, then reprojecting unplayed player-weeks + ROS...\n")
  source("pipeline/23_project_weekly_2026_2_5.R", local = FALSE)
  cat("[AUTO] Freezing updated pregame forecasts + refreshing performance reports...\n")
  source("pipeline/24_score_live_accuracy_2_5.R", local = FALSE)

  projection_week <- NA_integer_
  weekly_out <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
  if (file.exists(weekly_out)) {
    zz <- tryCatch(readr::read_csv(weekly_out, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
    if (nrow(zz) && all(c("week", "is_actual") %in% names(zz))) {
      ww <- live25_num(zz$week[live25_num(zz$is_actual, 0) == 0])
      ww <- ww[is.finite(ww)]
      if (length(ww)) projection_week <- as.integer(min(ww))
    }
  }

  state <- list(
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE), model_version = release_version,
    fingerprint = fingerprint, completed_games = completed_games, latest_actual_week = latest_actual_week,
    projection_week = if (is.finite(projection_week)) projection_week else NULL,
    current_stat_rows = nrow(stats_compact), schedule_rows = nrow(schedule_compact),
    source = "nflverse current player-week stats + schedule; current PBP defense refresh after completed games"
  )
  live25_write_json(state, state_path)
  live25_append_log(log_path, data.frame(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), status = "refreshed",
    fingerprint = fingerprint, completed_games = completed_games, latest_actual_week = latest_actual_week,
    projection_week = projection_week, duration_sec = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1), stringsAsFactors = FALSE
  ))

  cat("[AUTO] Rebuilding career-based dynasty values and rookie-pick expected values...\n")
  source("R/player_identity.R", local = FALSE)
  source("R/league_engine.R", local = FALSE)
  source("R/dynasty_value_engine.R", local = FALSE)
  fm3_write_dynasty_value_outputs(BASE_LEAGUE_SETTINGS)

  # Export only after the state file is written so Data Health shows this run, not the previous run.
  # In the production two-repository layout the engine owns the snapshot.
  # The separate web repository receives a copy in GitHub Actions.
  standalone_export <- file.path(root, "scripts", "export_model_snapshot.R")
  nested_export <- file.path(root, "web", "scripts", "export_model_snapshot.R")
  web_export <- if (file.exists(standalone_export)) standalone_export else nested_export
  web_snapshot <- file.path(root, "output", "model_snapshot.json")
  if (file.exists(web_export)) {
    Sys.setenv(FANTASY_MODEL_ROOT = root, FANTASY_WEB_SNAPSHOT = web_snapshot)
    cat("[AUTO] Exporting updated website snapshot to ", web_snapshot, "...\n", sep = "")
    source(web_export, local = new.env(parent = globalenv()))
  } else {
    cat("[AUTO] Website exporter not found; model outputs updated but website export skipped.\n")
  }

  cat("\n[AUTO] Refresh complete.\n")
  cat("[AUTO] New state -> freeze completed rows -> W", projection_week, "-18 only -> ROS -> accuracy -> web snapshot.\n", sep = "")
  invisible(TRUE)
}

run_auto_refresh25()
