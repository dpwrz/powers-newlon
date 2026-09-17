# ============================================================
# FANTASY MODEL 3.1 - INCREMENTAL PRODUCTION AUTO REFRESH
# ============================================================
# In-progress stat movement alone does not rebuild frozen pregame projections.
# Full projection work runs after a final game, future context changes, an
# explicit force, or the first 3.1 state build.

.fm31_root <- function() {
  script_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  candidates <- c(getwd(), dirname(getwd()))
  if (!is.null(script_path) && length(script_path) == 1 && nzchar(script_path)) {
    d <- dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
    candidates <- c(d, dirname(d), candidates)
  }
  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  for (p in candidates) {
    if (file.exists(file.path(p, "config.R")) && dir.exists(file.path(p, "R")) &&
        dir.exists(file.path(p, "pipeline"))) return(p)
  }
  stop("Could not locate Fantasy Model project root.")
}

run_auto_refresh31 <- function() {
  root <- .fm31_root(); setwd(root)
  source("config.R")
  ensure_packages(c("dplyr","readr","nflreadr","jsonlite","xgboost"))
  source("R/live_refresh_engine_31.R")
  source("R/weekly_engine_31.R")

  dir.create("data/state", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  dir.create("output", recursive = TRUE, showWarnings = FALSE)

  state_path <- file.path("data/state", paste0("live_refresh_state_3_1_", CURRENT_SEASON, ".json"))
  timing_path <- file.path("output", paste0("live_refresh_timing_3_1_", CURRENT_SEASON, ".csv"))
  log_path <- file.path("output", paste0("live_refresh_log_3_1_", CURRENT_SEASON, ".csv"))
  run_id <- format(Sys.time(), "%Y%m%dT%H%M%S")
  timer <- live31_timer_start()
  release_version <- Sys.getenv("FM_RELEASE_VERSION", "3.1")

  cat("\n========================================\n")
  cat(" FANTASY MODEL ", release_version, " - INCREMENTAL AUTO REFRESH\n", sep = "")
  cat("========================================\n")

  stats <- tryCatch(nflreadr::load_player_stats(seasons = CURRENT_SEASON, summary_level = "week"), error = function(e) data.frame())
  if (!nrow(stats)) {
    p <- paste0("data/raw/player_weekly_stats_", CURRENT_SEASON, ".csv")
    if (file.exists(p)) stats <- tryCatch(readr::read_csv(p, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
  }
  timer <- live31_timer_mark(timer, "poll_player_stats")

  schedule <- tryCatch(nflreadr::load_schedules(seasons = CURRENT_SEASON), error = function(e) data.frame())
  if (!nrow(schedule)) {
    candidates <- c(paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv"), "data/raw/schedules_weekly_model.csv")
    for (p in candidates) {
      if (!file.exists(p)) next
      z <- tryCatch(readr::read_csv(p, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
      if (!nrow(z)) next
      if ("season" %in% names(z)) z <- z[as.integer(z$season) == CURRENT_SEASON, , drop = FALSE]
      if (nrow(z)) { schedule <- z; break }
    }
  }
  timer <- live31_timer_mark(timer, "poll_schedule")

  fp <- live31_state_fingerprints(stats, schedule)
  previous <- live25_read_json(state_path)
  force <- live25_force_refresh()
  change <- live31_classify_change(fp, previous, force)
  timer <- live31_timer_mark(timer, "classify_change")

  cat("[3.1 AUTO] reason=", change$reason, " | completed=", fp$completed_games,
      " | stats_week=", fp$latest_actual_week, " | rebuild=", change$rebuild, "\n", sep = "")

  if (nrow(stats)) readr::write_csv(stats, paste0("data/raw/player_weekly_stats_", CURRENT_SEASON, ".csv"))
  if (nrow(schedule)) readr::write_csv(schedule, paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv"))
  timer <- live31_timer_mark(timer, "persist_polled_data")

  if (!change$rebuild) {
    state <- list(
      generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE), model_version = release_version,
      status = change$reason, stats_fingerprint = fp$stats, schedule_fingerprint = fp$schedule,
      future_context_fingerprint = fp$future_context, public_fingerprint = fp$public,
      completed_games = fp$completed_games, latest_actual_week = fp$latest_actual_week,
      current_stat_rows = fp$stats_rows, schedule_rows = fp$schedule_rows
    )
    live31_atomic_json(state, state_path)
    live25_append_log(log_path, data.frame(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), status = change$reason,
      completed_games = fp$completed_games, latest_actual_week = fp$latest_actual_week,
      duration_sec = round(as.numeric(difftime(Sys.time(), timer$start, units = "secs")), 1), stringsAsFactors = FALSE
    ))
    timer <- live31_timer_mark(timer, "write_state")
    live31_write_timing(timer, timing_path, run_id, change$reason, "skipped_rebuild")
    cat("[3.1 AUTO] No future pregame input changed; expensive rebuild skipped.\n")
    return(invisible(FALSE))
  }

  weekly_path <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
  pregame_path <- paste0("output/pregame_projection_archive_", CURRENT_SEASON, ".csv")
  if (file.exists(weekly_path) && file.exists(pregame_path)) {
    tryCatch(source("pipeline/24_score_live_accuracy_2_5.R", local = FALSE),
             error = function(e) warning("Pre-projection accuracy scoring skipped: ", conditionMessage(e)))
  }
  timer <- live31_timer_mark(timer, "score_completed_before_projection")

  if (change$completed_changed || live31_env_true("FM_REFRESH_DEFENSE")) {
    tryCatch(source("maintenance/REFRESH_WEEKLY_DEFENSE_CURRENT.R", local = FALSE),
             error = function(e) warning("Defense refresh skipped: ", conditionMessage(e)))
  }
  timer <- live31_timer_mark(timer, "refresh_defense_if_needed")

  has31 <- all(file.exists(c(
    "output/weekly_3_1_promotion.csv", "output/weekly_3_1_champion_manifest.csv",
    "output/weekly_3_1_residual_pool.csv", paste0("models/weekly_3_1_direct_", POSITIONS, ".json"),
    paste0("models/weekly_3_1_direct_", POSITIONS, "_meta.rds")
  )))

  if (has31) source("pipeline/28_project_weekly_2026_3_1.R", local = FALSE) else {
    cat("[3.1 AUTO] 3.1 challenger artifacts absent; using exact 2.5 production projection.\n")
    source("pipeline/23_project_weekly_2026_2_5.R", local = FALSE)
  }
  timer <- live31_timer_mark(timer, "project_unplayed_and_ros")

  tryCatch(source("pipeline/24_score_live_accuracy_2_5.R", local = FALSE),
           error = function(e) warning("Post-projection accuracy scoring skipped: ", conditionMessage(e)))
  timer <- live31_timer_mark(timer, "freeze_pregame_and_score")

  if (change$completed_changed || live31_env_true("FM_REFRESH_DYNASTY")) {
    source("R/player_identity.R", local = FALSE); source("R/league_engine.R", local = FALSE)
    source("R/dynasty_value_engine.R", local = FALSE); fm3_write_dynasty_value_outputs(BASE_LEAGUE_SETTINGS)
  }
  timer <- live31_timer_mark(timer, "dynasty_if_needed")

  projection_week <- NA_integer_
  if (file.exists(weekly_path)) {
    z <- tryCatch(readr::read_csv(weekly_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
    if (nrow(z) && all(c("week","is_actual") %in% names(z))) {
      ww <- wk31_num(z$week[wk31_num(z$is_actual, 0) == 0], NA_real_); ww <- ww[is.finite(ww)]
      if (length(ww)) projection_week <- as.integer(min(ww))
    }
  }

  state <- list(
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE), model_version = release_version,
    status = "refreshed", refresh_reason = change$reason, stats_fingerprint = fp$stats,
    schedule_fingerprint = fp$schedule, future_context_fingerprint = fp$future_context,
    public_fingerprint = fp$public, completed_games = fp$completed_games,
    latest_actual_week = fp$latest_actual_week,
    projection_week = if (is.finite(projection_week)) projection_week else NULL,
    current_stat_rows = fp$stats_rows, schedule_rows = fp$schedule_rows
  )
  live31_atomic_json(state, state_path)
  timer <- live31_timer_mark(timer, "write_state")

  standalone_export <- file.path(root, "scripts", "export_model_snapshot.R")
  nested_export <- file.path(root, "web", "scripts", "export_model_snapshot.R")
  exporter <- if (file.exists(standalone_export)) standalone_export else nested_export
  if (file.exists(exporter)) {
    Sys.setenv(FANTASY_MODEL_ROOT = root, FANTASY_WEB_SNAPSHOT = file.path(root, "output", "model_snapshot.json"))
    source(exporter, local = new.env(parent = globalenv()))
  }
  timer <- live31_timer_mark(timer, "export_web_snapshot")

  live25_append_log(log_path, data.frame(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), status = "refreshed", reason = change$reason,
    completed_games = fp$completed_games, latest_actual_week = fp$latest_actual_week,
    projection_week = projection_week,
    duration_sec = round(as.numeric(difftime(Sys.time(), timer$start, units = "secs")), 1), stringsAsFactors = FALSE
  ))
  live31_write_timing(timer, timing_path, run_id, change$reason, "refreshed")
  cat("[3.1 AUTO] Refresh complete. Timing -> ", timing_path, "\n", sep = "")
  invisible(TRUE)
}

run_auto_refresh31()
