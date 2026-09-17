# ============================================================
# FANTASY MODEL 3.1 - INCREMENTAL LIVE REFRESH SUPPORT
# ============================================================
# 3.1 separates noisy in-progress stat changes from changes that can alter
# future pregame projections. It preserves the validated 2.5/3.0 projection
# math; this file changes orchestration only.

if (!exists("live25_md5_objects", mode = "function")) {
  source("R/live_refresh_engine_25.R")
}

live31_atomic_json <- function(x, path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required by the Model 3.1 live refresh layer.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(x, tmp, pretty = TRUE, auto_unbox = TRUE, na = "null")
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}

live31_schedule_context <- function(schedule) {
  if (is.null(schedule) || !nrow(schedule)) return(data.frame())
  z <- live25_compact_schedule(schedule)
  if (!nrow(z)) return(z)

  result <- live25_num(live25_first_col(z, c("result")))
  hs <- live25_num(live25_first_col(z, c("home_score")))
  as_ <- live25_num(live25_first_col(z, c("away_score")))
  # Match the established 2.5 completed-game contract: prefer the explicit
  # result field whenever the schedule source supplies it. Only fall back to
  # finite scores when result is unavailable for the dataset. This prevents
  # live score updates from being mistaken for final games.
  completed <- if (any(is.finite(result))) {
    is.finite(result)
  } else {
    is.finite(hs) & is.finite(as_)
  }
  z <- z[!completed, , drop = FALSE]

  # Scores and result are intentionally excluded. They move constantly during
  # games but do not change a frozen pregame forecast for another unplayed game.
  keep <- intersect(c(
    "season", "week", "game_id", "game_type", "gameday", "gametime", "weekday",
    "home_team", "away_team", "spread_line", "total_line",
    "home_moneyline", "away_moneyline", "roof", "surface"
  ), names(z))
  z <- z[, keep, drop = FALSE]
  ord <- intersect(c("season", "week", "game_id"), names(z))
  if (length(ord) && nrow(z)) z <- z[do.call(order, z[ord]), , drop = FALSE]
  z
}

live31_state_fingerprints <- function(stats, schedule) {
  stats_compact <- live25_compact_stats(stats)
  schedule_compact <- live25_compact_schedule(schedule)
  future_context <- live31_schedule_context(schedule)
  list(
    stats = live25_md5_objects(stats_compact),
    schedule = live25_md5_objects(schedule_compact),
    future_context = live25_md5_objects(future_context),
    public = live25_md5_objects(stats_compact, schedule_compact),
    completed_games = live25_completed_games(schedule_compact),
    latest_actual_week = live25_latest_actual_week(stats_compact),
    stats_rows = nrow(stats_compact),
    schedule_rows = nrow(schedule_compact)
  )
}

live31_prev_chr <- function(previous, name, default = "") {
  x <- previous[[name]]
  if (is.null(x) || !length(x) || is.na(x[[1]])) default else as.character(x[[1]])
}

live31_prev_int <- function(previous, name, default = 0L) {
  x <- suppressWarnings(as.integer(previous[[name]]))
  if (!length(x) || !is.finite(x[[1]])) default else as.integer(x[[1]])
}

live31_classify_change <- function(current, previous = list(), force = FALSE) {
  first_run <- !length(previous) || !nzchar(live31_prev_chr(previous, "generated_at", ""))
  prev_completed <- live31_prev_int(previous, "completed_games", 0L)
  prev_stats <- live31_prev_chr(previous, "stats_fingerprint", "")
  prev_context <- live31_prev_chr(previous, "future_context_fingerprint", "")
  prev_public <- live31_prev_chr(previous, "public_fingerprint", "")

  completed_changed <- current$completed_games > prev_completed
  stats_changed <- !identical(as.character(current$stats), prev_stats)
  context_changed <- !identical(as.character(current$future_context), prev_context)
  public_changed <- !identical(as.character(current$public), prev_public)

  reason <- if (force) {
    "forced"
  } else if (first_run) {
    "first_run"
  } else if (completed_changed) {
    "completed_game"
  } else if (context_changed) {
    "future_context"
  } else if (stats_changed) {
    "in_progress_stats_only"
  } else if (public_changed) {
    "public_state_only"
  } else {
    "no_change"
  }

  rebuild <- reason %in% c("forced", "first_run", "completed_game", "future_context")
  list(
    reason = reason,
    rebuild = rebuild,
    first_run = first_run,
    completed_changed = completed_changed,
    stats_changed = stats_changed,
    context_changed = context_changed,
    public_changed = public_changed
  )
}

live31_timer_start <- function() {
  list(start = Sys.time(), marks = data.frame())
}

live31_timer_mark <- function(timer, stage) {
  now <- Sys.time()
  last <- if (nrow(timer$marks)) timer$marks$timestamp[nrow(timer$marks)] else timer$start
  row <- data.frame(
    stage = as.character(stage),
    timestamp = now,
    stage_sec = as.numeric(difftime(now, last, units = "secs")),
    total_sec = as.numeric(difftime(now, timer$start, units = "secs")),
    stringsAsFactors = FALSE
  )
  timer$marks <- dplyr::bind_rows(timer$marks, row)
  timer
}

live31_write_timing <- function(timer, path, run_id, reason, status) {
  if (!nrow(timer$marks)) return(invisible(path))
  out <- timer$marks
  out$run_id <- run_id
  out$reason <- reason
  out$status <- status
  out$generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  out <- out[, c("generated_at", "run_id", "reason", "status", "stage",
                 "stage_sec", "total_sec", "timestamp"), drop = FALSE]
  live25_append_log(path, out)
  invisible(path)
}

live31_env_true <- function(name, default = "false") {
  tolower(trimws(Sys.getenv(name, default))) %in% c("1", "true", "t", "yes", "y")
}
