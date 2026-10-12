# ============================================================
# FANTASY MODEL 3.1 - HONEST FEATURE BACKFILL FROM GIT HISTORY
# ============================================================
# Replays timestamped production commits and keeps, per player-week, only a
# feature snapshot that existed before that player's team kickoff.
#
# This is a recovery bridge for 2026 weeks played before weekly-learning feature
# capture existed. It never reconstructs features from postgame/current state.

source("config.R")
ensure_packages(c("dplyr","readr","nflreadr"))
source("R/weekly_engine_31.R")
source("R/benchmark_archive_31.R")
source("R/weekly_learning_31.R")

season <- CURRENT_SEASON
feature_path <- file.path("data","processed",paste0("weekly_learning_features_", season, ".csv"))
accuracy_path <- file.path("output",paste0("live_accuracy_player_weeks_", season, ".csv"))
status_path <- file.path("output",paste0("weekly_learning_backfill_status_", season, ".csv"))

accuracy <- wl31_read_csv(accuracy_path)
if (!nrow(accuracy)) {
  cat("[3.1 LEARNING BACKFILL] No finalized accuracy rows. Nothing to backfill.\n")
  quit(save = "no", status = 0)
}

existing <- wl31_read_csv(feature_path)
coverage_before <- wl31_coverage_by_week(existing, accuracy)
coverage_eligible_before <- wl31_eligible_weeks(coverage_before)

schedule_team <- bench31_schedule(season)
if (!nrow(schedule_team)) {
  warning("[3.1 LEARNING BACKFILL] No kickoff schedule available.")
  quit(save = "no", status = 0)
}
schedule_raw <- wl31_read_csv(paste0("data/raw/schedules_live_", season, ".csv"))
if (!nrow(schedule_raw)) {
  schedule_raw <- tryCatch(nflreadr::load_schedules(seasons = season), error = function(e) data.frame())
}
final_weeks <- wl31_completed_schedule_weeks(schedule_raw)
eligible_before <- intersect(coverage_eligible_before, final_weeks)

completed_weeks <- sort(unique(suppressWarnings(as.integer(accuracy$week))))
completed_weeks <- intersect(completed_weeks[is.finite(completed_weeks)], final_weeks)
need_weeks <- setdiff(completed_weeks, eligible_before)

if (!length(need_weeks)) {
  status <- data.frame(
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    season = season,
    requested_weeks = "",
    commits_replayed = 0L,
    recovered_rows = 0L,
    eligible_weeks_after = paste(eligible_before, collapse = ","),
    status = "already_complete",
    stringsAsFactors = FALSE
  )
  wl31_atomic_write_csv(status, status_path)
  cat("[3.1 LEARNING BACKFILL] Existing archive already covers finalized weeks.\n")
  quit(save = "no", status = 0)
}

git_ok <- suppressWarnings(system2("git", "--version", stdout = TRUE, stderr = TRUE))
if (!length(git_ok)) {
  warning("[3.1 LEARNING BACKFILL] git executable unavailable.")
  quit(save = "no", status = 0)
}

log_lines <- tryCatch(
  system2(
    "git",
    c("log","--all","--full-history","--reverse","--format=%H__FMSEP__%cI","--","output/weekly_2026_projections.csv"),
    stdout = TRUE, stderr = TRUE
  ),
  error = function(e) character()
)
log_lines <- log_lines[grepl("^[0-9a-f]{40}__FMSEP__", log_lines)]
if (!length(log_lines)) {
  warning("[3.1 LEARNING BACKFILL] No projection commits found.")
  quit(save = "no", status = 0)
}

parts <- strsplit(log_lines, "__FMSEP__", fixed = TRUE)
commits <- data.frame(
  sha = vapply(parts, function(x) x[[1]], character(1)),
  timestamp = vapply(parts, function(x) paste(x[-1], collapse = "__FMSEP__"), character(1)),
  stringsAsFactors = FALSE
)
parse_git_time <- function(x) {
  y <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  suppressWarnings(as.POSIXct(y, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))
}
commits$time <- parse_git_time(commits$timestamp)
commits <- commits[is.finite(as.numeric(commits$time)), , drop = FALSE]

# Ignore commits after now and obviously pre-season commits.
season_floor <- as.POSIXct(paste0(season, "-08-01 00:00:00"), tz = "UTC")
commits <- commits[
  commits$time >= season_floor &
    commits$time <= Sys.time(),
  , drop = FALSE
]

active_at_commit <- vapply(
  commits$time,
  function(tt) {
    z <- wl31_active_week(schedule_team, tt)
    if (is.finite(z)) as.integer(z) else NA_integer_
  },
  integer(1)
)
active_tab <- table(active_at_commit[active_at_commit %in% need_weeks], useNA = "no")
cat("[3.1 LEARNING BACKFILL] Projection commits found: ", nrow(commits),
    " | requested active-week commit counts: ",
    if (length(active_tab)) paste(names(active_tab), as.integer(active_tab), sep = "=", collapse = ",") else "none",
    "\n", sep = "")

tmp_archive <- tempfile(fileext = ".csv")
tmp_status <- tempfile(fileext = ".csv")
replayed <- 0L

for (i in seq_len(nrow(commits))) {
  sha <- commits$sha[[i]]
  ct <- commits$time[[i]]

  # Skip commits whose active NFL week is not one we need. Do this before
  # reading the large historical projection CSV so normal weekly reruns stay fast.
  aw <- wl31_active_week(schedule_team, ct)
  if (!is.finite(aw) || !aw %in% need_weeks) next

  requested_schedule <- schedule_team |>
    dplyr::filter(week == aw)
  if (!nrow(requested_schedule) ||
      !any(as.numeric(requested_schedule$kickoff) > as.numeric(ct), na.rm = TRUE)) next

  tf <- tempfile(fileext = ".csv")
  spec <- paste0(sha, ":output/weekly_2026_projections.csv")
  rc <- suppressWarnings(system2("git", c("show", spec), stdout = tf, stderr = FALSE))
  if (!identical(as.integer(rc), 0L) || !file.exists(tf) || file.info(tf)$size <= 0) {
    unlink(tf)
    next
  }

  weekly <- tryCatch(
    readr::read_csv(tf, show_col_types = FALSE, progress = FALSE),
    error = function(e) data.frame()
  )
  unlink(tf)
  if (!nrow(weekly)) next

  ok <- tryCatch({
    wl31_capture_features(
      weekly = weekly,
      schedule_team = schedule_team,
      captured_at = ct,
      season = season,
      archive_path = tmp_archive,
      status_path = tmp_status
    )
    TRUE
  }, error = function(e) {
    message("[3.1 LEARNING BACKFILL] Skip ", substr(sha,1,8), ": ", conditionMessage(e))
    FALSE
  })
  if (ok) replayed <- replayed + 1L
}

backfilled <- wl31_read_csv(tmp_archive)
unlink(c(tmp_archive, tmp_status))

if (nrow(backfilled)) {
  # Merge with any live-captured features and keep the latest honest pre-kickoff
  # capture per player-week. Existing newer captures therefore cannot be rolled
  # backward by the historical replay.
  if (nrow(existing)) {
    zz <- wl31_normalize_bind_types(existing, backfilled)
    combined <- dplyr::bind_rows(zz$old, zz$new)
  } else {
    combined <- backfilled
  }

  parse_capture <- function(x) {
    suppressWarnings(as.POSIXct(as.character(x), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  combined$.capture_num <- as.numeric(parse_capture(combined$captured_at_utc))
  combined$.kickoff_num <- as.numeric(parse_capture(combined$kickoff_utc))
  combined <- combined |>
    dplyr::filter(
      is.finite(.capture_num),
      is.finite(.kickoff_num),
      .capture_num < .kickoff_num
    ) |>
    dplyr::arrange(season, week, player_id, .capture_num) |>
    dplyr::group_by(season, week, player_id) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(-.capture_num, -.kickoff_num) |>
    dplyr::arrange(season, week, position, player_display_name)

  wl31_atomic_write_csv(combined, feature_path)
}

final_features <- wl31_read_csv(feature_path)
coverage_after <- wl31_coverage_by_week(final_features, accuracy)
eligible_after <- intersect(wl31_eligible_weeks(coverage_after), final_weeks)
recovered <- if (nrow(backfilled)) nrow(backfilled) else 0L

status <- data.frame(
  generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  season = season,
  requested_weeks = paste(need_weeks, collapse = ","),
  commits_replayed = replayed,
  recovered_rows = recovered,
  archive_rows_after = nrow(final_features),
  eligible_weeks_after = paste(eligible_after, collapse = ","),
  status = if (all(need_weeks %in% eligible_after)) "complete" else "partial",
  stringsAsFactors = FALSE
)
wl31_atomic_write_csv(status, status_path)

cat("[3.1 LEARNING BACKFILL] Requested weeks: ", paste(need_weeks, collapse = ","), "\n", sep = "")
cat("[3.1 LEARNING BACKFILL] Commits replayed: ", replayed,
    " | recovered rows: ", recovered,
    " | eligible after: ",
    if (length(eligible_after)) paste(eligible_after, collapse = ",") else "none",
    "\n", sep = "")
