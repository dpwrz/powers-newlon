# ============================================================
# FANTASY MODEL 3.1 - APPEND-ONLY CURRENT-SEASON LEARNING STORE
# ============================================================
# Historical weekly training rows remain untouched. Current-season finalized
# player-weeks are captured into an append-only ledger. The derived training
# table always uses the immutable historical base plus the latest captured
# revision of each current-season player-week.

source("config.R")
ensure_packages(c("dplyr", "readr", "nflreadr"))
source("R/live_refresh_engine_25.R")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("data/state", recursive = TRUE, showWarnings = FALSE)

historical_path <- "data/processed/weekly_model_table_2_3.csv"
live_build_path <- paste0("data/processed/weekly_model_table_3_1_live_build_", CURRENT_SEASON, ".csv")
ledger_path <- paste0("data/processed/weekly_model_", CURRENT_SEASON, "_learning_ledger.csv")
training_path <- "data/processed/weekly_model_table_3_1_training.csv"
audit_path <- paste0("data/state/weekly_learning_audit_", CURRENT_SEASON, ".csv")

if (!file.exists(historical_path)) stop("Missing immutable historical weekly store: ", historical_path)

# Build the same leak-free weekly feature table through the current season, but
# write it to an isolated path so the validated historical store is never
# overwritten.
Sys.setenv(
  FM_WEEKLY_DATA_END = as.character(CURRENT_SEASON),
  FM_WEEKLY_MODEL_TABLE_OUT = live_build_path
)
source("pipeline/09_build_weekly_data.R", local = FALSE)

historical <- readr::read_csv(historical_path, show_col_types = FALSE, progress = FALSE)
live_build <- readr::read_csv(live_build_path, show_col_types = FALSE, progress = FALSE)

historical$season <- as.integer(historical$season)
historical$week <- as.integer(historical$week)
historical$player_id <- as.character(historical$player_id)
live_build$season <- as.integer(live_build$season)
live_build$week <- as.integer(live_build$week)
live_build$player_id <- as.character(live_build$player_id)
live_build$team <- as.character(live_build$team)

# The historical base must remain historical. If this fails, stop rather than
# silently replacing validated rows with a rebuilt version.
if (any(historical$season > TRAIN_END, na.rm = TRUE)) {
  stop("Historical base unexpectedly contains season(s) after TRAIN_END=", TRAIN_END, ".")
}

current <- live_build |>
  dplyr::filter(season == CURRENT_SEASON)

# Only finalized games can enter training. This prevents an in-progress Sunday
# game from ever becoming a target row.
schedule <- tryCatch(
  nflreadr::load_schedules(seasons = CURRENT_SEASON),
  error = function(e) data.frame()
)
if (!nrow(schedule)) {
  schedule_path <- paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv")
  if (file.exists(schedule_path)) {
    schedule <- tryCatch(
      readr::read_csv(schedule_path, show_col_types = FALSE, progress = FALSE),
      error = function(e) data.frame()
    )
  }
}
if (!nrow(schedule) || !all(c("week", "home_team", "away_team") %in% names(schedule))) {
  stop("A current-season schedule with completion state is required before weekly learning.")
}

schedule$week <- as.integer(schedule$week)
done <- live25_completed_mask(schedule)
final_teams <- dplyr::bind_rows(
  schedule[done, , drop = FALSE] |>
    dplyr::transmute(season = CURRENT_SEASON, week, team = as.character(home_team)),
  schedule[done, , drop = FALSE] |>
    dplyr::transmute(season = CURRENT_SEASON, week, team = as.character(away_team))
) |>
  dplyr::filter(is.finite(week), !is.na(team), nzchar(team)) |>
  dplyr::distinct(season, week, team)

current_final <- current |>
  dplyr::inner_join(final_teams, by = c("season", "week", "team")) |>
  dplyr::filter(is.finite(weekly_fppg), !is.na(player_id), nzchar(player_id))

if (!nrow(current_final)) stop("No finalized current-season player-weeks are available for weekly learning.")

captured_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
snapshot_week <- max(current_final$week, na.rm = TRUE)
current_capture <- current_final |>
  dplyr::mutate(
    learning_captured_at_utc = captured_at,
    learning_snapshot_week = as.integer(snapshot_week)
  )

# Append, never replace. Prior snapshots remain available for audit/recovery.
old_ledger <- if (file.exists(ledger_path)) {
  tryCatch(readr::read_csv(ledger_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
} else data.frame()

# Keep column types compatible across captures.
char_cols <- unique(c(
  names(current_capture)[vapply(current_capture, is.character, logical(1))],
  "learning_captured_at_utc", "player_id", "player_display_name", "position", "team", "opponent"
))
for (nm in intersect(char_cols, names(old_ledger))) old_ledger[[nm]] <- as.character(old_ledger[[nm]])
for (nm in intersect(char_cols, names(current_capture))) current_capture[[nm]] <- as.character(current_capture[[nm]])

ledger <- dplyr::bind_rows(old_ledger, current_capture)
readr::write_csv(ledger, ledger_path)

# Training uses the newest preserved revision of each current-season
# player-week, while every prior revision remains in the append-only ledger.
current_latest <- ledger |>
  dplyr::mutate(
    season = as.integer(season),
    week = as.integer(week),
    player_id = as.character(player_id),
    learning_captured_at_utc = as.character(learning_captured_at_utc)
  ) |>
  dplyr::filter(season == CURRENT_SEASON) |>
  dplyr::arrange(season, week, player_id, learning_captured_at_utc) |>
  dplyr::group_by(season, week, player_id) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup()

metadata_cols <- c("learning_captured_at_utc", "learning_snapshot_week")
current_training <- current_latest |>
  dplyr::select(-dplyr::any_of(metadata_cols))

# Historical columns define the contract. Current rows are aligned to that
# exact schema so weekly learning cannot mutate past feature definitions.
missing_current <- setdiff(names(historical), names(current_training))
for (nm in missing_current) current_training[[nm]] <- NA
extra_current <- setdiff(names(current_training), names(historical))
if (length(extra_current)) current_training <- current_training |> dplyr::select(-dplyr::all_of(extra_current))
current_training <- current_training[, names(historical), drop = FALSE]

training <- dplyr::bind_rows(historical, current_training) |>
  dplyr::arrange(season, week, position, player_display_name)

if (nrow(training |> dplyr::filter(season <= TRAIN_END)) != nrow(historical)) {
  stop("Historical row-count guard failed while assembling weekly learning table.")
}

readr::write_csv(training, training_path)

audit_row <- data.frame(
  captured_at_utc = captured_at,
  historical_rows = nrow(historical),
  historical_min_season = min(historical$season, na.rm = TRUE),
  historical_max_season = max(historical$season, na.rm = TRUE),
  ledger_rows = nrow(ledger),
  current_unique_player_weeks = nrow(current_latest),
  latest_finalized_week = snapshot_week,
  combined_training_rows = nrow(training),
  current_season_weight = suppressWarnings(as.numeric(Sys.getenv("FM_31_CURRENT_SEASON_WEIGHT", unset = "1.5"))),
  stringsAsFactors = FALSE
)
old_audit <- if (file.exists(audit_path)) {
  tryCatch(readr::read_csv(audit_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
} else data.frame()
readr::write_csv(dplyr::bind_rows(old_audit, audit_row), audit_path)

cat("\n[3.1 LEARNING] Historical rows preserved: ", nrow(historical), "\n", sep = "")
cat("[3.1 LEARNING] Current finalized player-weeks available: ", nrow(current_latest), "\n", sep = "")
cat("[3.1 LEARNING] Latest finalized week: ", snapshot_week, "\n", sep = "")
cat("[3.1 LEARNING] Append-only ledger rows: ", nrow(ledger), "\n", sep = "")
cat("[3.1 LEARNING] Combined training rows: ", nrow(training), "\n", sep = "")
