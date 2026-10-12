# ============================================================
# FANTASY MODEL 3.1 - APPEND-ONLY CURRENT-SEASON TRAINING STORE
# ============================================================
# Historical 2016-2025 training data is read-only. Current-season rows come
# only from frozen production player-weeks that already passed the honest
# live-accuracy audit. Every completed-week cutoff is written to an immutable
# snapshot so a later retrain can never erase what an earlier model learned from.

source("config.R")
ensure_packages(c("dplyr", "readr", "nflreadr"))
source("R/live_refresh_engine_25.R")
source("R/weekly_engine_31.R")

dir.create("data/online", recursive = TRUE, showWarnings = FALSE)

hist_path <- "data/processed/weekly_model_table_2_3.csv"
ledger_path <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
accuracy_path <- paste0("output/live_accuracy_player_weeks_", CURRENT_SEASON, ".csv")
schedule_path <- paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv")
latest_path <- paste0("data/online/weekly_training_", CURRENT_SEASON, "_latest.csv")
manifest_path <- paste0("data/online/weekly_training_", CURRENT_SEASON, "_manifest.csv")

for (p in c(hist_path, ledger_path, accuracy_path)) {
  if (!file.exists(p)) stop("[3.1 ONLINE STORE] Missing required file: ", p)
}

hist_hash <- unname(tools::md5sum(hist_path))

schedule <- if (file.exists(schedule_path)) {
  tryCatch(readr::read_csv(schedule_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
} else data.frame()
if (!nrow(schedule)) {
  schedule <- tryCatch(nflreadr::load_schedules(seasons = CURRENT_SEASON), error = function(e) data.frame())
}
if (!nrow(schedule) || !"week" %in% names(schedule)) {
  stop("[3.1 ONLINE STORE] Cannot determine completed NFL weeks.")
}
if ("game_type" %in% names(schedule)) {
  gt <- toupper(trimws(as.character(schedule$game_type)))
  schedule <- schedule[is.na(gt) | gt == "" | gt == "REG", , drop = FALSE]
}
schedule$week <- suppressWarnings(as.integer(schedule$week))
schedule <- schedule[is.finite(schedule$week), , drop = FALSE]
schedule$.completed <- live25_completed_mask(schedule)

week_status <- schedule |>
  dplyr::group_by(week) |>
  dplyr::summarise(
    games = dplyr::n(),
    completed_games = sum(.completed, na.rm = TRUE),
    fully_completed = games > 0 && completed_games == games,
    .groups = "drop"
  )
full_weeks <- week_status$week[week_status$fully_completed %in% TRUE]
if (!length(full_weeks)) {
  stop("[3.1 ONLINE STORE] No fully completed regular-season week yet.")
}
cutoff_week <- max(full_weeks, na.rm = TRUE)

ledger <- readr::read_csv(ledger_path, show_col_types = FALSE, progress = FALSE)
accuracy <- readr::read_csv(accuracy_path, show_col_types = FALSE, progress = FALSE)

required_ledger <- c(
  "week", "player_id", "player_display_name", "position", "team", "opponent",
  "is_actual", "actual_weekly_fppg", MODEL25_SAFE_FEATURES
)
missing_ledger <- setdiff(required_ledger, names(ledger))
if (length(missing_ledger)) {
  stop("[3.1 ONLINE STORE] Frozen ledger is missing leak-safe training field(s): ",
       paste(missing_ledger, collapse = ", "))
}

required_accuracy <- c(
  "week", "player_id", "position", "projection", "actual_fppg",
  "starter_cohort", "relevant_cohort", "captured_at", "model_version"
)
missing_accuracy <- setdiff(required_accuracy, names(accuracy))
if (length(missing_accuracy)) {
  stop("[3.1 ONLINE STORE] Accuracy audit is missing field(s): ",
       paste(missing_accuracy, collapse = ", "))
}

ledger$week <- suppressWarnings(as.integer(ledger$week))
ledger$player_id <- as.character(ledger$player_id)
ledger$is_actual <- suppressWarnings(as.numeric(ledger$is_actual))
accuracy$week <- suppressWarnings(as.integer(accuracy$week))
accuracy$player_id <- as.character(accuracy$player_id)

honest <- accuracy |>
  dplyr::filter(
    is.finite(week), week <= cutoff_week,
    nzchar(player_id), position %in% POSITIONS,
    is.finite(suppressWarnings(as.numeric(actual_fppg))),
    is.finite(suppressWarnings(as.numeric(projection)))
  ) |>
  dplyr::transmute(
    week,
    player_id,
    production_base_31 = suppressWarnings(as.numeric(projection)),
    audited_actual_fppg = suppressWarnings(as.numeric(actual_fppg)),
    starter_cohort = as.logical(starter_cohort),
    relevant_cohort = as.logical(relevant_cohort),
    captured_at = as.character(captured_at),
    source_model_version = as.character(model_version)
  ) |>
  dplyr::distinct(week, player_id, .keep_all = TRUE)

if (!nrow(honest)) {
  stop("[3.1 ONLINE STORE] No honestly audited completed player-weeks are available through Week ", cutoff_week, ".")
}

online <- ledger |>
  dplyr::filter(
    week <= cutoff_week,
    is_actual == 1,
    position %in% POSITIONS,
    nzchar(player_id)
  ) |>
  dplyr::inner_join(honest, by = c("week", "player_id")) |>
  dplyr::mutate(
    season = CURRENT_SEASON,
    weekly_fppg = audited_actual_fppg,
    actual_fppg = audited_actual_fppg,
    online_training_cutoff_week = cutoff_week,
    online_training_source = "honest_frozen_production_ledger"
  ) |>
  dplyr::filter(is.finite(weekly_fppg), is.finite(production_base_31)) |>
  dplyr::arrange(week, position, player_id)

# Sanity-check that the frozen ledger and audited actual agree before training.
ledger_actual <- suppressWarnings(as.numeric(online$actual_weekly_fppg))
delta <- abs(ledger_actual - online$audited_actual_fppg)
bad_delta <- is.finite(delta) & delta > 1e-8
if (any(bad_delta)) {
  stop("[3.1 ONLINE STORE] Actual-points mismatch between frozen ledger and audited scorer for ",
       sum(bad_delta), " row(s).")
}

# Derived 3.1 fields are deterministic functions of leak-safe pre-kickoff inputs.
online <- wk31_add_features(online)

keep <- unique(c(
  "season", "week", "player_id", "player_display_name", "position", "team", "opponent",
  MODEL31_SAFE_FEATURES,
  "weekly_fppg", "actual_fppg", "production_base_31",
  "starter_cohort", "relevant_cohort", "captured_at", "source_model_version",
  "online_training_cutoff_week", "online_training_source"
))
online <- online[, intersect(keep, names(online)), drop = FALSE]

snapshot_path <- sprintf(
  "data/online/weekly_training_%s_through_week_%02d.csv",
  CURRENT_SEASON, cutoff_week
)

if (file.exists(snapshot_path)) {
  existing_hash <- unname(tools::md5sum(snapshot_path))
  cat("[3.1 ONLINE STORE] Immutable cutoff already exists: ", snapshot_path,
      " | md5=", existing_hash, "\n", sep = "")
} else {
  readr::write_csv(online, snapshot_path)
  cat("[3.1 ONLINE STORE] Wrote immutable cutoff: ", snapshot_path,
      " | rows=", nrow(online), "\n", sep = "")
}

# latest.csv is only a convenience pointer. Immutable cutoff files are canonical.
file.copy(snapshot_path, latest_path, overwrite = TRUE)
snapshot_hash <- unname(tools::md5sum(snapshot_path))

row <- data.frame(
  season = CURRENT_SEASON,
  cutoff_week = cutoff_week,
  generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  snapshot_path = snapshot_path,
  snapshot_rows = nrow(readr::read_csv(snapshot_path, show_col_types = FALSE, progress = FALSE)),
  snapshot_md5 = snapshot_hash,
  historical_path = hist_path,
  historical_md5 = hist_hash,
  stringsAsFactors = FALSE
)

manifest <- if (file.exists(manifest_path)) {
  tryCatch(readr::read_csv(manifest_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
} else data.frame()
if (!nrow(manifest) || !any(suppressWarnings(as.integer(manifest$cutoff_week)) == cutoff_week)) {
  manifest <- dplyr::bind_rows(manifest, row) |>
    dplyr::arrange(cutoff_week)
  readr::write_csv(manifest, manifest_path)
}

# Historical source must remain byte-for-byte unchanged.
hist_hash_after <- unname(tools::md5sum(hist_path))
if (!identical(hist_hash, hist_hash_after)) {
  stop("[3.1 ONLINE STORE] Historical training store changed unexpectedly. Aborting.")
}

cat("[3.1 ONLINE STORE] Historical data preserved: ", hist_hash, "\n", sep = "")
cat("[3.1 ONLINE STORE] Latest fully completed week: ", cutoff_week,
    " | honest current-season rows: ", nrow(online), "\n", sep = "")

invisible(list(cutoff_week = cutoff_week, rows = nrow(online), snapshot = snapshot_path))
