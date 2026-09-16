# ============================================================
# FANTASY MODEL 2.1 - WEEKLY PREREQUISITE CHECK
# ============================================================
source("config.R")
ensure_packages(c("readr", "nflreadr"))

cat("\n========================================\n")
cat(" FANTASY MODEL 2.1 - WEEKLY PREREQS\n")
cat("========================================\n\n")

checks <- c(
  "Season 2.0 projections" = paste0("output/", CURRENT_SEASON, "_projections.csv"),
  "Validated season feature store" = "data/processed/model_table.csv",
  "2026 season projection feature store" = paste0("data/processed/projection_table_", CURRENT_SEASON, ".csv"),
  "Base player stats" = "data/raw/player_stats.csv",
  "Base team stats" = "data/raw/team_stats.csv"
)
all_ok <- TRUE
for (nm in names(checks)) {
  ok <- file.exists(checks[[nm]])
  cat(if (ok) "PASS - " else "FAIL - ", nm, "\n", sep = "")
  all_ok <- all_ok && ok
}

# Schedule availability is checked live because future matchups are foundational.
sched_ok <- tryCatch({
  s <- nflreadr::load_schedules(seasons = CURRENT_SEASON)
  nrow(s) > 0 && all(c("season", "week", "home_team", "away_team") %in% names(s))
}, error = function(e) FALSE)
cat(if (sched_ok) "PASS - " else "FAIL - ", CURRENT_SEASON, " schedule available\n", sep = "")
all_ok <- all_ok && sched_ok

# Historical weekly stats must be available. Only fetch one season for this light test.
weekly_ok <- tryCatch({
  w <- nflreadr::load_player_stats(seasons = TRAIN_END, summary_level = "week")
  nrow(w) > 0 && all(c("season", "week", "player_id") %in% names(w))
}, error = function(e) FALSE)
cat(if (weekly_ok) "PASS - " else "FAIL - ", "Weekly player stats available\n", sep = "")
all_ok <- all_ok && weekly_ok

cat("\nOptional live enrichments (failure does not block 2.1):\n")
snap_ok <- tryCatch(nrow(nflreadr::load_snap_counts(seasons = TRAIN_END)) > 0, error = function(e) FALSE)
cat(if (snap_ok) "PASS - " else "WARN - ", "PFR snap-count feed\n", sep = "")
cat("INFO - NFLverse injury reports currently end after 2024; 2.1 preserves historical injury features and treats current injury data as optional.\n")

if (!all_ok) stop("One or more required Fantasy Model 2.1 prerequisites failed. See FAIL lines above.")
cat("\nPASS: Fantasy Model 2.1 weekly foundation prerequisites are available.\n")
cat("Next: source(\"MOBILE_RUN_WEEKLY.R\")\n")
