# ============================================================
# FANTASY MODEL 2.2 - WEEKLY ACCURACY PREREQUISITE CHECK
# ============================================================
source("config.R")
ensure_packages(c("readr", "nflreadr", "rpart"))
cat("\n========================================\n")
cat(" FANTASY MODEL 2.2 - PREREQUISITES\n")
cat("========================================\n\n")
checks <- c(
  "Validated 2.0 season projection" = paste0("output/", CURRENT_SEASON, "_projections.csv"),
  "Validated season feature store" = "data/processed/model_table.csv",
  "Base player stats" = "data/raw/player_stats.csv",
  "Base team stats" = "data/raw/team_stats.csv"
)
all_ok <- TRUE
for (nm in names(checks)) {
  ok <- file.exists(checks[[nm]])
  cat(if (ok) "PASS - " else "FAIL - ", nm, "\n", sep = "")
  all_ok <- all_ok && ok
}
sched_ok <- tryCatch({
  s <- nflreadr::load_schedules(seasons = CURRENT_SEASON)
  nrow(s) > 0 && all(c("season", "week", "home_team", "away_team") %in% names(s))
}, error = function(e) FALSE)
cat(if (sched_ok) "PASS - " else "FAIL - ", CURRENT_SEASON, " schedule/feed\n", sep = "")
all_ok <- all_ok && sched_ok
week_ok <- tryCatch({
  w <- nflreadr::load_player_stats(seasons = TRAIN_END, summary_level = "week")
  nrow(w) > 0 && all(c("season", "week", "player_id") %in% names(w))
}, error = function(e) FALSE)
cat(if (week_ok) "PASS - " else "FAIL - ", "Weekly player stats\n", sep = "")
all_ok <- all_ok && week_ok
cat("\nOptional accuracy feeds:\n")
snap_ok <- tryCatch(nrow(nflreadr::load_snap_counts(seasons = TRAIN_END)) > 0, error = function(e) FALSE)
cat(if (snap_ok) "PASS - " else "WARN - ", "PFR offensive snap counts\n", sep = "")
ngs_ok <- tryCatch(nrow(nflreadr::load_nextgen_stats(seasons = TRAIN_END, stat_type = "passing")) > 0, error = function(e) FALSE)
cat(if (ngs_ok) "PASS - " else "WARN - ", "Weekly Next Gen Stats\n", sep = "")
pbp_ok <- file.exists("data/raw/weekly_defense_context_raw.csv")
cat(if (pbp_ok) "PASS - " else "INFO - ", "Compact weekly PBP defense checkpoint", if (pbp_ok) " (fast run available)\n" else " (full weekly run will build it)\n", sep = "")
cat("INFO - nflverse injury data currently ends after 2024; 2.2 keeps historical injury signal and leaves current availability hook optional.\n")
if (!all_ok) stop("One or more required Fantasy Model 2.2 prerequisites failed.")
cat("\nPASS: Fantasy Model 2.2 prerequisites are available.\n")
if (pbp_ok) cat("Recommended: source(\"MOBILE_RUN_WEEKLY_FAST.R\")\n") else cat("Next: source(\"MOBILE_RUN_WEEKLY.R\")\n")
