# Pre-Week-1 regression test for empty current-season player stats.
source("config.R")
source("R/weekly_engine.R")
x <- normalize_weekly_stats21(data.frame())
stopifnot(nrow(x) == 0L)
stopifnot(all(c("player_id", "position", "season", "week", "weekly_fppg") %in% names(x)))
cat("[PASS] Empty current-season weekly stats retain canonical schema.\n")
