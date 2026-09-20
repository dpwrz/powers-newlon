# ============================================================
# FANTASY MODEL 3.1 - LIGHTWEIGHT LIVE ACCURACY REFRESH
# ============================================================

find_project_root31_accuracy <- function(start = getwd()) {
  cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(cur, "config.R")) && dir.exists(file.path(cur, "pipeline"))) return(cur)
    parent <- dirname(cur)
    if (identical(parent, cur)) stop("Could not locate Fantasy Model project root.")
    cur <- parent
  }
}

root <- find_project_root31_accuracy()
setwd(root)

source("config.R")
ensure_packages(c("dplyr", "readr", "jsonlite", "nflreadr"))

cat("\n============================================================\n")
cat(" FANTASY MODEL 3.1 - LIGHTWEIGHT ACCURACY REFRESH\n")
cat("============================================================\n")

stats <- tryCatch(
  nflreadr::load_player_stats(seasons = CURRENT_SEASON, summary_level = "week"),
  error = function(e) {
    warning("Current player stats refresh failed: ", conditionMessage(e))
    data.frame()
  }
)
schedule <- tryCatch(
  nflreadr::load_schedules(seasons = CURRENT_SEASON),
  error = function(e) {
    warning("Current schedule refresh failed: ", conditionMessage(e))
    data.frame()
  }
)

if (nrow(stats)) {
  readr::write_csv(stats, paste0("data/raw/player_weekly_stats_", CURRENT_SEASON, ".csv"))
}
if (nrow(schedule)) {
  readr::write_csv(schedule, paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv"))
}

source("pipeline/24_score_live_accuracy_2_5.R", local = FALSE)

Sys.setenv(
  FANTASY_MODEL_ROOT = root,
  FM_SEASON = as.character(CURRENT_SEASON)
)
source("scripts/patch_live_accuracy_snapshot.R", local = FALSE)

dir.create("web/public/data", recursive = TRUE, showWarnings = FALSE)
file.copy(
  "output/model_snapshot.json",
  "web/public/data/model_snapshot.json",
  overwrite = TRUE
)

cat("[3.1 ACCURACY] Accuracy refresh complete.\n")
