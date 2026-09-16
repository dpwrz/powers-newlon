# Fantasy Model 3.0.x production ENGINE repository integrity test.
# Web 1.0 is intentionally a separate Git repository and is not required here.

required <- c(
  "config.R",
  "RUN_AUTO_REFRESH.R",
  "runners/RUN_3_0_AUTO_REFRESH.R",
  "runners/RUN_2_5_AUTO_REFRESH.R",
  "R/weekly_engine_25.R",
  "R/live_refresh_engine_25.R",
  "R/live_error_feedback_engine_25.R",
  "R/league_scoring_engine_25.R",
  "R/live_game_state_engine_25.R",
  "pipeline/23_project_weekly_2026_2_5.R",
  "pipeline/24_score_live_accuracy_2_5.R",
  "scripts/export_model_snapshot.R"
)

missing <- required[!file.exists(required)]
if (length(missing)) {
  stop(
    "Fantasy Model 3.0 engine files missing from the Git repository: ",
    paste(missing, collapse = ", ")
  )
}

if (!file.exists("VERSION.txt")) stop("VERSION.txt is missing")

ver <- trimws(readLines("VERSION.txt", warn = FALSE)[1])
if (!grepl("^3\\.0(?:\\.|$)", ver)) {
  stop("Expected a Fantasy Model 3.0.x release, found VERSION.txt = ", ver)
}

cat("[PASS] Fantasy Model ", ver,
    " engine repository structure is complete. Web is deployed from its separate repository.\n",
    sep = "")
