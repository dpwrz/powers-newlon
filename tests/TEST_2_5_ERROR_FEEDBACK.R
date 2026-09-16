# Contract test for Model 2.5 final-forecast error feedback.
source("config.R")
req <- c(
  "R/live_error_feedback_engine_25.R",
  "pipeline/25_train_validate_error_feedback_2_5.R",
  "pipeline/23_project_weekly_2026_2_5.R",
  "runners/RUN_2_5_AUTO_REFRESH.R"
)
miss <- req[!file.exists(req)]
if (length(miss)) stop("Missing error-feedback file(s): ", paste(miss, collapse = ", "))

src23 <- paste(readLines("pipeline/23_project_weekly_2026_2_5.R", warn = FALSE), collapse = "\n")
src_auto <- paste(readLines("runners/RUN_2_5_AUTO_REFRESH.R", warn = FALSE), collapse = "\n")
for (tok in c("live_accuracy_player_weeks_", "feedback_correction_25", "feedback_promoted_25", "is_actual, 0) == 0")) {
  if (!grepl(tok, src23, fixed = TRUE)) stop("2.5 final-error feedback contract missing: ", tok)
}
if (!grepl("Scoring newly completed games first", src_auto, fixed = TRUE)) stop("Auto refresh does not score completed games before reprojection.")
cat("[2.5 FEEDBACK TEST] Final-model error feedback contract OK.\n")

promo <- "output/weekly_2_5_feedback_promotion.csv"
if (file.exists(promo)) {
  z <- readr::read_csv(promo, show_col_types = FALSE, progress = FALSE)
  cat("[2.5 FEEDBACK TEST] Existing feedback promotion artifact:\n")
  print(z)
} else {
  cat("[2.5 FEEDBACK TEST] Feedback tournament has not been run yet. Run runners/RUN_2_5_FEEDBACK_TOURNAMENT.R before expecting point corrections.\n")
}
