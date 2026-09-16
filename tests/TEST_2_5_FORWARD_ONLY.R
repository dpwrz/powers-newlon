# Contract test for Model 2.5 Live forward-only production behavior.
source("config.R")
path <- "pipeline/23_project_weekly_2026_2_5.R"
if (!file.exists(path)) stop("Missing ", path)
src <- paste(readLines(path, warn = FALSE), collapse = "\n")
required_tokens <- c(
  "Frozen completed rows restored",
  "wk25_predict_direct(obj, du)",
  "wk25_num(d$is_actual, 0) == 0",
  "Forward-only refresh complete"
)
missing <- required_tokens[!vapply(required_tokens, function(x) grepl(x, src, fixed = TRUE), logical(1))]
if (length(missing)) stop("Forward-only production contract missing token(s): ", paste(missing, collapse = ", "))
if (grepl("wk25_predict_direct(obj, d)", src, fixed = TRUE)) stop("Completed rows may still be sent to the 2.5 learner.")
weekly <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
if (file.exists(weekly)) {
  z <- tryCatch(readr::read_csv(weekly, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
  if (nrow(z) && "is_actual" %in% names(z)) {
    ia <- suppressWarnings(as.numeric(z$is_actual))
    cat("[2.5 FORWARD TEST] Completed rows: ", sum(ia == 1, na.rm = TRUE),
        " | unplayed rows: ", sum(ia == 0, na.rm = TRUE), "\n", sep = "")
  }
}
cat("[2.5 FORWARD TEST] Forward-only production contract OK.\n")
