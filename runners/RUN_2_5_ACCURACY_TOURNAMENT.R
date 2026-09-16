# ---- Model 2.5 project-root bootstrap ---------------------------------------
.fm25_project_root <- function() {
  script_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  candidates <- character()
  if (!is.null(script_path) && length(script_path) == 1 && nzchar(script_path)) {
    p <- normalizePath(script_path, winslash = "/", mustWork = FALSE)
    d <- dirname(p)
    candidates <- c(candidates, d, dirname(d))
  }
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  p <- wd
  for (i in 0:6) {
    candidates <- c(candidates, p)
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  candidates <- unique(candidates[nzchar(candidates)])
  ok <- vapply(candidates, function(x) {
    file.exists(file.path(x, "config.R")) &&
      dir.exists(file.path(x, "R")) &&
      dir.exists(file.path(x, "pipeline"))
  }, logical(1))
  hits <- candidates[ok]
  if (!length(hits)) {
    stop(
      "Could not locate the Fantasy Model project root. Expected a folder containing config.R, R/, and pipeline/. ",
      "Current working directory: ", getwd()
    )
  }
  project_root <- normalizePath(hits[[1]], winslash = "/", mustWork = TRUE)
  if (!identical(normalizePath(getwd(), winslash = "/", mustWork = TRUE), project_root)) {
    setwd(project_root)
  }
  cat("[2.5] Project root: ", project_root, "\n", sep = "")
  invisible(project_root)
}
.fm25_project_root()
# -----------------------------------------------------------------------------

# Validate Model 2.5 only. This does not rewrite live production projections.
source("config.R")
ensure_packages(c("dplyr", "readr", "xgboost"))
cat("\n=== FANTASY MODEL 2.5 - ACCURACY TOURNAMENT (VALIDATE ONLY) ===\n")
source("pipeline/22_train_validate_accuracy_tournament_2_5.R")
cat("\nReview output/weekly_2_5_champion_manifest.csv and output/weekly_2_5_model_quality_report.txt.\n")
cat("To apply only positions that passed every guardrail, run: source(\"pipeline/23_project_weekly_2026_2_5.R\")\n")
