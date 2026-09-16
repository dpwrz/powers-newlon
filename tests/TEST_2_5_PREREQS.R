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

source("config.R")
req_files <- c(
  "data/processed/weekly_model_table_2_3.csv",
  "output/weekly_2_4_validation_predictions.csv",
  "output/weekly_2_4_promotion.csv",
  "R/weekly_engine_25.R",
  "pipeline/22_train_validate_accuracy_tournament_2_5.R",
  "pipeline/23_project_weekly_2026_2_5.R"
)
missing <- req_files[!file.exists(req_files)]
if (length(missing)) stop("Missing 2.5 prerequisite(s): ", paste(missing, collapse = ", "))
ensure_packages(c("dplyr", "readr", "xgboost"))
source("R/weekly_engine_25.R")
weekly_names <- names(readr::read_csv("data/processed/weekly_model_table_2_3.csv", n_max = 1, show_col_types = FALSE, progress = FALSE))
missing_features <- setdiff(MODEL25_SAFE_FEATURES, weekly_names)
if (length(missing_features)) stop("2.5 historical feature contract missing: ", paste(missing_features, collapse = ", "))
if (length(intersect(MODEL25_SAFE_FEATURES, MODEL25_FORBIDDEN_FEATURES))) stop("2.5 leakage contract failed.")
cat("[2.5 TEST] Prerequisites OK. Safe features: ", length(MODEL25_SAFE_FEATURES), "\n", sep = "")
