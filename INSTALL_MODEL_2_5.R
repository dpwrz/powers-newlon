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
ensure_packages(c("dplyr", "readr", "xgboost"))
cat("Model 2.5 dependencies installed.\n")
cat("Next: source(\"tests/TEST_2_5_PREREQS.R\")\n")
cat("Then: source(\"runners/RUN_2_5_ACCURACY_TOURNAMENT.R\")\n")
