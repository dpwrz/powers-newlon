# ============================================================
# FANTASY MODEL 3.0 - RESUME AFTER LIVE ROLE PROJECTION
# ============================================================
# Use when output/weekly_role_forecasts_3_0.csv already exists.

.fm3_bootstrap_root <- function() {
  candidates <- character()
  frames <- sys.frames()
  if (length(frames)) {
    for (fr in frames) {
      of <- fr$ofile
      if (!is.null(of) && length(of) == 1L && !is.na(of) && nzchar(of)) {
        f <- tryCatch(normalizePath(of, winslash = "/", mustWork = TRUE), error = function(e) NA_character_)
        if (!is.na(f)) {
          d <- dirname(f)
          candidates <- c(candidates, d, dirname(d))
        }
      }
    }
  }
  wd <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
  p <- wd
  for (i in 0:5) {
    candidates <- c(candidates, p)
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  candidates <- c(candidates, tryCatch(list.dirs(wd, recursive = FALSE, full.names = TRUE), error = function(e) character()))
  candidates <- unique(candidates[nzchar(candidates)])
  ok <- vapply(candidates, function(x) {
    file.exists(file.path(x, "config.R")) && dir.exists(file.path(x, "runners")) && dir.exists(file.path(x, "pipeline"))
  }, logical(1))
  hits <- candidates[ok]
  if (!length(hits)) stop("Fantasy Model 3.0 project root not found. Set working directory to the project folder first.")
  root <- normalizePath(hits[[1]], winslash = "/", mustWork = TRUE)
  if (!identical(normalizePath(getwd(), winslash = "/", mustWork = TRUE), root)) setwd(root)
  invisible(root)
}
.fm3_bootstrap_root()

source("config.R")
role_path <- "output/weekly_role_forecasts_3_0.csv"
if (!file.exists(role_path)) {
  stop("Live role forecasts are missing. Run source(\"runners/RESUME_3_0_FROM_ROLE_PROJECTION.R\") instead.")
}

cat("[3.0] Existing live role forecasts found; skipping Steps 17-19.\n")
invisible(gc(full = TRUE))
source("pipeline/21_audit_decision_relevant_error_3_0.R")
invisible(gc(full = TRUE))
source("pipeline/20_build_dynasty_intelligence_3_0.R")
invisible(gc(full = TRUE))
cat("\n[3.0] Resume complete. Launch with source(\"runners/RUN_DYNASTY_APP.R\").\n")
