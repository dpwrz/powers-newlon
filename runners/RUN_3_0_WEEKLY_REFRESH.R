# ============================================================
# FANTASY MODEL 3.0 - WEEKLY / DRAFT-DAY REFRESH
# ============================================================
# Rebuild the current player-week data, run the locked 2.4.3 live projection
# chain using already-trained/validated models, then update 3.0 role forecasts.
# ---- 3.0 project-root bootstrap (Posit/RStudio safe) ----
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
    file.exists(file.path(x, "config.R")) && file.exists(file.path(x, "VERSION.txt")) &&
      dir.exists(file.path(x, "runners")) && dir.exists(file.path(x, "pipeline"))
  }, logical(1))
  hits <- candidates[ok]
  if (!length(hits)) stop("Fantasy Model 3.0 project root not found. Set working directory to the project folder first.")
  root <- normalizePath(hits[[1]], winslash = "/", mustWork = TRUE)
  if (!identical(normalizePath(getwd(), winslash = "/", mustWork = TRUE), root)) setwd(root)
  invisible(root)
}
.fm3_bootstrap_root()
# -----------------------------------------------------------

source("pipeline/09_build_weekly_data.R")
source("pipeline/16_project_weekly_2026_2_4.R")
source("pipeline/19_project_role_context_2026_3_0.R")
cat("[3.0 REFRESH] Weekly projections and role context refreshed. Sleeper league/draft state refreshes inside the app.\n")
