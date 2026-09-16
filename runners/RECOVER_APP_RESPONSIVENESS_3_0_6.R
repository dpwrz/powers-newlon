# ============================================================
# FANTASY MODEL 3.0.6 - APP RESPONSIVENESS RECOVERY
# ============================================================
# This patch does NOT clear Sleeper data and does NOT rerun model pipelines.
# It simply launches the revised cached-snapshot Shiny application.

.fm3_find_root <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  candidates <- wd
  p <- wd
  for (i in 0:5) {
    candidates <- c(candidates, p)
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  candidates <- c(
    candidates,
    file.path(wd, "Fantasy_Model_3_0_Dynasty_Intelligence"),
    "/cloud/project/Fantasy_Model_3_0_Dynasty_Intelligence"
  )
  candidates <- unique(candidates)
  ok <- vapply(candidates, function(x) {
    file.exists(file.path(x, "config.R")) &&
      file.exists(file.path(x, "app.R")) &&
      dir.exists(file.path(x, "R"))
  }, logical(1))
  hits <- candidates[ok]
  if (!length(hits)) stop("Fantasy Model 3.0 project root could not be found.")
  root <- normalizePath(hits[[1]], winslash = "/", mustWork = TRUE)
  if (!identical(normalizePath(getwd(), winslash = "/", mustWork = TRUE), root)) setwd(root)
  invisible(root)
}

.fm3_find_root()
source("config.R")
cat("Opening Fantasy Model ", APP_VERSION, " with responsive cached-feature mode...\n", sep = "")
source("app.R")
