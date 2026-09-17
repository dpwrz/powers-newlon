# ============================================================
# FANTASY MODEL 3.1 - PREGAME BENCHMARK ARCHIVE RUNNER
# ============================================================

find_project_root31_benchmark <- function(start = getwd()) {
  cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(cur, "config.R")) && dir.exists(file.path(cur, "pipeline"))) return(cur)
    parent <- dirname(cur)
    if (identical(parent, cur)) stop("Could not locate Fantasy Model project root.")
    cur <- parent
  }
}

root <- find_project_root31_benchmark()
setwd(root)
Sys.setenv(FM_RELEASE_VERSION = Sys.getenv("FM_RELEASE_VERSION", unset = "3.1"))
source("pipeline/29_capture_pregame_benchmarks_3_1.R")
