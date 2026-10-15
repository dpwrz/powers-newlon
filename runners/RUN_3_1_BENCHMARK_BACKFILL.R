# ============================================================
# FANTASY MODEL 3.1 - BENCHMARK BACKFILL RUNNER
# ============================================================

find_project_root31_backfill <- function(start = getwd()) {
  cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(cur, "config.R")) && dir.exists(file.path(cur, "pipeline"))) return(cur)
    parent <- dirname(cur)
    if (identical(parent, cur)) stop("Could not locate Fantasy Model project root.")
    cur <- parent
  }
}

root <- find_project_root31_backfill()
setwd(root)
Sys.setenv(FM_RELEASE_VERSION = Sys.getenv("FM_RELEASE_VERSION", unset = "3.1"))
source("pipeline/30_backfill_benchmark_providers_3_1.R")
