# ============================================================
# FANTASY MODEL 3.0.7 - CLEAN DYNASTY APP LAUNCHER
# ============================================================
# Purpose: launch Shiny in a low-memory R session. This intentionally clears
# objects left in .GlobalEnv by model/data-lab runs before starting the app.
# Generated model/data files on disk are NOT deleted.

local({
  find_root <- function() {
    wd <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
    candidates <- c(wd, file.path(wd, "Fantasy_Model_3_0_Dynasty_Intelligence"),
                    "/cloud/project/Fantasy_Model_3_0_Dynasty_Intelligence")
    p <- wd
    for (i in 0:5) {
      candidates <- c(candidates, p)
      parent <- dirname(p)
      if (identical(parent, p)) break
      p <- parent
    }
    candidates <- unique(candidates)
    ok <- vapply(candidates, function(x) {
      file.exists(file.path(x, "config.R")) && file.exists(file.path(x, "app.R")) && dir.exists(file.path(x, "R"))
    }, logical(1))
    hit <- candidates[ok]
    if (!length(hit)) stop("Fantasy Model 3.0 project root could not be found.")
    normalizePath(hit[[1]], winslash = "/", mustWork = TRUE)
  }

  root <- find_root()
  season_rank <- file.path(root, "output", "final_2026_rankings.csv")
  dynasty_rank <- file.path(root, "output", "final_dynasty_rankings.csv")
  if (!file.exists(season_rank) || !file.exists(dynasty_rank)) {
    stop("Required projection outputs are missing. Run the 3.0 bootstrap before launching the app.")
  }

  cat("[3.0.7] Clearing model-build objects from .GlobalEnv before Shiny launch...\n")
  global_names <- ls(envir = .GlobalEnv, all.names = TRUE)
  if (length(global_names)) rm(list = global_names, envir = .GlobalEnv)
  invisible(gc(full = TRUE))
  setwd(root)
  cat("[3.0.7] Starting Dynasty app from a clean R session.\n")
  shiny::runApp(root, launch.browser = TRUE)
})
