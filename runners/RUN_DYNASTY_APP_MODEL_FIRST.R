# ============================================================
# FANTASY MODEL 3.0.9 - MODEL-FIRST DYNASTY APP LAUNCHER
# ============================================================
# Sleeper = league/roster/player IDs/draft/picks only.
# Fantasy Model = weekly/season/career projections, role signals, dynasty value.

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
  setwd(root)
  source("config.R")

  required_model <- c(
    paste0("output/final_", CURRENT_SEASON, "_rankings.csv"),
    "output/final_dynasty_rankings.csv"
  )
  if (any(!file.exists(required_model))) stop("Required Fantasy Model projection outputs are missing. Run the model bootstrap first.")

  if (!file.exists(SLEEPER_COMPACT_PLAYERS_PATH) || !file.exists(FM3_APP_WEEKLY_SNAPSHOT_PATH)) {
    cat("[3.0.9] Compact app files missing; preparing them before Shiny starts...\n")
    source("runners/PREP_DYNASTY_APP_3_0_8.R", local = TRUE)
  }

  # Remove objects left by model training. Files on disk are preserved.
  global_names <- ls(envir = .GlobalEnv, all.names = TRUE)
  if (length(global_names)) rm(list = global_names, envir = .GlobalEnv)
  invisible(gc(full = TRUE))
  setwd(root)
  cat("[3.0.9] Starting model-first Dynasty GM app.\n")
  cat("[3.0.9] Live Sleeper connect uses lightweight league/roster/draft endpoints only.\n")
  shiny::runApp(root, launch.browser = TRUE)
})
