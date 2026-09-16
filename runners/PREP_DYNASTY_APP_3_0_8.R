# ============================================================
# FANTASY MODEL 3.0.8 - PREPARE MODEL-FIRST DYNASTY APP DATA
# ============================================================
# Run this outside Shiny. It creates two compact files:
#   1) Sleeper player identity only (IDs/name/position/team)
#   2) Fantasy Model current-week projection/role snapshot
# No Sleeper projections/rankings are imported or used.

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
    ok <- vapply(candidates, function(x) file.exists(file.path(x, "config.R")) && dir.exists(file.path(x, "R")), logical(1))
    hit <- candidates[ok]
    if (!length(hit)) stop("Fantasy Model 3.0 project root could not be found.")
    normalizePath(hit[[1]], winslash = "/", mustWork = TRUE)
  }

  root <- find_root()
  setwd(root)
  source("config.R")
  source("R/sleeper_api.R")
  source("R/player_identity.R")
  source("R/app_data_bridge.R")

  cat("\n[3.0.8] Preparing lightweight Dynasty app data...\n")
  cat("[3.0.8] Sleeper is used for player identity only during this prep.\n")

  players <- sleeper_sync_compact_players(force = FALSE)
  cat("[3.0.8] Compact Sleeper identity rows:", nrow(players), "\n")

  model_players <- fm3_model_player_frame()
  identity <- fm3_build_identity_map(players, model_players)
  cat("[3.0.8] Model identity links:", nrow(identity), "\n")

  fm3_build_current_week_snapshot(force = TRUE)

  rm(players, model_players, identity)
  invisible(gc(full = TRUE))
  cat("[3.0.8] Preparation complete. Live Shiny connection will not download /players/nfl.\n\n")
})
