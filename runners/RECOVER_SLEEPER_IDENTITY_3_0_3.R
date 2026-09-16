# FANTASY MODEL 3.0.3 - SLEEPER IDENTITY RECOVERY
# Run this once after installing the 3.0.3 hotfix.
source("config.R")
source("R/sleeper_api.R")
cat("[3.0.3] Clearing old Sleeper player + league-state caches...\n")
fm3_clear_sleeper_cache(clear_players = TRUE)
cat("[3.0.3] Cache cleared. Launching Dynasty app...\n")
source("runners/RUN_DYNASTY_APP.R")
