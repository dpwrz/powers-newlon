# FANTASY MODEL 3.0.4 - SLEEPER IDENTITY RECOVERY
# Run once after overlaying the 3.0.4 hotfix.
source("config.R")
source("R/sleeper_api.R")
cat("[3.0.4] Clearing Sleeper player + league-state caches...\n")
fm3_clear_sleeper_cache(clear_players = TRUE)
id_path <- "data/processed/sleeper_player_identity_3_0.csv"
if (file.exists(id_path)) unlink(id_path, force = TRUE)
cat("[3.0.4] Cache cleared. Launching Dynasty app...\n")
source("runners/RUN_DYNASTY_APP.R")
