# ============================================================
# STEP 20 - BUILD GENERIC 3.0 DYNASTY VALUE / PICK TABLES
# ============================================================
source("config.R")
source("R/league_engine.R")
source("R/player_identity.R")
source("R/dynasty_value_engine.R")
ensure_packages(c("dplyr", "readr"))

profiles <- read_league_profiles()
settings <- read_league_profile(as.character(profiles$league_name[1]))
fm3_write_dynasty_value_outputs(settings, sleeper_players = NULL)
cat("[3.0 DYNASTY] Generic model values and rookie-pick curve built. League-specific values are rebuilt after Sleeper sync in the app.\n")
