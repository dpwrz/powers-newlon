# Fantasy Model 3.1 - explicit weekly commit.
# Use after final games / official weekly data are available.
Sys.setenv(
  FM_RELEASE_VERSION = "3.1",
  FM_FORCE_REFRESH = "true",
  FM_REFRESH_DEFENSE = "true",
  FM_REFRESH_DYNASTY = "true"
)
source("runners/RUN_3_1_AUTO_REFRESH.R")
