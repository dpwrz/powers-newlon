# ============================================================
# FANTASY MODEL 3.1 - WEEKLY LEARNING + PRODUCTION REFRESH
# ============================================================

source("pipeline/30_train_weekly_live_3_1.R")

# Apply any newly promoted weekly learner only to unplayed rows. Completed rows
# remain frozen, and a failed weekly challenger leaves the incumbent untouched.
Sys.setenv(
  FM_RELEASE_VERSION = "3.1",
  FM_FORCE_REFRESH = "true",
  FM_REFRESH_DEFENSE = "true",
  FM_REFRESH_DYNASTY = "true"
)
source("runners/RUN_3_1_AUTO_REFRESH.R")
