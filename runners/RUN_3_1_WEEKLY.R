# ============================================================
# FANTASY MODEL 3.1 - WEEKLY LEARN + PROJECT
# ============================================================
# Run after the prior NFL week is final. Historical training data is preserved
# unchanged; finalized current-season rows are appended to the live learning
# ledger, guarded challengers are retrained, then future projections refresh.

Sys.setenv(
  FM_RELEASE_VERSION = "3.1",
  FM_FORCE_REFRESH = "true",
  FM_REFRESH_DEFENSE = "true",
  FM_REFRESH_DYNASTY = "true",
  FM_31_CURRENT_SEASON_WEIGHT = Sys.getenv("FM_31_CURRENT_SEASON_WEIGHT", unset = "1.5")
)

source("pipeline/30_update_weekly_learning_store_3_1.R", local = FALSE)
source("pipeline/31_retrain_weekly_3_1.R", local = FALSE)
source("runners/RUN_3_1_AUTO_REFRESH.R", local = FALSE)
