# ============================================================
# FANTASY MODEL 3.2 - OPTIMIZE + GUARDED REFRESH
# ============================================================
# Current-season offensive/defensive context must exist BEFORE the tournament;
# otherwise 2026 contextual factors collapse to zero and cannot be evaluated.
tryCatch(
  source("maintenance/REFRESH_WEEKLY_DEFENSE_CURRENT.R", local = FALSE),
  error = function(e) warning("Pre-tournament context refresh failed: ", conditionMessage(e))
)

source("pipeline/31_train_validate_denoise_signals_3_2.R")

Sys.setenv(
  FM_RELEASE_VERSION="3.2-challenger",
  FM_FORCE_REFRESH="true",
  FM_REFRESH_DEFENSE="false"
)
source("runners/RUN_3_1_AUTO_REFRESH.R")
