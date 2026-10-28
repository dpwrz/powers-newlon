# ============================================================
# FANTASY MODEL 3.2 - OPTIMIZE + GUARDED REFRESH
# ============================================================
source("pipeline/31_train_validate_denoise_signals_3_2.R")

Sys.setenv(
  FM_RELEASE_VERSION="3.2-challenger",
  FM_FORCE_REFRESH="true",
  FM_REFRESH_DEFENSE="true"
)
source("runners/RUN_3_1_AUTO_REFRESH.R")
