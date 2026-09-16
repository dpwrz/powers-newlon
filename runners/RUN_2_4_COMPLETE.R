# ============================================================
# FANTASY MODEL 2.4 - COMPLETE BUILD HELPER
# ============================================================
source("config.R")
if (!file.exists("output/weekly_2_3_2_validation_predictions.csv")) {
  cat("[2.4 COMPLETE] Locked 2.3.2 OOF benchmark missing; building it first.\n")
  source("runners/RUN_2_3_2_COMPLETE.R")
} else {
  cat("[2.4 COMPLETE] Existing 2.3.2 honest OOF history found; reusing it.\n")
}
source("runners/RESUME_2_4_FROM_SIGNAL_AUDIT.R")
