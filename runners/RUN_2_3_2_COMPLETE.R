# ============================================================
# FANTASY MODEL 2.3.2 - COMPLETE HELPER
# ============================================================
source("config.R")
if (!file.exists("output/weekly_2_3_validation_predictions.csv")) {
  cat("[2.3.2 COMPLETE] 2.3 OOF store missing; building/validating 2.3 first.\n")
  source("runners/RUN_2_3_COMPLETE.R")
} else {
  cat("[2.3.2 COMPLETE] Existing 2.3 honest OOF store found; reusing it.\n")
}
source("runners/RESUME_2_3_2_FROM_SIGNAL.R")
