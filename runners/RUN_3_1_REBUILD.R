# ============================================================
# FANTASY MODEL 3.1 - INTENTIONAL FULL PERFORMANCE REBUILD
# ============================================================
# This is the slow path. It is not used for normal live updates.

source("config.R")

if (!file.exists("output/weekly_2_5_validation_predictions.csv") ||
    !file.exists("output/weekly_2_5_champion_manifest.csv")) {
  source("pipeline/22_train_validate_accuracy_tournament_2_5.R")
}
if (!file.exists("output/weekly_2_5_feedback_validation_predictions.csv") ||
    !file.exists("output/weekly_2_5_feedback_promotion.csv")) {
  source("pipeline/25_train_validate_error_feedback_2_5.R")
}

source("pipeline/26_audit_projection_components_3_1.R")
source("pipeline/27_train_validate_accuracy_3_1.R")

# Once the tournament is complete, force one guarded live projection pass.
Sys.setenv(
  FM_RELEASE_VERSION = "3.1",
  FM_FORCE_REFRESH = "true",
  FM_REFRESH_DEFENSE = "true",
  FM_REFRESH_DYNASTY = "true"
)
source("runners/RUN_3_1_AUTO_REFRESH.R")
