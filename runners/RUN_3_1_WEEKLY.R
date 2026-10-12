# ============================================================
# FANTASY MODEL 3.1 - WEEKLY LEARN / RETRAIN / REPUBLISH
# ============================================================
# Run only after the NFL week is fully complete and official weekly data are
# available. Historical training data is verified byte-for-byte unchanged.

source("config.R")

hist_path <- "data/processed/weekly_model_table_2_3.csv"
if (!file.exists(hist_path)) stop("Missing immutable historical weekly store: ", hist_path)
hist_md5_before <- unname(tools::md5sum(hist_path))

# Bootstrap the historically validated 3.1 challenger exactly once if production
# has never built it. This step is historical-only: pipeline/27 is hard-gated to
# seasons <= TRAIN_END and explicitly excludes 2026 outcomes from fitting/tuning.
required31 <- c(
  "output/weekly_3_1_promotion.csv",
  "output/weekly_3_1_champion_manifest.csv",
  "output/weekly_3_1_residual_pool.csv",
  paste0("models/weekly_3_1_direct_", POSITIONS, ".json"),
  paste0("models/weekly_3_1_direct_", POSITIONS, "_meta.rds")
)
if (any(!file.exists(required31))) {
  cat("\n[3.1 WEEKLY] Historical 3.1 artifacts absent; bootstrapping from 2016-",
      TRAIN_END, " only.\n", sep = "")
  source("pipeline/26_audit_projection_components_3_1.R", local = FALSE)
  source("pipeline/27_train_validate_accuracy_3_1.R", local = FALSE)
}

Sys.setenv(
  FM_RELEASE_VERSION = "3.1",
  FM_FORCE_REFRESH = "true",
  FM_REFRESH_DEFENSE = "true",
  FM_REFRESH_DYNASTY = "true"
)

cat("\n[3.1 WEEKLY] Step 1/4 - finalize latest production ledger and accuracy.\n")
source("runners/RUN_3_1_AUTO_REFRESH.R", local = FALSE)

cat("\n[3.1 WEEKLY] Step 2/4 - snapshot honest current-season training rows.\n")
source("pipeline/30_update_online_training_3_1.R", local = FALSE)

cat("\n[3.1 WEEKLY] Step 3/4 - retrain guarded online challengers.\n")
source("pipeline/31_train_weekly_online_3_1.R", local = FALSE)

cat("\n[3.1 WEEKLY] Step 4/4 - force future projections through newly trained gates.\n")
Sys.setenv(FM_FORCE_REFRESH = "true")
source("runners/RUN_3_1_AUTO_REFRESH.R", local = FALSE)

hist_md5_after <- unname(tools::md5sum(hist_path))
if (!identical(hist_md5_before, hist_md5_after)) {
  stop("[3.1 WEEKLY] Historical training data changed. Weekly retrain rejected.")
}

cat("\n[3.1 WEEKLY] Complete. Historical training store preserved: ",
    hist_md5_after, "\n", sep = "")
