# ============================================================
# FANTASY MODEL 3.0 - RESUME AFTER ROLE VALIDATION
# ============================================================
# Use this when Step 18 completed and role model artifacts already exist.
source("config.R")
required <- c("output/role_3_0_model_manifest.csv", "output/role_3_0_validation_metrics.csv")
if (any(!file.exists(required))) {
  stop("Role validation artifacts are missing. Run source(\"runners/RUN_3_0_DATA_LAB.R\") instead.")
}
source("pipeline/19_project_role_context_2026_3_0.R")
invisible(gc(full = TRUE))
source("pipeline/21_audit_decision_relevant_error_3_0.R")
invisible(gc(full = TRUE))
source("pipeline/20_build_dynasty_intelligence_3_0.R")
invisible(gc(full = TRUE))
cat("\n[3.0] Resume complete. Launch with source(\"runners/RUN_DYNASTY_APP.R\").\n")
