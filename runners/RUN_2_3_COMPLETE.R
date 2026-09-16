# ============================================================
# FANTASY MODEL 2.3 - COMPLETE BUILD HELPER
# ============================================================
source("config.R")
season_projection <- paste0("output/", CURRENT_SEASON, "_projections.csv")
if (!file.exists(season_projection) || !file.exists("data/processed/model_table.csv")) {
  cat("[2.3 COMPLETE] Season foundation missing; running full 2.0 season build first.\n")
  source("runners/MOBILE_RUN.R")
} else {
  cat("[2.3 COMPLETE] Existing 2.0 season foundation found; reusing it.\n")
}
if (file.exists("data/raw/weekly_defense_context_raw.csv")) {
  cat("[2.3 COMPLETE] Weekly defense checkpoint found; using fast weekly runner.\n")
  source("runners/MOBILE_RUN_WEEKLY_FAST.R")
} else {
  cat("[2.3 COMPLETE] Weekly defense checkpoint missing; using full weekly runner.\n")
  source("runners/MOBILE_RUN_WEEKLY.R")
}
cat("\nFantasy Model 2.3 build complete. Launch with source(\"runners/RUN_APP.R\").\n")
