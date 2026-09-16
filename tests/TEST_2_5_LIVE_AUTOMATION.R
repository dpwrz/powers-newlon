# Model 2.5 live/automation prerequisite audit.
source("config.R")
required <- c(
  "R/live_refresh_engine_25.R",
  "R/league_scoring_engine_25.R",
  "R/live_game_state_engine_25.R",
  "pipeline/13_project_weekly_2026_2_3_2.R",
  "pipeline/23_project_weekly_2026_2_5.R",
  "pipeline/24_score_live_accuracy_2_5.R",
  "output/weekly_2_5_champion_manifest.csv",
  "output/weekly_2_5_residual_pool.csv",
  "data/processed/weekly_model_table_2_3.csv",
  "data/processed/model_table.csv",
  "data/raw/weekly_defense_context_raw.csv",
  paste0("models/weekly_2_5_direct_", POSITIONS, ".json"),
  paste0("models/weekly_2_5_direct_", POSITIONS, "_meta.rds")
)
missing <- required[!file.exists(required)]
if (length(missing)) stop("Live automation prerequisites missing:\n - ", paste(missing, collapse = "\n - "))
cat("[2.5 LIVE TEST] Runtime prerequisites OK.\n")
if (dir.exists("web")) {
  web_req <- c("web/scripts/export_model_snapshot.R", "web/public/data/model_snapshot.json")
  wm <- web_req[!file.exists(web_req)]
  if (length(wm)) warning("Website integration files missing: ", paste(wm, collapse = ", ")) else cat("[2.5 LIVE TEST] Website bridge OK.\n")
}
