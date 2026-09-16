# ============================================================
# FANTASY MODEL 2.3 - PREREQUISITE CHECK
# ============================================================
source("config.R")
cat("========================================\n")
cat(" FANTASY MODEL 2.3 PREREQUISITES\n")
cat("========================================\n")
checks <- c(
  season_projection = paste0("output/", CURRENT_SEASON, "_projections.csv"),
  season_features = "data/processed/model_table.csv",
  weekly_defense_context = "data/raw/weekly_defense_context_raw.csv"
)
for (nm in names(checks)) cat(sprintf("%-28s %s\n", nm, if (file.exists(checks[[nm]])) "PASS" else "MISSING"))
weekly_table <- if (file.exists("data/processed/weekly_model_table_2_3.csv")) "data/processed/weekly_model_table_2_3.csv" else if (file.exists("data/processed/weekly_model_table_2_2.csv")) "data/processed/weekly_model_table_2_2.csv" else NA_character_
cat(sprintf("%-28s %s\n", "weekly_feature_store", if (!is.na(weekly_table)) paste0("PASS (", weekly_table, ")") else "NOT BUILT YET"))
season_ok <- file.exists(checks[["season_projection"]]) && file.exists(checks[["season_features"]])
if (!season_ok) {
  cat("\nSeason foundation is missing. Run: source(\"runners/RUN_2_3_COMPLETE.R\")\n")
} else if (!file.exists(checks[["weekly_defense_context"]])) {
  cat("\nSeason foundation is ready. Run full weekly build: source(\"runners/MOBILE_RUN_WEEKLY.R\")\n")
} else {
  cat("\nCore prerequisites PASS. Run: source(\"runners/MOBILE_RUN_WEEKLY_FAST.R\")\n")
}
