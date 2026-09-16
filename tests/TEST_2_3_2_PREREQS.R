source("config.R")
required <- c(
  "output/weekly_2_3_validation_predictions.csv",
  "output/weekly_2_3_validation_metrics.csv",
  "data/processed/weekly_model_table_2_3.csv",
  paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
)
cat("FANTASY MODEL 2.3.2 PREREQUISITES\n")
for (p in required) cat(ifelse(file.exists(p), "[OK]   ", "[MISS] "), p, "\n", sep = "")
if (!all(file.exists(required))) stop("2.3.2 prerequisites are incomplete. Keep the existing project and finish 2.3 first.")
cat("\nReady. Run: source(\"runners/RESUME_2_3_2_FROM_SIGNAL.R\")\n")
