# ============================================================
# FANTASY MODEL 2.4 - PREREQUISITE CHECK
# ============================================================
source("config.R")
required <- c(
  "output/weekly_2_3_validation_predictions.csv",
  "output/weekly_2_3_2_validation_predictions.csv",
  "output/weekly_2_3_2_validation_metrics.csv"
)
weekly_table <- if (file.exists("data/processed/weekly_model_table_2_3.csv")) "data/processed/weekly_model_table_2_3.csv" else "data/processed/weekly_model_table_2_2.csv"
required <- c(required, weekly_table)
cat("\nFantasy Model 2.4 prerequisite check\n")
cat("====================================\n")
ok <- TRUE
for (p in required) {
  yes <- file.exists(p)
  cat(if (yes) "[OK]   " else "[MISS] ", p, "\n", sep = "")
  ok <- ok && yes
}
if (ok) cat("\n[READY] Reuse the completed 2.3.2 history. No PBP rebuild is required.\n")
else cat("\n[NOT READY] Run source(\"runners/RUN_2_3_2_COMPLETE.R\") first.\n")
invisible(ok)
