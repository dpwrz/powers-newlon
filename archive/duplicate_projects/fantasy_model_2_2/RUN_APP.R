# ============================================================
# FANTASY MODEL 2.2 - MOBILE DASHBOARD LAUNCHER
# ============================================================
source("config.R")
cat("\n========================================\n")
cat(" FANTASY MODEL 2.2 - DASHBOARD\n")
cat("========================================\n\n")

required <- c(
  paste0("output/final_", CURRENT_SEASON, "_rankings.csv"),
  "output/final_dynasty_rankings.csv"
)
missing <- required[!file.exists(required)]
if (length(missing) > 0) {
  stop(
    "Projection outputs are missing. Run source(\"MOBILE_RUN_FAST.R\") first. Missing: ",
    paste(missing, collapse = ", ")
  )
}

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny", repos = "https://cloud.r-project.org")
}
cat("Outputs found. Opening Fantasy Model 2.0...\n")
shiny::runApp(".", launch.browser = TRUE)
