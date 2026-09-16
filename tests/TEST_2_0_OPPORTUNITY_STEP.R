cat("\n========================================\n")
cat(" FANTASY MODEL 2.0 - HOTFIX 3 TEST\n")
cat("========================================\n\n")
cat("Testing only Step 1: opportunity/shrinkage feature store.\n")
cat("This does NOT retrain the full model.\n\n")

dir.create("logs", showWarnings = FALSE, recursive = TRUE)
rscript <- unname(Sys.which("Rscript"))
log_path <- "logs/test_02b_build_opportunity_data.log"

if (nzchar(rscript)) {
  status <- system2(rscript, args = c("maintenance/RUN_STAGE_20.R", "pipeline/02b_build_opportunity_data.R", log_path), stdout = "", stderr = "")
  if (!identical(as.integer(status), 0L)) {
    cat("\nFAIL: 02b_build_opportunity_data.R did not complete.\n")
    cat("Last log lines:\n\n")
    if (file.exists(log_path)) {
      lines <- readLines(log_path, warn = FALSE)
      cat(paste(utils::tail(lines, 160), collapse = "\n"), "\n")
    }
    stop("Opportunity-step test failed. Full log: ", log_path)
  }
} else {
  source("pipeline/02b_build_opportunity_data.R")
}

cat("\nPASS: Fantasy Model 2.0 opportunity/shrinkage feature store completed.\n")
cat("Next run: source(\"runners/MOBILE_RUN_FAST.R\")\n\n")
