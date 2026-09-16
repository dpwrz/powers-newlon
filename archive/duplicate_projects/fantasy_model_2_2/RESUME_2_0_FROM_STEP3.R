# ============================================================
# FANTASY MODEL 2.0 - HOTFIX 5 RESUME FROM STEP 3
# Assumes Step 1 opportunity store and Step 2 direct OOF validation
# already completed successfully.
# ============================================================
cat("\n========================================\n")
cat(" FANTASY MODEL 2.0 - HOTFIX 5 RESUME\n")
cat(" Starting at Step 3/10\n")
cat("========================================\n\n")
source("config.R")
ensure_packages(c("readr"))

required <- c(
  "data/processed/opportunity_model_table_2_0.csv",
  "output/validation_predictions.csv"
)
missing <- required[!file.exists(required)]
if (length(missing) > 0) {
  stop("Cannot resume from Step 3. Missing: ", paste(missing, collapse = ", "),
       ". Run source(\"MOBILE_RUN_FAST.R\") again instead.")
}

cat("[HOTFIX 5] Step 1 opportunity store found.\n")
cat("[HOTFIX 5] Step 2 direct OOF predictions found.\n")
cat("[HOTFIX 5] No need to rebuild either one.\n\n")

dir.create("logs", showWarnings = FALSE, recursive = TRUE)
run_stage_resume20 <- function(label, script) {
  cat("\n", label, "\n", sep = "")
  rscript <- unname(Sys.which("Rscript"))
  safe_name <- gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(script)))
  log_path <- file.path("logs", paste0("stage_", safe_name, ".log"))
  if (nzchar(rscript)) {
    status <- system2(rscript, args = c("RUN_STAGE_20.R", script, log_path), stdout = "", stderr = "")
    if (!identical(as.integer(status), 0L)) {
      cat("\n[ERROR] ", script, " failed. Last log lines:\n", sep = "")
      if (file.exists(log_path)) {
        lines <- readLines(log_path, warn = FALSE)
        cat(paste(utils::tail(lines, 160), collapse = "\n"), "\n")
      }
      stop("Stage failed: ", script, ". Full log: ", log_path)
    }
  } else {
    stage_env <- new.env(parent = globalenv())
    sys.source(script, envir = stage_env)
    rm(stage_env); invisible(gc(full = TRUE))
  }
  invisible(gc(full = TRUE))
}

run_stage_resume20("Step 3/10: Validating 2.0 opportunity + shrinkage + residual architecture...", "04b_validate_2_0.R")
run_stage_resume20("Step 4/10: Training final 1.2 direct + fantasy decision models...", "03_train_models.R")
run_stage_resume20("Step 5/10: Training final 2.0 opportunity/residual models...", "03b_train_2_0_models.R")
run_stage_resume20("Step 6/10: Producing validated 1.2 direct 2026 projections...", "05_project_2026.R")
run_stage_resume20("Step 7/10: Producing 2.0 structured stat lines + final guardrail projections...", "05b_project_2_0.R")
run_stage_resume20("Step 8/10: Building dynasty projections...", "06_dynasty_projection.R")
run_stage_resume20("Step 9/10: Building league/scarcity rankings...", "07_rank_players.R")
run_stage_resume20("Step 10/10: Finding historical player comps...", "08_player_comps.R")

cat("\n========================================\n")
cat(" FANTASY MODEL 2.0 RUN COMPLETE\n")
cat("========================================\n\n")
cat("Important evaluation files:\n")
cat(" - output/model_quality_report.txt\n")
cat(" - output/validation_2_0_model_comparison.csv\n")
cat(" - output/selected_2_0_architecture_weights.csv\n")
cat(" - output/opportunity_signal_correlations.csv\n")
cat(" - output/shrinkage_signal_metrics.csv\n\n")
if (file.exists("output/model_quality_report.txt")) {
  cat(paste(readLines("output/model_quality_report.txt", warn = FALSE), collapse = "\n"), "\n\n")
}
cat("Next: source(\"RUN_APP.R\")\n")
