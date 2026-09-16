# ============================================================
# FANTASY MODEL 2.2 - FAST WEEKLY RUN (REUSE PBP CHECKPOINT)
# ============================================================
cat("\n========================================\n")
cat(" FANTASY MODEL 2.2 - FAST WEEKLY RUN\n")
cat(" Reuses compact PBP defense checkpoint\n")
cat("========================================\n\n")
source("config.R")
ensure_packages(c("readr"))

season_projection <- paste0("output/", CURRENT_SEASON, "_projections.csv")
for (p in c(season_projection, "data/processed/model_table.csv", "data/raw/weekly_defense_context_raw.csv")) {
  if (!file.exists(p)) stop("Missing fast-run prerequisite: ", p, ". Use source(\"MOBILE_RUN_WEEKLY.R\") for a clean build.")
}

dir.create("logs", recursive = TRUE, showWarnings = FALSE)
run_stage_22_fast <- function(label, script) {
  cat("\n", label, "\n", sep = "")
  rscript <- unname(Sys.which("Rscript"))
  safe_name <- gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(script)))
  log_path <- file.path("logs", paste0("weekly22_stage_", safe_name, ".log"))
  if (nzchar(rscript)) {
    status <- system2(rscript, args = c("RUN_STAGE_20.R", script, log_path), stdout = "", stderr = "")
    if (!identical(as.integer(status), 0L)) {
      cat("\n[2.2 WEEKLY ERROR] ", script, " failed. Last log lines:\n", sep = "")
      if (file.exists(log_path)) {
        lines <- readLines(log_path, warn = FALSE)
        cat(paste(utils::tail(lines, 180), collapse = "\n"), "\n")
      }
      stop("2.2 weekly stage failed: ", script, ". Full log: ", log_path)
    }
  } else {
    stage_env <- new.env(parent = globalenv())
    sys.source(script, envir = stage_env)
    rm(stage_env); invisible(gc(full = TRUE))
  }
  invisible(gc(full = TRUE))
}

cat("PBP context: REUSED (no historical PBP rebuild)\n")
cat("Leakage rule: every historical player-week uses pre-kickoff state only\n")
run_stage_22_fast("Step 1/3: Building 2.2 player-week role/NGS/matchup feature store...", "09_build_weekly_data.R")
run_stage_22_fast("Step 2/3: Validating + training 2.2 neutral/opportunity/matchup/direct stack...", "10_train_validate_weekly.R")
run_stage_22_fast(paste0("Step 3/3: Projecting ", CURRENT_SEASON, " weekly distributions + ROS..."), "11_project_weekly_2026.R")
cat("\n========================================\n")
cat(" FANTASY MODEL 2.2 FAST RUN COMPLETE\n")
cat("========================================\n")
cat("Quality: output/weekly_2_2_model_quality_report.txt\n")
cat("Ablation: output/weekly_2_2_architecture_comparison.csv\n")
cat("Starter accuracy: output/weekly_2_2_cohort_metrics.csv\n")
cat("Matchup calibration: output/weekly_2_2_matchup_calibration.csv\n")
cat("Production: output/weekly_", CURRENT_SEASON, "_projections.csv\n", sep = "")
cat("Next: source(\"RUN_APP.R\")\n")
