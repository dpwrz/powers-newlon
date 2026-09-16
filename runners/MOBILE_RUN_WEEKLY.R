# ============================================================
# FANTASY MODEL 2.3 - POSIT-SAFE WEEKLY PIPELINE
# ============================================================
cat("\n========================================\n")
cat(" FANTASY MODEL 2.3 - WEEKLY RUN\n")
cat(" Pre-kickoff matchup projections + ROS\n")
cat("========================================\n\n")
source("config.R")
ensure_packages(c("readr"))

season_projection <- paste0("output/", CURRENT_SEASON, "_projections.csv")
if (!file.exists(season_projection)) {
  stop("Season Model 2.0/2.1 projection is missing. Run the season pipeline first so the weekly engine has its preseason prior: ", season_projection)
}
if (!file.exists("data/processed/model_table.csv")) stop("Validated season feature store missing.")

dir.create("logs", recursive = TRUE, showWarnings = FALSE)
run_stage_22 <- function(label, script) {
  cat("\n", label, "\n", sep = "")
  rscript <- unname(Sys.which("Rscript"))
  safe_name <- gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(script)))
  log_path <- file.path("logs", paste0("weekly_stage_", safe_name, ".log"))
  if (nzchar(rscript)) {
    status <- system2(rscript, args = c("maintenance/RUN_STAGE_20.R", script, log_path), stdout = "", stderr = "")
    if (!identical(as.integer(status), 0L)) {
      cat("\n[WEEKLY ERROR] ", script, " failed. Last log lines:\n", sep = "")
      if (file.exists(log_path)) {
        lines <- readLines(log_path, warn = FALSE)
        cat(paste(utils::tail(lines, 160), collapse = "\n"), "\n")
      }
      stop("Weekly stage failed: ", script, ". Full log: ", log_path)
    }
  } else {
    stage_env <- new.env(parent = globalenv())
    sys.source(script, envir = stage_env)
    rm(stage_env); invisible(gc(full = TRUE))
  }
  invisible(gc(full = TRUE))
}

cat("Project version: ", PROJECT_VERSION, "\n", sep = "")
cat("Weekly philosophy: 2.0 prior + 2.1/2.2 challengers + calibrated matchup + phase-aware historical OOF meta-stack + version guardrail\n")
cat("Leakage rule: player-week features use only information available before kickoff\n")
cat("Memory: each stage runs in a fresh R process\n\n")

run_stage_22("Step 1/4: Building/refreshing resumable weekly PBP defense context...", "pipeline/BUILD_WEEKLY_DEFENSE_CONTEXT.R")
run_stage_22("Step 2/4: Building strict pre-kickoff historical player-week features...", "pipeline/09_build_weekly_data.R")
run_stage_22("Step 3/4: Walk-forward validating historical meta-calibration + training 2.3 weekly models...", "pipeline/10_train_validate_weekly.R")
run_stage_22(paste0("Step 4/4: Projecting every ", CURRENT_SEASON, " week + rest of season..."), "pipeline/11_project_weekly_2026.R")

cat("\n========================================\n")
cat(" FANTASY MODEL 2.3 WEEKLY RUN COMPLETE\n")
cat("========================================\n\n")
cat("Validation:\n")
cat(" - output/weekly_model_quality_report.txt\n")
cat(" - output/weekly_validation_metrics.csv\n")
cat(" - output/weekly_2_3_validation_by_year.csv\n")
cat(" - output/weekly_2_3_matchup_calibration.csv\n")
cat(" - output/weekly_2_3_version_comparison.csv\n")
cat(" - output/weekly_2_3_cohort_metrics.csv\n")
cat(" - output/weekly_2_3_selected_meta_weights.csv\n")
cat(" - output/weekly_2_3_selected_version_weights.csv\n")
cat(" - output/weekly_2_3_confidence_calibration.csv\n")
cat(" - output/weekly_2_3_feature_importance.csv\n\n")
cat("Production:\n")
cat(" - output/weekly_", CURRENT_SEASON, "_projections.csv\n", sep = "")
cat(" - output/rest_of_season_", CURRENT_SEASON, ".csv\n", sep = "")
if (file.exists("output/current_week.txt")) {
  wk <- readLines("output/current_week.txt", warn = FALSE)[1]
  cat(" - output/week_", wk, "_rankings.csv\n", sep = "")
}
cat("\nNext: source(\"runners/RUN_APP.R\")\n")
