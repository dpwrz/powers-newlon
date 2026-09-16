# Resume 2.3 weekly build from validation/training.
source("config.R")
required <- c(paste0("output/", CURRENT_SEASON, "_projections.csv"), "data/raw/weekly_defense_context_raw.csv")
weekly_table <- if (file.exists("data/processed/weekly_model_table_2_3.csv")) "data/processed/weekly_model_table_2_3.csv" else "data/processed/weekly_model_table_2_2.csv"
required <- c(required, weekly_table)
for (p in required) if (!file.exists(p)) stop("Missing Step-3 resume prerequisite: ", p)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
run_stage_23 <- function(label, script) {
  cat("\n", label, "\n", sep = "")
  rscript <- unname(Sys.which("Rscript")); log_path <- file.path("logs", paste0("weekly23_stage_", tools::file_path_sans_ext(basename(script)), ".log"))
  if (nzchar(rscript)) {
    status <- system2(rscript, args = c("maintenance/RUN_STAGE_20.R", script, log_path), stdout = "", stderr = "")
    if (!identical(as.integer(status), 0L)) { if (file.exists(log_path)) cat(paste(utils::tail(readLines(log_path, warn = FALSE), 240), collapse = "\n"), "\n"); stop("2.3 weekly stage failed: ", script, ". Full log: ", log_path) }
  } else sys.source(script, envir = new.env(parent = globalenv()))
  invisible(gc(full = TRUE))
}
run_stage_23("Step 3/4: Walk-forward validating + training Fantasy Model 2.3...", "pipeline/10_train_validate_weekly.R")
run_stage_23(paste0("Step 4/4: Projecting ", CURRENT_SEASON, " weekly matchups + ROS..."), "pipeline/11_project_weekly_2026.R")
cat("\n2.3 weekly build complete. Next: source(\"runners/RUN_APP.R\")\n")
