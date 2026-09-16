# Resume 2.3 weekly build from live projection only.
source("config.R")
required <- c(
  paste0("output/", CURRENT_SEASON, "_projections.csv"), "data/raw/weekly_defense_context_raw.csv",
  "output/weekly_2_3_validation_metrics.csv", "output/weekly_2_3_selected_meta_weights.csv",
  "output/weekly_2_3_selected_version_weights.csv", "output/weekly_2_3_matchup_calibration_parameters.csv"
)
for (p in required) if (!file.exists(p)) stop("Missing Step-4 resume prerequisite: ", p)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
rscript <- unname(Sys.which("Rscript")); log_path <- "logs/weekly23_stage_11_project_weekly_2026.log"
cat("Step 4/4: Projecting ", CURRENT_SEASON, " weekly matchups + ROS...\n", sep = "")
if (nzchar(rscript)) {
  status <- system2(rscript, args = c("maintenance/RUN_STAGE_20.R", "pipeline/11_project_weekly_2026.R", log_path), stdout = "", stderr = "")
  if (!identical(as.integer(status), 0L)) { if (file.exists(log_path)) cat(paste(utils::tail(readLines(log_path, warn = FALSE), 260), collapse = "\n"), "\n"); stop("2.3 projection stage failed. Full log: ", log_path) }
} else sys.source("pipeline/11_project_weekly_2026.R", envir = new.env(parent = globalenv()))
cat("\n2.3 projection complete. Next: source(\"runners/RUN_APP.R\")\n")
