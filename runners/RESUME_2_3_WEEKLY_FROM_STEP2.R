# Resume 2.3 weekly build from historical feature-store construction.
source("config.R")
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
run_stage_23 <- function(label, script) {
  cat("\n", label, "\n", sep = "")
  rscript <- unname(Sys.which("Rscript")); log_path <- file.path("logs", paste0("weekly23_stage_", tools::file_path_sans_ext(basename(script)), ".log"))
  if (nzchar(rscript)) {
    status <- system2(rscript, args = c("maintenance/RUN_STAGE_20.R", script, log_path), stdout = "", stderr = "")
    if (!identical(as.integer(status), 0L)) { if (file.exists(log_path)) cat(paste(utils::tail(readLines(log_path, warn = FALSE), 220), collapse = "\n"), "\n"); stop("2.3 weekly stage failed: ", script, ". Full log: ", log_path) }
  } else sys.source(script, envir = new.env(parent = globalenv()))
  invisible(gc(full = TRUE))
}
run_stage_23("Step 2/4: Building strict pre-kickoff historical player-week features...", "pipeline/09_build_weekly_data.R")
run_stage_23("Step 3/4: Walk-forward validating + training Fantasy Model 2.3...", "pipeline/10_train_validate_weekly.R")
run_stage_23(paste0("Step 4/4: Projecting ", CURRENT_SEASON, " weekly matchups + ROS..."), "pipeline/11_project_weekly_2026.R")
cat("\n2.3 weekly build complete. Next: source(\"runners/RUN_APP.R\")\n")
