# ============================================================
# FANTASY MODEL 2.3.2 - FAST UPGRADE FROM COMPLETED 2.3
# ============================================================
source("config.R")
required <- c("output/weekly_2_3_validation_predictions.csv", "output/weekly_2_3_validation_metrics.csv")
weekly_table <- if (file.exists("data/processed/weekly_model_table_2_3.csv")) "data/processed/weekly_model_table_2_3.csv" else "data/processed/weekly_model_table_2_2.csv"
required <- c(required, weekly_table)
for (p in required) if (!file.exists(p)) stop("Missing 2.3.2 upgrade prerequisite: ", p)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
run_stage_232 <- function(label, script) {
  cat("\n", label, "\n", sep = "")
  rscript <- unname(Sys.which("Rscript"))
  log_path <- file.path("logs", paste0("weekly232_stage_", tools::file_path_sans_ext(basename(script)), ".log"))
  if (nzchar(rscript)) {
    status <- system2(rscript, args = c("maintenance/RUN_STAGE_20.R", script, log_path), stdout = "", stderr = "")
    if (!identical(as.integer(status), 0L)) {
      if (file.exists(log_path)) cat(paste(utils::tail(readLines(log_path, warn = FALSE), 260), collapse = "\n"), "\n")
      stop("2.3.2 stage failed: ", script, ". Full log: ", log_path)
    }
  } else sys.source(script, envir = new.env(parent = globalenv()))
  invisible(gc(full = TRUE))
}
run_stage_232("2.3.2 Step A: Signal discovery + honest historical error-feedback validation...", "pipeline/12_train_validate_signal_feedback_2_3_2.R")
run_stage_232(paste0("2.3.2 Step B: Refreshing ", CURRENT_SEASON, " and applying earned controllers..."), "pipeline/13_project_weekly_2026_2_3_2.R")
cat("\nFantasy Model 2.3.2 complete. Launch with source(\"runners/RUN_APP.R\").\n")
