# ============================================================
# FANTASY MODEL 2.4 - FAST UPGRADE FROM COMPLETED 2.3.2
# ============================================================
source("config.R")
required <- c("output/weekly_2_3_validation_predictions.csv",
              "output/weekly_2_3_2_validation_predictions.csv")
weekly_table <- if (file.exists("data/processed/weekly_model_table_2_3.csv")) "data/processed/weekly_model_table_2_3.csv" else "data/processed/weekly_model_table_2_2.csv"
required <- c(required, weekly_table)
for (p in required) if (!file.exists(p)) stop("Missing 2.4 upgrade prerequisite: ", p, ". Run source(\"runners/RUN_2_3_2_COMPLETE.R\") first.")

dir.create("logs", recursive = TRUE, showWarnings = FALSE)
run_stage_24 <- function(label, script) {
  cat("\n", label, "\n", sep = "")
  rscript <- unname(Sys.which("Rscript"))
  log_path <- file.path("logs", paste0("weekly24_stage_", tools::file_path_sans_ext(basename(script)), ".log"))
  if (nzchar(rscript)) {
    status <- system2(rscript, args = c("maintenance/RUN_STAGE_20.R", script, log_path), stdout = "", stderr = "")
    if (!identical(as.integer(status), 0L)) {
      if (file.exists(log_path)) cat(paste(utils::tail(readLines(log_path, warn = FALSE), 320), collapse = "\n"), "\n")
      stop("2.4 stage failed: ", script, ". Full log: ", log_path)
    }
  } else sys.source(script, envir = new.env(parent = globalenv()))
  invisible(gc(full = TRUE))
}

run_stage_24("2.4 Step 1/3: College + combine talent prior, defensive style and xFP context...", "pipeline/14_build_talent_context_2_4.R")
run_stage_24("2.4 Step 2/3: Signal attribution + hierarchical player-response walk-forward validation...", "pipeline/15_train_validate_signal_model_2_4.R")
run_stage_24(paste0("2.4 Step 3/3: Refreshing ", CURRENT_SEASON, " with strict promotion gates..."), "pipeline/16_project_weekly_2026_2_4.R")
cat("\nFantasy Model 2.4 complete. Launch with source(\"runners/RUN_APP.R\").\n")
