# ============================================================
# FANTASY MODEL 2.2 - FULL SEASON-FOUNDATION RUNNER (2.0 CORE)
# Use this for a clean project. Existing users should normally use
# MOBILE_RUN_FAST.R after the 1.2/PBP feature store already exists.
# ============================================================
cat("\n========================================\n")
cat(" FANTASY MODEL 2.2 - FULL SEASON BUILD (2.0 CORE)\n")
cat("========================================\n\n")
source("config.R")
dir.create("logs", showWarnings = FALSE, recursive = TRUE)

run_stage_20 <- function(label, script) {
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
        cat(paste(utils::tail(lines, 140), collapse = "\n"), "\n")
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

run_stage_20("Step 1/12: Downloading/reusing compact NFL data + PBP feature store...", "01_download_data.R")
run_stage_20("Step 2/12: Building validated 1.2 direct-model features...", "02_build_features.R")
run_stage_20("Step 3/12: Building 2.0 opportunity/shrinkage feature store...", "02b_build_opportunity_data.R")
run_stage_20("Step 4/12: Re-running validated 1.2 direct walk-forward baseline...", "04_validate_models.R")
run_stage_20("Step 5/12: Validating 2.0 architecture...", "04b_validate_2_0.R")
run_stage_20("Step 6/12: Training final direct + decision models...", "03_train_models.R")
run_stage_20("Step 7/12: Training final 2.0 opportunity/residual models...", "03b_train_2_0_models.R")
run_stage_20("Step 8/12: Producing 1.2 direct 2026 projections...", "05_project_2026.R")
run_stage_20("Step 9/12: Producing 2.0 final projections...", "05b_project_2_0.R")
run_stage_20("Step 10/12: Building dynasty projections...", "06_dynasty_projection.R")
run_stage_20("Step 11/12: Building league/scarcity rankings...", "07_rank_players.R")
run_stage_20("Step 12/12: Finding historical player comps...", "08_player_comps.R")
cat("\nSeason foundation complete. Next run source(\"MOBILE_RUN_WEEKLY.R\") for the 2.2 weekly engine, then source(\"RUN_APP.R\").\n")
