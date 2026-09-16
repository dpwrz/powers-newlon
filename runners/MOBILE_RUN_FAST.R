# ============================================================
# FANTASY MODEL 2.3 - SEASON ENGINE FAST RUNNER (2.0 CORE)
# Reuses validated 1.2 feature tables + compact PBP feature store.
# Every heavy stage runs in its own R process.
# ============================================================
cat("\n========================================\n")
cat(" FANTASY MODEL 2.3 - SEASON FOUNDATION (2.0 CORE)\n")
cat(" Validated 2.0 season prior for 2.2 weekly engine\n")
cat("========================================\n\n")
source("config.R")
ensure_packages(c("readr"))
cat("Project version: ", PROJECT_VERSION, "\n", sep = "")
cat("Season architecture: validated 2.0 opportunity/direct hybrid; weekly matchup engine runs separately via MOBILE_RUN_WEEKLY.R\n")
cat("PBP: reused from compact feature store; raw multi-season PBP is not loaded here\n")
cat("Memory: each stage runs in a fresh R process\n\n")

required_features <- c(
  "data/processed/model_table.csv",
  paste0("data/processed/projection_table_", CURRENT_SEASON, ".csv")
)
required_context <- file.path("data/raw", c("team_context.csv", "qb_context.csv", "receiver_context.csv", "team_qb_context.csv"))
required_raw <- file.path("data/raw", c("player_stats.csv", "team_stats.csv"))
if (!all(file.exists(required_features))) stop("Validated 1.2 feature store missing. Run the working 1.2/full 2.0 pipeline once.")
if (!all(file.exists(required_context))) stop("Compact PBP context store missing. Run source(\"pipeline/BUILD_PBP_CONTEXT.R\") first.")
if (!all(file.exists(required_raw))) stop("Base player/team stats missing. Run source(\"pipeline/01_download_data.R\") once.")

# Validate headers before trusting cached tables.
model_header <- readr::read_csv(required_features[1], n_max = 0, show_col_types = FALSE)
proj_header <- readr::read_csv(required_features[2], n_max = 0, show_col_types = FALSE)
if (!all(c("player_id", "position", "season", "target_fppg") %in% names(model_header))) stop("model_table.csv is not a valid 1.2 feature store.")
if (!all(c("player_id", "position", "current_team") %in% names(proj_header))) stop("Current projection table is not a valid 1.2 feature store.")
rm(model_header, proj_header); gc()

cat("[FAST] Existing direct feature store found. It will be schema-checked before 2.0 uses it.\n")
cat("[FAST] PBP-derived context found; no raw PBP reload.\n\n")

dir.create("logs", showWarnings = FALSE, recursive = TRUE)
run_stage_20 <- function(label, script) {
  cat("\n", label, "\n", sep = "")
  rscript <- unname(Sys.which("Rscript"))
  safe_name <- gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(script)))
  log_path <- file.path("logs", paste0("stage_", safe_name, ".log"))
  if (nzchar(rscript)) {
    status <- system2(rscript, args = c("maintenance/RUN_STAGE_20.R", script, log_path), stdout = "", stderr = "")
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

# ------------------------------------------------------------
# Surgical QB interception/scoring repair.
# Do NOT rebuild the working 1.2 feature-engineering pipeline.
# ------------------------------------------------------------
run_stage_20(
  "Preflight: repairing/verifying QB interception scoring without rebuilding 1.2 features...",
  "maintenance/REPAIR_QB_SCORING_2_0.R"
)

# Remove only 2.0 model artifacts; keep the direct models until they are retrained.
old20 <- list.files("models", pattern = "^(opportunity_models_2_0_|residual_model_2_0_|team_pass_volume_model_2_0|team_carry_volume_model_2_0).*\\.rds$", full.names = TRUE)
if (length(old20) > 0) unlink(old20)

run_stage_20("Step 1/10: Building 2.0 opportunity/shrinkage feature store...", "pipeline/02b_build_opportunity_data.R")
run_stage_20("Step 2/10: Re-running validated 1.2 direct walk-forward baseline...", "pipeline/04_validate_models.R")
run_stage_20("Step 3/10: Validating 2.0 opportunity + shrinkage + residual architecture...", "pipeline/04b_validate_2_0.R")
run_stage_20("Step 4/10: Training final 1.2 direct + fantasy decision models...", "pipeline/03_train_models.R")
run_stage_20("Step 5/10: Training final 2.0 opportunity/residual models...", "pipeline/03b_train_2_0_models.R")
run_stage_20("Step 6/10: Producing validated 1.2 direct 2026 projections...", "pipeline/05_project_2026.R")
run_stage_20("Step 7/10: Producing 2.0 structured stat lines + final guardrail projections...", "pipeline/05b_project_2_0.R")
run_stage_20("Step 8/10: Building dynasty projections...", "pipeline/06_dynasty_projection.R")
run_stage_20("Step 9/10: Building league/scarcity rankings...", "pipeline/07_rank_players.R")
run_stage_20("Step 10/10: Finding historical player comps...", "pipeline/08_player_comps.R")

cat("\n========================================\n")
cat(" FANTASY MODEL 2.3 SEASON FOUNDATION COMPLETE\n")
cat("========================================\n\n")
cat("Most important evaluation files:\n")
cat(" - output/model_quality_report.txt\n")
cat(" - output/validation_metrics_2_0.csv\n")
cat(" - output/validation_2_0_model_comparison.csv\n")
cat(" - output/selected_2_0_architecture_weights.csv\n")
cat(" - output/opportunity_signal_correlations.csv\n")
cat(" - output/shrinkage_signal_metrics.csv\n\n")
cat("Production outputs:\n")
cat(" - output/", CURRENT_SEASON, "_projections.csv\n", sep = "")
cat(" - output/final_", CURRENT_SEASON, "_rankings.csv\n", sep = "")
cat(" - output/final_dynasty_rankings.csv\n\n")
if (file.exists("output/model_quality_report.txt")) {
  cat(paste(readLines("output/model_quality_report.txt", warn = FALSE), collapse = "\n"), "\n\n")
}
cat("Next: source(\"runners/RUN_APP.R\")\n")
