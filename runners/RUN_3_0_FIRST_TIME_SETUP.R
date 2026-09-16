# ============================================================
# FANTASY MODEL 3.0 - FIRST-TIME PRODUCTION SETUP
# ============================================================
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists("config.R")) stop("Run from the Fantasy Model project root or source RUN_FIRST_TIME_SETUP.R.")
source("config.R")
cat("\n========================================\n")
cat(" FANTASY MODEL 3.0 - FIRST TIME SETUP\n")
cat("========================================\n")
cat("[3.0] Project root: ", root, "\n", sep = "")

source("tests/TEST_2_5_PREREQS.R")

accuracy_required <- c(
  "output/weekly_2_5_champion_manifest.csv",
  "output/weekly_2_5_promotion.csv",
  "output/weekly_2_5_residual_pool.csv",
  paste0("models/weekly_2_5_direct_", POSITIONS, ".json"),
  paste0("models/weekly_2_5_direct_", POSITIONS, "_meta.rds")
)
if (any(!file.exists(accuracy_required))) {
  cat("[3.0] Production accuracy artifacts are missing; running the one-time honest 2.5 accuracy tournament...\n")
  source("runners/RUN_2_5_ACCURACY_TOURNAMENT.R", local = FALSE)
} else {
  cat("[3.0] Existing validated accuracy artifacts found; reusing them.\n")
}

feedback_required <- paste0("models/weekly_2_5_error_feedback_", POSITIONS, ".rds")
if (any(!file.exists(feedback_required))) {
  cat("[3.0] Running the one-time chronological final-error feedback tournament...\n")
  source("runners/RUN_2_5_FEEDBACK_TOURNAMENT.R", local = FALSE)
} else {
  cat("[3.0] Existing error-feedback models found; reusing them.\n")
}

source("tests/TEST_2_5_FORWARD_ONLY.R")
source("tests/TEST_2_5_ERROR_FEEDBACK.R")
source("tests/TEST_2_5_FROZEN_GAMEDAY_BIND.R")
source("tests/TEST_CURRENT_STATS_OPPONENT_JOIN.R")
source("tests/TEST_2_5_LIVE_SCORING.R")
source("tests/TEST_2_5_LIVE_AUTOMATION.R")

Sys.setenv(FM_FORCE_REFRESH = "true", FM_RELEASE_VERSION = "3.0.0")
cat("[3.0] Running first forward-only live refresh and Web 1.0 snapshot export...\n")
source("runners/RUN_2_5_AUTO_REFRESH.R", local = FALSE)

cat("\n[3.0] FIRST-TIME SETUP COMPLETE.\n")
cat("[3.0] Normal refresh command: source(\"RUN_AUTO_REFRESH.R\")\n")
cat("[3.0] Web local build: cd web && npm install && npm run build\n")
