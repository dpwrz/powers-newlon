# ============================================================
# FANTASY MODEL 2.0 - FAST-RUN PREREQUISITE CHECK
# ============================================================
cat("\n========================================\n")
cat(" FANTASY MODEL 2.0 PREREQUISITE CHECK\n")
cat("========================================\n\n")
source("config.R")
ensure_packages(c("readr"))

checks <- list(
  "Validated 1.2 model feature store" = "data/processed/model_table.csv",
  "2026 projection feature store" = paste0("data/processed/projection_table_", CURRENT_SEASON, ".csv"),
  "Player season stats" = "data/raw/player_stats.csv",
  "Team season stats" = "data/raw/team_stats.csv",
  "PBP team context" = "data/raw/team_context.csv",
  "PBP QB context" = "data/raw/qb_context.csv",
  "PBP receiver context" = "data/raw/receiver_context.csv",
  "PBP team/QB context" = "data/raw/team_qb_context.csv"
)

all_ok <- TRUE
for (nm in names(checks)) {
  ok <- file.exists(checks[[nm]])
  cat(if (ok) "PASS" else "FAIL", " - ", nm, "\n", sep = "")
  all_ok <- all_ok && ok
}

if (file.exists(checks[[1]])) {
  h <- readr::read_csv(checks[[1]], n_max = 0, show_col_types = FALSE, progress = FALSE)
  needed <- c("player_id", "position", "season", "target_fppg")
  schema_ok <- all(needed %in% names(h))
  cat(if (schema_ok) "PASS" else "FAIL", " - model_table.csv schema\n", sep = "")
  all_ok <- all_ok && schema_ok
}
if (file.exists(checks[[2]])) {
  h <- readr::read_csv(checks[[2]], n_max = 0, show_col_types = FALSE, progress = FALSE)
  needed <- c("player_id", "position", "current_team")
  schema_ok <- all(needed %in% names(h))
  cat(if (schema_ok) "PASS" else "FAIL", " - projection table schema\n", sep = "")
  all_ok <- all_ok && schema_ok
}

cat("\n")
if (all_ok) {
  cat("PASS: Fantasy Model 2.0 fast-run prerequisites are available.\n")
  cat("Next: source(\"MOBILE_RUN_FAST.R\")\n")
} else {
  cat("FAIL: One or more prerequisites are missing.\n")
  cat("If only PBP context is missing, run source(\"BUILD_PBP_CONTEXT.R\").\n")
  cat("For a clean rebuild, run source(\"MOBILE_RUN.R\").\n")
  stop("Fantasy Model 2.0 prerequisite check failed.")
}
