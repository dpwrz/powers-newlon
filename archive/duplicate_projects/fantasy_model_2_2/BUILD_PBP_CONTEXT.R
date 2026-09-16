# ============================================================
# FANTASY MODEL 2.0 - ONE-TIME / RESUMABLE PBP FEATURE STORE BUILD
# ============================================================

cat("\n========================================\n")
cat(" FANTASY MODEL 2.0 - PBP CONTEXT BUILD\n")
cat("========================================\n\n")

source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr", "nflreadr", "janitor", "rpart"))
source("R/team_context.R")

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

players_path <- "data/raw/players.csv"
players <- if (file.exists(players_path)) {
  readr::read_csv(players_path, show_col_types = FALSE)
} else {
  cat("[CONTEXT] Downloading player metadata for QB positional target profiles...\n")
  x <- nflreadr::load_players()
  readr::write_csv(x, players_path)
  x
}

data_end <- min(TRAIN_END, nflreadr::most_recent_season())
context_seasons <- if (data_end >= CONTEXT_START) CONTEXT_START:data_end else integer()
if (length(context_seasons) == 0) stop("No PBP context seasons are available.")

cat("[CONTEXT] Seasons: ", min(context_seasons), "-", max(context_seasons), "\n", sep = "")
cat("[CONTEXT] Each season is processed independently and checkpointed.\n")
cat("[CONTEXT] If Posit Cloud restarts, rerun this same file and completed seasons will be skipped.\n\n")

build_team_context_files(context_seasons, output_dir = "data/raw", player_lookup = players)

cat("\n========================================\n")
cat(" PBP FEATURE STORE COMPLETE\n")
cat("========================================\n")
cat("You can now run source(\"MOBILE_RUN.R\"). Historical PBP will be reused from the compact context cache.\n\n")
