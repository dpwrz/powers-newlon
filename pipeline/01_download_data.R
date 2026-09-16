# ============================================================
# STEP 1 - DOWNLOAD BASE DATA + COMPACT TEAM CONTEXT
# FANTASY MODEL 2.0
# ============================================================

source("config.R")

cat("\n[DATA] Checking required packages...\n")
ensure_packages(c("dplyr", "readr", "tidyr", "nflreadr", "janitor", "rpart"))
source("R/team_context.R")
cat("[DATA] Packages ready.\n")
cat("[MODEL] Engine selected: ", resolve_model_engine(), "\n", sep = "")

write_optional_csv <- function(x, path) {
  if (ncol(x) == 0) x <- data.frame(.empty = character())
  readr::write_csv(x, path)
}

latest_supported <- nflreadr::most_recent_season()
data_end <- min(TRAIN_END, latest_supported)
download_start <- max(1999, TRAIN_START - HISTORY_LOOKBACK_YEARS)

if (data_end < TRAIN_START) stop("No completed NFL seasons are available for training.")
seasons <- download_start:data_end

cat("[DATA] Projection season: ", CURRENT_SEASON, "\n", sep = "")
cat("[DATA] Model target seasons: ", TRAIN_START, "-", TRAIN_END, "\n", sep = "")
cat("[DATA] Downloading compact box-score history: ", min(seasons), "-", max(seasons), "\n", sep = "")

player_stats <- nflreadr::load_player_stats(seasons = seasons, summary_level = "reg")
readr::write_csv(player_stats, "data/raw/player_stats.csv")
cat("[DATA] Player stats saved: ", nrow(player_stats), " rows.\n", sep = "")

team_stats <- tryCatch(
  nflreadr::load_team_stats(seasons = seasons, summary_level = "reg"),
  error = function(e) {
    warning("Team stats unavailable; team-environment features will use zeros. ", conditionMessage(e))
    data.frame()
  }
)
write_optional_csv(team_stats, "data/raw/team_stats.csv")
cat("[DATA] Team stats saved: ", nrow(team_stats), " rows.\n", sep = "")

players <- tryCatch(
  nflreadr::load_players(),
  error = function(e) {
    warning("Player metadata unavailable: ", conditionMessage(e))
    data.frame()
  }
)
write_optional_csv(players, "data/raw/players.csv")
cat("[DATA] Player metadata saved: ", nrow(players), " rows.\n", sep = "")

draft <- tryCatch(
  nflreadr::load_draft_picks(),
  error = function(e) {
    warning("Draft data unavailable: ", conditionMessage(e))
    data.frame()
  }
)
write_optional_csv(draft, "data/raw/draft_picks.csv")
cat("[DATA] Draft data saved: ", nrow(draft), " rows.\n", sep = "")

current_roster <- tryCatch(
  nflreadr::load_rosters(seasons = CURRENT_SEASON),
  error = function(e) {
    warning("Current roster data unavailable; Fantasy Model 2.0 will fall back to last-season players. ", conditionMessage(e))
    data.frame()
  }
)
write_optional_csv(current_roster, paste0("data/raw/roster_", CURRENT_SEASON, ".csv"))
cat("[DATA] Current roster saved: ", nrow(current_roster), " rows.\n", sep = "")

# ------------------------------------------------------------
# Shared compact PBP-derived team/QB/receiver context (built in 1.1, reused by 1.2/2.0).
# We process PBP one season at a time and keep only small season-level
# summaries. The large PBP frames are never written to the project.
# ------------------------------------------------------------
context_seasons <- if (data_end >= CONTEXT_START) CONTEXT_START:data_end else integer()
context_files <- file.path("data/raw", c("team_context.csv", "qb_context.csv", "receiver_context.csv", "team_qb_context.csv"))
context_current <- FALSE
if (length(context_seasons) > 0 && all(file.exists(context_files))) {
  context_current <- tryCatch({
    x <- readr::read_csv("data/raw/team_context.csv", show_col_types = FALSE)
    "season" %in% names(x) && nrow(x) > 0 && max(as.numeric(x$season), na.rm = TRUE) >= max(context_seasons)
  }, error = function(e) FALSE)
}

if (length(context_seasons) > 0) {
  if (context_current) {
    cat("[CONTEXT] Cached team/QB/receiver context already covers ", min(context_seasons), "-", max(context_seasons), ". Reusing it.\n", sep = "")
  } else {
    cat("[CONTEXT] Building detailed context for ", min(context_seasons), "-", max(context_seasons), ".\n", sep = "")
    cat("[CONTEXT] PBP is streamed one season at a time using only required columns; each completed season is checkpointed.\n")
    build_team_context_files(context_seasons, output_dir = "data/raw", player_lookup = players)
  }
}

writeLines(as.character(data_end), "data/raw/latest_training_season.txt")
cat("[DATA] Download complete. Latest completed training season: ", data_end, "\n\n", sep = "")
