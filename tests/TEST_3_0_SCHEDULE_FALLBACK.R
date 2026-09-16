# Fantasy Model 3.0.1 schedule fallback regression test
source("config.R")
ensure_packages(c("dplyr", "readr"))
source("R/weekly_engine.R")

static_path <- "data/raw/schedules_weekly_model.csv"
if (!file.exists(static_path)) stop("Missing bundled schedule cache: ", static_path)
s <- readr::read_csv(static_path, show_col_types = FALSE, progress = FALSE)
if (!"season" %in% names(s)) stop("Bundled schedule cache has no season column.")
s <- s[suppressWarnings(as.integer(s$season)) == CURRENT_SEASON, , drop = FALSE]
if (!nrow(s)) stop("Bundled schedule cache has no rows for CURRENT_SEASON = ", CURRENT_SEASON)
t <- schedule_team_rows21(s)
if (!all(c("season", "week", "team", "opponent", "gameday") %in% names(t))) stop("Team schedule schema is incomplete.")
if (!nrow(t)) stop("Team schedule conversion returned zero rows.")
empty <- schedule_team_rows21(data.frame())
if (!all(c("season", "week", "team", "opponent") %in% names(empty))) stop("Empty schedule schema is not typed.")
cat("[PASS] Model 3.0.1 schedule fallback and typed-empty schedule are valid.\\n")
