# ============================================================
# FANTASY MODEL 2.3 - RESUMABLE WEEKLY DEFENSE PBP FEATURE STORE
# ============================================================
# Reads only the play-by-play fields required for matchup defense.
# Each season is summarized and checkpointed immediately so Posit
# Cloud never needs to hold several PBP seasons in memory.

cat("\n========================================\n")
cat(" FANTASY MODEL 2.3 - WEEKLY DEFENSE CONTEXT\n")
cat("========================================\n\n")
source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr", "nflreadr"))
source("R/weekly_engine.R")

dir.create("data/raw/weekly_defense_by_season", recursive = TRUE, showWarnings = FALSE)

WEEKLY_PBP_COLUMNS_21 <- c(
  "season", "season_type", "week", "game_id", "posteam", "defteam", "qtr", "down", "wp",
  "pass_attempt", "rush_attempt", "qb_dropback", "qb_scramble", "epa", "success", "sack", "qb_hit",
  "air_yards", "yardline_100", "pass_touchdown", "rush_touchdown", "pass_location"
)

load_weekly_pbp_compact21 <- function(season) {
  season <- as.integer(season)
  old_timeout <- getOption("timeout")
  options(timeout = max(900, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)
  parquet_url <- sprintf("https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_%d.parquet", season)
  csv_url <- sprintf("https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_%d.csv", season)

  if (requireNamespace("arrow", quietly = TRUE)) {
    tmp <- tempfile(pattern = paste0("weekly_pbp_", season, "_"), fileext = ".parquet")
    on.exit(unlink(tmp), add = TRUE)
    ok <- tryCatch({
      status <- utils::download.file(parquet_url, tmp, mode = "wb", quiet = TRUE, method = "libcurl")
      identical(status, 0L)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(ok) && file.exists(tmp) && file.info(tmp)$size > 0) {
      out <- tryCatch(arrow::read_parquet(tmp, col_select = WEEKLY_PBP_COLUMNS_21, as_data_frame = TRUE), error = function(e) NULL)
      if (!is.null(out)) {
        cat("[WEEKLY DEF] ", season, ": parquet selected-column reader.\n", sep = "")
        return(as.data.frame(out))
      }
    }
  }

  tmp <- tempfile(pattern = paste0("weekly_pbp_", season, "_"), fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  cat("[WEEKLY DEF] ", season, ": CSV selected-column fallback.\n", sep = "")
  status <- utils::download.file(csv_url, tmp, mode = "wb", quiet = TRUE, method = "libcurl")
  if (!identical(status, 0L) || !file.exists(tmp) || file.info(tmp)$size <= 0) stop("Could not download PBP for ", season)
  # Use a header pass so schema changes cannot crash col_select.
  hdr <- names(readr::read_csv(tmp, n_max = 0, show_col_types = FALSE, progress = FALSE))
  keep <- intersect(WEEKLY_PBP_COLUMNS_21, hdr)
  required <- c("season", "season_type", "week", "defteam")
  if (!all(required %in% keep)) stop("PBP ", season, " missing required weekly-defense fields: ", paste(setdiff(required, keep), collapse = ", "))
  as.data.frame(readr::read_csv(tmp, col_select = dplyr::all_of(keep), show_col_types = FALSE, progress = FALSE))
}

summarize_defense_week21 <- function(pbp) {
  if (nrow(pbp) == 0) return(data.frame())
  for (nm in WEEKLY_PBP_COLUMNS_21) pbp <- ensure_weekly_col21(pbp, nm, NA)
  p <- pbp |>
    dplyr::filter(season_type == "REG", !is.na(defteam), defteam != "") |>
    dplyr::mutate(
      season = as.integer(wk_num(season)), week = as.integer(wk_num(week)), defense = wk_chr(defteam),
      pass_attempt = dplyr::coalesce(wk_num(pass_attempt), 0), rush_attempt = dplyr::coalesce(wk_num(rush_attempt), 0),
      qb_dropback = dplyr::coalesce(wk_num(qb_dropback), 0), qb_scramble = dplyr::coalesce(wk_num(qb_scramble), 0),
      epa = wk_num(epa), success = wk_num(success), sack = dplyr::coalesce(wk_num(sack), 0), qb_hit = dplyr::coalesce(wk_num(qb_hit), 0),
      air_yards = wk_num(air_yards), yardline_100 = wk_num(yardline_100),
      pass_touchdown = dplyr::coalesce(wk_num(pass_touchdown), 0), rush_touchdown = dplyr::coalesce(wk_num(rush_touchdown), 0),
      is_pass = as.numeric(qb_dropback == 1 | pass_attempt == 1),
      is_rush = as.numeric(rush_attempt == 1 & qb_scramble != 1),
      is_deep_pass = as.numeric(pass_attempt == 1 & is.finite(air_yards) & air_yards >= 15),
      is_explosive_pass = as.numeric(pass_attempt == 1 & is.finite(epa) & epa >= 1.5),
      is_redzone = as.numeric(is.finite(yardline_100) & yardline_100 <= 20),
      is_middle_pass = as.numeric(pass_attempt == 1 & pass_location == "middle")
    )

  p |>
    dplyr::group_by(season, week, defense) |>
    dplyr::summarise(
      def_games = dplyr::n_distinct(game_id),
      def_dropbacks = sum(is_pass, na.rm = TRUE), def_rushes = sum(is_rush, na.rm = TRUE),
      def_pass_epa_allowed = ifelse(sum(is_pass & is.finite(epa), na.rm = TRUE) > 0, mean(epa[is_pass == 1 & is.finite(epa)], na.rm = TRUE), 0),
      def_rush_epa_allowed = ifelse(sum(is_rush & is.finite(epa), na.rm = TRUE) > 0, mean(epa[is_rush == 1 & is.finite(epa)], na.rm = TRUE), 0),
      def_pass_success_allowed = ifelse(sum(is_pass & is.finite(success), na.rm = TRUE) > 0, mean(success[is_pass == 1 & is.finite(success)], na.rm = TRUE), 0),
      def_rush_success_allowed = ifelse(sum(is_rush & is.finite(success), na.rm = TRUE) > 0, mean(success[is_rush == 1 & is.finite(success)], na.rm = TRUE), 0),
      def_explosive_pass_rate = wk_rate(sum(is_explosive_pass, na.rm = TRUE), sum(pass_attempt == 1, na.rm = TRUE)),
      def_deep_pass_rate = wk_rate(sum(is_deep_pass, na.rm = TRUE), sum(pass_attempt == 1, na.rm = TRUE)),
      def_middle_pass_rate = wk_rate(sum(is_middle_pass, na.rm = TRUE), sum(pass_attempt == 1, na.rm = TRUE)),
      def_deep_epa_allowed = ifelse(sum(is_deep_pass == 1 & is.finite(epa), na.rm = TRUE) > 0, mean(epa[is_deep_pass == 1 & is.finite(epa)], na.rm = TRUE), 0),
      def_middle_epa_allowed = ifelse(sum(is_middle_pass == 1 & is.finite(epa), na.rm = TRUE) > 0, mean(epa[is_middle_pass == 1 & is.finite(epa)], na.rm = TRUE), 0),
      def_sack_rate = wk_rate(sum(sack == 1, na.rm = TRUE), sum(is_pass, na.rm = TRUE)),
      def_qb_hit_rate = wk_rate(sum(qb_hit == 1, na.rm = TRUE), sum(is_pass, na.rm = TRUE)),
      def_redzone_pass_td_rate = wk_rate(sum(pass_touchdown == 1 & is_redzone == 1, na.rm = TRUE), sum(is_pass == 1 & is_redzone == 1, na.rm = TRUE)),
      def_redzone_rush_td_rate = wk_rate(sum(rush_touchdown == 1 & is_redzone == 1, na.rm = TRUE), sum(is_rush == 1 & is_redzone == 1, na.rm = TRUE)),
      .groups = "drop"
    )
}

context_end <- min(CURRENT_SEASON, nflreadr::most_recent_season())
all_context_seasons <- WEEKLY_TRAIN_START:context_end
current_only <- exists("WEEKLY_DEFENSE_CURRENT_ONLY", inherits = TRUE) && isTRUE(get("WEEKLY_DEFENSE_CURRENT_ONLY", inherits = TRUE))
seasons <- if (current_only) CURRENT_SEASON else all_context_seasons
cat("[WEEKLY DEF] Processing seasons: ", min(seasons), "-", max(seasons),
    if (current_only) " (current-season refresh only)" else "",
    " (training still stops at ", TRAIN_END, ")\n", sep = "")

for (yr in seasons) {
  checkpoint <- file.path("data/raw/weekly_defense_by_season", paste0("defense_week_", yr, ".csv"))
  if (file.exists(checkpoint) && yr != CURRENT_SEASON) {
    chk <- tryCatch(readr::read_csv(checkpoint, show_col_types = FALSE, n_max = 3), error = function(e) data.frame())
    if (nrow(chk) > 0 && all(c("season", "week", "defense") %in% names(chk))) {
      cat("[WEEKLY DEF] ", yr, " checkpoint found; skipping.\n", sep = "")
      next
    }
  }
  if (yr == CURRENT_SEASON && file.exists(checkpoint)) cat("[WEEKLY DEF] Refreshing current-season checkpoint ", yr, "...\n", sep = "")
  cat("[WEEKLY DEF] Processing ", yr, "...\n", sep = "")
  pbp <- tryCatch(load_weekly_pbp_compact21(yr), error = function(e) {
    if (yr == CURRENT_SEASON) { warning("Current-season PBP not available yet: ", conditionMessage(e)); return(data.frame()) }
    stop(e)
  })
  summary <- summarize_defense_week21(pbp)
  if (nrow(summary) == 0) {
    if (yr == CURRENT_SEASON) {
      cat("[WEEKLY DEF] No completed regular-season PBP yet for ", yr, "; current-season checkpoint not written.\n", sep = "")
      rm(pbp, summary); invisible(gc(full = TRUE)); next
    }
    stop("No regular-season defense rows produced for ", yr)
  }
  readr::write_csv(summary, checkpoint)
  rm(pbp, summary); invisible(gc(full = TRUE))
  cat("[WEEKLY DEF] ", yr, " checkpoint saved.\n", sep = "")
}

# Always recombine every available historical checkpoint after a current-only
# refresh; never overwrite the feature store with just the current season.
combine_seasons <- all_context_seasons
files <- file.path("data/raw/weekly_defense_by_season", paste0("defense_week_", combine_seasons, ".csv"))
files <- files[file.exists(files)]
combined <- dplyr::bind_rows(lapply(files, function(f) readr::read_csv(f, show_col_types = FALSE, progress = FALSE)))
readr::write_csv(combined, "data/raw/weekly_defense_context_raw.csv")
cat("\n[WEEKLY DEF] Combined weekly defense store: ", nrow(combined), " rows.\n", sep = "")
cat("[WEEKLY DEF] Complete.\n\n")
