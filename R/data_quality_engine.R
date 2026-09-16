# ============================================================
# FANTASY MODEL 3.0 - DATA / MODEL HEALTH
# ============================================================
source("config.R")
ensure_packages(c("dplyr", "readr", "tibble", "purrr"))

fm3_file_health <- function(path, label, expected = TRUE) {
  exists <- file.exists(path)
  info <- if (exists) file.info(path) else NULL
  tibble::tibble(
    source = label,
    path = path,
    status = if (exists) "OK" else if (expected) "MISSING" else "OPTIONAL",
    modified = if (exists) format(info$mtime, "%Y-%m-%d %H:%M") else "—",
    size_mb = if (exists) round(as.numeric(info$size) / 1024^2, 2) else NA_real_
  )
}

fm3_data_health <- function(league_id = NULL) {
  paths <- list(
    c("Production rankings", paste0("output/final_", CURRENT_SEASON, "_rankings.csv"), TRUE),
    c("Weekly 2.4.3 projections", paste0("output/weekly_", CURRENT_SEASON, "_projections.csv"), TRUE),
    c("Dynasty rankings", "output/final_dynasty_rankings.csv", TRUE),
    c("Weekly history", "data/processed/weekly_model_table_2_3.csv", TRUE),
    c("Role 3.0 enriched table", ROLE30_OUTPUT_TABLE, FALSE),
    c("Role 3.0 validation", "output/role_3_0_validation_metrics.csv", FALSE),
    c("Role 3.0 live forecast", ROLE30_LIVE_OUTPUT, FALSE),
    c("Sleeper identity map", "data/processed/sleeper_player_identity_3_0.csv", FALSE),
    c("Sleeper compact player IDs", SLEEPER_COMPACT_PLAYERS_PATH, TRUE),
    c("Fantasy Model app weekly snapshot", FM3_APP_WEEKLY_SNAPSHOT_PATH, TRUE),
    c("Sleeper raw player download (prep only)", file.path(SLEEPER_CACHE_DIR, "sleeper_players_nfl.rds"), FALSE)
  )
  if (!is.null(league_id) && nzchar(as.character(league_id))) {
    paths[[length(paths) + 1]] <- c("Sleeper league state", file.path(SLEEPER_CACHE_DIR, paste0("league_state_", league_id, ".rds")), FALSE)
  }
  purrr::map_dfr(paths, function(x) fm3_file_health(x[[2]], x[[1]], identical(x[[3]], "TRUE")))
}

fm3_role_health <- function() {
  path <- "output/role_3_0_validation_metrics.csv"
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      mae_gain_pct_display = round(100 * mae_gain_pct, 2),
      status = dplyr::if_else(promoted %in% TRUE, "PROMOTED FOR ROLE OUTPUT", "BASELINE RETAINED")
    )
}
