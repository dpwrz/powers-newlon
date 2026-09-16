# ============================================================
# FANTASY MODEL 3.0.8 - MODEL-FIRST APP DATA BRIDGE
# ============================================================
# Builds a compact current-week snapshot from Fantasy Model outputs so the
# Shiny app never needs to parse the 25-70 MB weekly tables while connected.

source("config.R")
ensure_packages(c("readr", "dplyr", "tibble"))

fm3_choose_current_week <- function(d) {
  if (is.null(d) || !is.data.frame(d) || !nrow(d) || !"week" %in% names(d)) return(NA_integer_)
  wk <- suppressWarnings(as.integer(d[["week"]]))
  ok <- is.finite(wk)
  if (!any(ok)) return(NA_integer_)

  if ("game_played" %in% names(d)) {
    gp <- suppressWarnings(as.numeric(d[["game_played"]]))
    unplayed <- wk[ok & (!is.finite(gp) | gp <= 0)]
    if (length(unplayed)) return(as.integer(min(unplayed, na.rm = TRUE)))
  }
  as.integer(max(wk[ok], na.rm = TRUE))
}

fm3_build_current_week_snapshot <- function(force = TRUE) {
  out_path <- FM3_APP_WEEKLY_SNAPSHOT_PATH
  if (!isTRUE(force) && file.exists(out_path)) return(invisible(out_path))

  weekly_path <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
  if (!file.exists(weekly_path)) {
    message("[3.0.8 APP] Weekly projection file not found; compact weekly snapshot skipped.")
    return(invisible(NULL))
  }

  weekly_cols <- c(
    "player_id", "player_display_name", "position", "team", "season", "week",
    "opponent", "game_played", "gameday", "projected_weekly_fppg_24",
    "projected_weekly_fppg", "weekly_floor", "weekly_ceiling",
    "projection_confidence", "expected_abs_error", "matchup_grade",
    "boom_probability", "bust_probability", "injury_status", "practice_status",
    "availability_factor", "model24_promoted", "risk_score_24"
  )

  w <- readr::read_csv(
    weekly_path,
    col_select = dplyr::any_of(weekly_cols),
    show_col_types = FALSE,
    progress = FALSE
  )
  if (!nrow(w)) return(invisible(NULL))
  if ("season" %in% names(w)) w <- w[w[["season"]] == CURRENT_SEASON, , drop = FALSE]
  live_week <- fm3_choose_current_week(w)
  if (is.finite(live_week)) w <- w[suppressWarnings(as.integer(w[["week"]])) == live_week, , drop = FALSE]

  # Normalize the 2.4.3 point forecast name. Never substitute Sleeper values.
  if (!"projected_weekly_fppg_24" %in% names(w) && "projected_weekly_fppg" %in% names(w)) {
    w[["projected_weekly_fppg_24"]] <- suppressWarnings(as.numeric(w[["projected_weekly_fppg"]]))
  }
  if (!"projected_weekly_fppg_24" %in% names(w)) w[["projected_weekly_fppg_24"]] <- NA_real_

  role_path <- ROLE30_LIVE_OUTPUT
  if (file.exists(role_path)) {
    role_cols <- c(
      "player_id", "season", "week", "projected_targets_30", "projected_carries_30",
      "projected_snap_share_30", "role_regime_probability_30", "role_uncertainty_30"
    )
    r <- tryCatch(
      readr::read_csv(role_path, col_select = dplyr::any_of(role_cols), show_col_types = FALSE, progress = FALSE),
      error = function(e) tibble::tibble()
    )
    if (nrow(r)) {
      if ("season" %in% names(r)) r <- r[r[["season"]] == CURRENT_SEASON, , drop = FALSE]
      if (is.finite(live_week) && "week" %in% names(r)) r <- r[suppressWarnings(as.integer(r[["week"]])) == live_week, , drop = FALSE]
      r <- r |> dplyr::select(-dplyr::any_of(c("season", "week"))) |> dplyr::distinct(.data$player_id, .keep_all = TRUE)
      w <- dplyr::left_join(w, r, by = "player_id")
    }
  }

  # Keep one row per player and only fields useful to the application/copilot.
  w <- w |>
    dplyr::distinct(.data$player_id, .keep_all = TRUE) |>
    dplyr::arrange(.data$position, dplyr::desc(.data$projected_weekly_fppg_24))

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(w, out_path)
  message("[3.0.8 APP] Wrote model-only current-week snapshot: ", out_path,
          " (", nrow(w), " players; week ", ifelse(is.finite(live_week), live_week, "?"), ").")
  invisible(out_path)
}

fm3_load_current_week_snapshot <- function() {
  if (!file.exists(FM3_APP_WEEKLY_SNAPSHOT_PATH)) return(tibble::tibble())
  tryCatch(
    readr::read_csv(FM3_APP_WEEKLY_SNAPSHOT_PATH, show_col_types = FALSE, progress = FALSE),
    error = function(e) tibble::tibble()
  )
}
