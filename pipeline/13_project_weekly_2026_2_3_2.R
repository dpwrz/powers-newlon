# ============================================================
# FANTASY MODEL 2.3.2 - LIVE WEEKLY / ROS PROJECTION
# ============================================================
# First refresh the complete 2.3 projection grid, then apply only those 2.3.2
# position controllers that earned promotion in honest chronological OOF tests.
source("pipeline/11_project_weekly_2026.R")
source("R/weekly_engine_232.R")

required232 <- c(
  "output/weekly_2_3_2_selected_base.csv",
  "output/weekly_2_3_2_selected_controller.csv",
  "output/weekly_2_3_2_promotion.csv",
  "output/weekly_2_3_2_residual_pool.csv",
  "output/weekly_2_3_2_confidence_calibration.csv"
)
for (pp in required232) if (!file.exists(pp)) stop("Missing 2.3.2 prerequisite: ", pp, ". Run 12_train_validate_signal_feedback_2_3_2.R first.")

base_tbl <- readr::read_csv("output/weekly_2_3_2_selected_base.csv", show_col_types = FALSE, progress = FALSE)
promotion <- readr::read_csv("output/weekly_2_3_2_promotion.csv", show_col_types = FALSE, progress = FALSE)
res232 <- readr::read_csv("output/weekly_2_3_2_residual_pool.csv", show_col_types = FALSE, progress = FALSE)
conf232 <- readr::read_csv("output/weekly_2_3_2_confidence_calibration.csv", show_col_types = FALSE, progress = FALSE)

hist_path <- paste0("output/projection_history_", CURRENT_SEASON, ".csv")
# Prefer the compact frozen pregame archive when it exists. This makes live PID
# feedback persistent in GitHub Actions without committing the much larger full
# projection-history file after every refresh. The archive stores the 2.3.2
# controller forecast separately from the final 2.5 forecast.
pregame_path232 <- paste0("output/pregame_projection_archive_", CURRENT_SEASON, ".csv")
if (file.exists(pregame_path232)) {
  .pregame232 <- tryCatch(readr::read_csv(pregame_path232, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
  if (nrow(.pregame232) && all(c("week", "player_id") %in% names(.pregame232))) {
    if (!"captured_at" %in% names(.pregame232)) .pregame232$captured_at <- ""
    if (!"controller_projection_232" %in% names(.pregame232)) {
      .pregame232$controller_projection_232 <- if ("projection" %in% names(.pregame232)) .pregame232$projection else NA_real_
    }
    if (!"projection" %in% names(.pregame232)) .pregame232$projection <- .pregame232$controller_projection_232
    hist <- .pregame232 |>
      dplyr::transmute(
        generated_at = as.character(captured_at),
        week = as.integer(wk232_num(week)),
        player_id = as.character(player_id),
        projected_weekly_fppg_232 = wk232_num(controller_projection_232),
        projected_weekly_fppg = wk232_num(projection)
      )
    cat("[2.3.2 LIVE] PID feedback history: frozen pregame archive (", nrow(hist), " rows).\n", sep = "")
  } else {
    hist <- if (file.exists(hist_path)) tryCatch(readr::read_csv(hist_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
  }
} else {
  hist <- if (file.exists(hist_path)) tryCatch(readr::read_csv(hist_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
}
actual232 <- current_weekly
if (nrow(actual232) > 0 && !"actual_fppg" %in% names(actual232)) actual232$actual_fppg <- actual232$weekly_fppg

live_base_col232 <- function(hist_col) {
  switch(hist_col,
    legacy21_honest_fppg = "legacy21_honest_fppg",
    honest22_fppg = "honest22_fppg",
    meta_precal_fppg = "meta_precal_fppg",
    candidate23_fppg = "candidate23_fppg",
    honest_final_fppg = "projected_weekly_fppg_pre_availability",
    "projected_weekly_fppg_pre_availability"
  )
}

assign_confidence232 <- function(risk, pos, table) {
  z <- table[table$position == pos, , drop = FALSE]
  if (nrow(z) == 0) return(list(confidence = rep("Medium", length(risk)), expected = rep(NA_real_, length(risk))))
  q33 <- wk232_num(z$q33_risk[1]); q67 <- wk232_num(z$q67_risk[1])
  lab <- ifelse(risk <= q33, "High", ifelse(risk <= q67, "Medium", "Low"))
  expected <- vapply(lab, function(ll) {
    rr <- z[z$confidence == ll, , drop = FALSE]
    if (nrow(rr)) wk232_num(rr$expected_abs_error[1]) else NA_real_
  }, numeric(1))
  list(confidence = lab, expected = expected)
}

future232_rows <- list()
for (pos in POSITIONS) {
  d <- future_pred |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next
  hist_base <- base_tbl$base_column[match(pos, base_tbl$position)]
  if (length(hist_base) == 0 || is.na(hist_base)) hist_base <- "honest_final_fppg"
  base_col <- live_base_col232(hist_base)
  if (!base_col %in% names(d)) base_col <- "projected_weekly_fppg_pre_availability"
  d$base232_fppg <- wk232_num(d[[base_col]])

  sig_path <- paste0("models/weekly_2_3_2_signal_controller_", pos, ".rds")
  pid_path <- paste0("models/weekly_2_3_2_pid_controller_", pos, ".rds")
  sig <- if (file.exists(sig_path)) readRDS(sig_path) else list(enabled = FALSE)
  pid <- if (file.exists(pid_path)) readRDS(pid_path) else list(enabled = FALSE, kp = 0, ki = 0, kd = 0)
  state <- build_live_pid_state232(hist, actual232 |> dplyr::filter(position == pos), pos)
  d <- predict_live_controller232(d, pos, sig, pid, state, "base232_fppg")

  promote_row <- promotion[promotion$position == pos, , drop = FALSE]
  promote <- nrow(promote_row) > 0 && isTRUE(as.logical(promote_row$promoted_for_2026[1]))
  d$model232_promoted <- promote
  d$controller_base_method_232 <- hist_base
  d$controller_base_fppg_232 <- d$base232_fppg
  d$projected_weekly_fppg_23 <- d$projected_weekly_fppg
  d$projected_weekly_fppg_pre_availability_23 <- d$projected_weekly_fppg_pre_availability

  if (promote) {
    unplayed <- d$is_actual == 0
    d$projected_weekly_fppg_pre_availability[unplayed] <- d$projected_weekly_fppg_pre_availability_232[unplayed]
    d$projected_weekly_fppg[unplayed] <- d$projected_weekly_fppg_pre_availability[unplayed] * d$availability_factor[unplayed]
    d$projected_weekly_fppg_232 <- d$projected_weekly_fppg
    d$risk_score_232 <- wk232_num(d$model_disagreement_23) + 0.75 * abs(wk232_num(d$feedback_total_correction_232)) + 0.35 * wk232_num(d$roll3_fppg_sd)
    cc <- assign_confidence232(d$risk_score_232, pos, conf232)
    d$projection_confidence[unplayed] <- cc$confidence[unplayed]
    d$expected_abs_error[unplayed] <- cc$expected[unplayed]
  } else {
    d$projected_weekly_fppg_232 <- d$projected_weekly_fppg
    d$risk_score_232 <- wk232_num(d$model_disagreement_23)
  }

  # Recalculate empirical range around the promoted forecast using honest 2.3.2
  # residuals; non-promoted positions retain the locked 2.3 residual pool.
  rr <- if (promote) res232 |> dplyr::filter(position == pos) |> dplyr::pull(residual) else residual_pool |> dplyr::filter(position == pos) |> dplyr::pull(residual)
  dist <- empirical_distribution22(d$projected_weekly_fppg, rr, pos)
  for (nm in names(dist)) d[[nm]] <- dist[[nm]]
  out_mask <- d$availability_factor <= 0
  if (any(out_mask)) {
    d$weekly_median[out_mask] <- 0; d$weekly_floor[out_mask] <- 0; d$weekly_ceiling[out_mask] <- 0
    d$boom_probability[out_mask] <- 0; d$bust_probability[out_mask] <- 1
  }
  future232_rows[[length(future232_rows) + 1]] <- d
}

future_pred <- dplyr::bind_rows(future232_rows) |>
  dplyr::group_by(week, position) |>
  dplyr::arrange(dplyr::desc(projected_weekly_fppg), .by_group = TRUE) |>
  dplyr::mutate(weekly_position_rank = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::arrange(week, dplyr::desc(projected_weekly_fppg))

# Recalculate Top-N probability under the actual production residual pool.
add_topn232 <- function(d) {
  if (nrow(d) == 0) return(d)
  pos <- as.character(d$position[1]); cutoff <- as.numeric(WEEKLY_22_TOP_PROB_CUTOFF[[pos]])
  promote_row <- promotion[promotion$position == pos, , drop = FALSE]
  promote <- nrow(promote_row) > 0 && isTRUE(as.logical(promote_row$promoted_for_2026[1]))
  rr <- if (promote) res232 |> dplyr::filter(position == pos) |> dplyr::pull(residual) else residual_pool |> dplyr::filter(position == pos) |> dplyr::pull(residual)
  rr <- wk232_num(rr); rr <- rr[is.finite(rr)]
  if (length(rr) < 30 || !is.finite(cutoff)) { d$topN_probability <- NA_real_; return(d) }
  draws <- min(1000L, WEEKLY_22_SIM_DRAWS); counts <- rep(0, nrow(d))
  set.seed(SEED + as.integer(d$week[1]) * 1000 + match(pos, POSITIONS))
  for (b in seq_len(draws)) {
    sim <- pmax(0, d$projected_weekly_fppg + sample(rr, nrow(d), replace = TRUE))
    counts <- counts + as.numeric(rank(-sim, ties.method = "min") <= cutoff)
  }
  d$topN_probability <- counts / draws
  d
}
future_pred <- dplyr::bind_rows(lapply(split(future_pred, interaction(future_pred$week, future_pred$position, drop = TRUE)), add_topn232)) |>
  dplyr::arrange(week, dplyr::desc(projected_weekly_fppg))

# Season ledger must reflect corrected future forecasts while preserving actuals.
future_pred$season_ledger_points <- ifelse(future_pred$is_actual == 1, future_pred$actual_weekly_fppg, future_pred$projected_weekly_fppg)
readr::write_csv(future_pred, paste0("output/weekly_", CURRENT_SEASON, "_projections.csv"))

week_now <- future_pred |> dplyr::filter(week == current_week, is_actual == 0) |> dplyr::arrange(dplyr::desc(projected_weekly_fppg))
readr::write_csv(week_now, paste0("output/week_", current_week, "_rankings.csv"))

# Append a 2.3.2 snapshot. These become feedback observations only AFTER the
# corresponding games are complete and actual points are available.
if (nrow(week_now) > 0) {
  snap <- week_now |> dplyr::transmute(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), week, player_id, player_display_name, position, team, opponent,
    projected_weekly_fppg, projected_weekly_fppg_232, projected_weekly_fppg_23,
    controller_base_fppg_232, controller_base_method_232,
    signal_correction_232, pid_correction_232, feedback_total_correction_232,
    pid_p_232, pid_i_232, pid_d_232, model232_promoted,
    weekly_floor, weekly_ceiling, projection_confidence, expected_abs_error,
    calibrated_matchup_delta_23, model_disagreement_23,
    version_weight_21, version_weight_22, version_weight_23
  )
  old_hist <- if (file.exists(hist_path)) tryCatch(readr::read_csv(hist_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
  readr::write_csv(dplyr::bind_rows(old_hist, snap), hist_path)
}

ros <- future_pred |>
  dplyr::group_by(player_id, player_display_name, position, team) |>
  dplyr::summarise(
    actual_points_to_date = sum(dplyr::if_else(is_actual == 1, actual_weekly_fppg, 0), na.rm = TRUE),
    projected_remaining_points = sum(dplyr::if_else(is_actual == 0, projected_weekly_fppg, 0), na.rm = TRUE),
    projected_full_season_points = sum(season_ledger_points, na.rm = TRUE),
    remaining_games = sum(is_actual == 0), .groups = "drop") |>
  dplyr::group_by(position) |>
  dplyr::arrange(dplyr::desc(projected_full_season_points), .by_group = TRUE) |>
  dplyr::mutate(ros_position_rank = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(projected_full_season_points))
readr::write_csv(ros, paste0("output/rest_of_season_", CURRENT_SEASON, ".csv"))

cat("[2.3.2 LIVE] Applied signal/error-feedback controller only to positions that passed the honest promotion gate.\n")
cat("[2.3.2 LIVE] Upcoming week: ", current_week, " | ranked players: ", nrow(week_now), "\n", sep = "")
print(promotion)
