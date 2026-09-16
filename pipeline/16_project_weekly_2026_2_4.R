# ============================================================
# FANTASY MODEL 2.4 - LIVE 2026 SIGNAL-FIRST PROJECTION
# ============================================================
# Build the complete 2.3.2 live grid first, then explicitly restore the STRICT
# locked 2.3/2.3.2 benchmark selected by 2.4 and apply the 2.4 challenger only
# at positions that passed MAE + RMSE + starter + correlation gates.

source("pipeline/13_project_weekly_2026_2_3_2.R")
source("R/weekly_engine_24.R")

req24 <- c("output/weekly_2_4_promotion.csv", "output/weekly_2_4_locked_benchmark.csv",
           "output/weekly_2_4_selected_signal_model.csv", "output/weekly_2_4_residual_pool.csv")
for (pp in req24) if (!file.exists(pp)) stop("Missing 2.4 prerequisite: ", pp, ". Run 15_train_validate_signal_model_2_4.R first.")

promotion24 <- readr::read_csv("output/weekly_2_4_promotion.csv", show_col_types = FALSE, progress = FALSE)
locked24 <- readr::read_csv("output/weekly_2_4_locked_benchmark.csv", show_col_types = FALSE, progress = FALSE)
res24 <- readr::read_csv("output/weekly_2_4_residual_pool.csv", show_col_types = FALSE, progress = FALSE)
conf24 <- if (file.exists("output/weekly_2_4_confidence_calibration.csv")) tryCatch(readr::read_csv("output/weekly_2_4_confidence_calibration.csv", show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
talent24 <- readr::read_csv("data/processed/player_talent_profiles_2_4.csv", show_col_types = FALSE, progress = FALSE)
def24 <- if (file.exists(paste0("data/processed/defense_style_projection_", CURRENT_SEASON, "_2_4.csv"))) tryCatch(readr::read_csv(paste0("data/processed/defense_style_projection_", CURRENT_SEASON, "_2_4.csv"), show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
xfp24 <- if (file.exists("data/processed/expected_opportunity_history_2_4.csv")) tryCatch(readr::read_csv("data/processed/expected_opportunity_history_2_4.csv", show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
res23 <- if (file.exists("output/weekly_2_3_residual_pool.csv")) tryCatch(readr::read_csv("output/weekly_2_3_residual_pool.csv", show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
res232_locked <- if (file.exists("output/weekly_2_3_2_residual_pool.csv")) tryCatch(readr::read_csv("output/weekly_2_3_2_residual_pool.csv", show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()

assign_conf24 <- function(risk, pos) {
  z <- conf24[conf24$position == pos, , drop = FALSE]
  if (!nrow(z)) return(list(label = rep("Medium", length(risk)), expected = rep(NA_real_, length(risk))))
  q33 <- wk24_num(z$q33_risk[1]); q67 <- wk24_num(z$q67_risk[1])
  lab <- ifelse(risk <= q33, "High", ifelse(risk <= q67, "Medium", "Low"))
  exp <- vapply(lab, function(ll) {
    rr <- z[z$confidence == ll, , drop = FALSE]
    if (nrow(rr)) wk24_num(rr$expected_abs_error[1]) else NA_real_
  }, numeric(1))
  list(label = lab, expected = exp)
}

residual_for24 <- function(pos, promoted, locked_method) {
  if (promoted) rr <- res24 |> dplyr::filter(position == pos) |> dplyr::pull(residual)
  else if (identical(locked_method, "2.3.2") && nrow(res232_locked)) rr <- res232_locked |> dplyr::filter(position == pos) |> dplyr::pull(residual)
  else if (nrow(res23)) rr <- res23 |> dplyr::filter(position == pos) |> dplyr::pull(residual)
  else rr <- res24 |> dplyr::filter(position == pos) |> dplyr::pull(residual)
  rr <- suppressWarnings(as.numeric(rr)); rr[is.finite(rr)]
}

future24_rows <- list()
for (pos in POSITIONS) {
  d <- future_pred |> dplyr::filter(position == pos)
  if (!nrow(d)) next
  lockrow <- locked24[locked24$position == pos, , drop = FALSE]
  locked_method <- if (nrow(lockrow)) as.character(lockrow$locked_method_2026[1]) else "2.3"

  # 13_project preserves both versions before it applies the looser 2.3.2 gate.
  if (identical(locked_method, "2.3.2") && "projected_weekly_fppg_pre_availability_232" %in% names(d)) {
    locked_pre <- wk24_num(d$projected_weekly_fppg_pre_availability_232)
    locked_final <- if ("projected_weekly_fppg_232" %in% names(d)) wk24_num(d$projected_weekly_fppg_232) else locked_pre * wk24_num(d$availability_factor)
  } else {
    locked_method <- "2.3"
    locked_pre <- if ("projected_weekly_fppg_pre_availability_23" %in% names(d)) wk24_num(d$projected_weekly_fppg_pre_availability_23) else wk24_num(d$projected_weekly_fppg_pre_availability)
    locked_final <- if ("projected_weekly_fppg_23" %in% names(d)) wk24_num(d$projected_weekly_fppg_23) else locked_pre * wk24_num(d$availability_factor)
  }
  d$locked_method_24 <- locked_method
  d$locked_base_pre_availability_24 <- locked_pre
  d$locked_base_fppg_24 <- locked_pre
  d$locked_final_fppg_24 <- locked_final

  # New 2.4 context: player talent prior + current lagged opponent style +
  # optional lagged expected-opportunity history.
  d <- wk24_add_talent_features(d, talent24, live = TRUE)
  if (nrow(def24) && all(c("season", "week", "defense") %in% names(def24))) {
    if (!"season" %in% names(d)) d$season <- CURRENT_SEASON
    dk <- intersect(c("season", "week", "defense", MODEL24_DEFENSE_STYLE_FEATURES, "n_prior_games_24"), names(def24))
    d <- dplyr::left_join(d, def24[, dk, drop = FALSE], by = c("season", "week", "opponent" = "defense"))
  }
  if (nrow(xfp24) && all(c("season", "week", "player_id") %in% names(xfp24))) {
    if (!"season" %in% names(d)) d$season <- CURRENT_SEASON
    xx <- xfp24[as.integer(xfp24$season) == CURRENT_SEASON, , drop = FALSE]
    xk <- intersect(c("season", "week", "player_id", "xfp_roll3_24", "xfp_roll5_24", "fpoe_roll3_24", "fpoe_roll5_24"), names(xx))
    if (nrow(xx) && length(xk) >= 3) d <- dplyr::left_join(d, xx[, xk, drop = FALSE] |> dplyr::distinct(season, week, player_id, .keep_all = TRUE), by = c("season", "week", "player_id"))
  }
  d$archetype_24 <- wk24_archetype(d)
  d <- engineer_signal_features232(d, "locked_base_fppg_24")

  rsp_path <- paste0("models/weekly_2_4_response_", pos, ".rds")
  sig_path <- paste0("models/weekly_2_4_signal_", pos, ".rds")
  rsp <- if (file.exists(rsp_path)) readRDS(rsp_path) else list(maps = list())
  sig <- if (file.exists(sig_path)) readRDS(sig_path) else list(enabled = FALSE)
  d <- wk24_apply_response_map(d, rsp, pos)
  d$signal_correction_24 <- wk24_predict_signal(sig, d, pos)
  d$projected_weekly_fppg_pre_availability_24 <- pmax(0, d$locked_base_pre_availability_24 + d$signal_correction_24)
  d$projected_weekly_fppg_24 <- d$projected_weekly_fppg_pre_availability_24 * wk24_num(d$availability_factor)

  prow <- promotion24[promotion24$position == pos, , drop = FALSE]
  promote <- nrow(prow) > 0 && isTRUE(as.logical(prow$promoted_for_2026[1]))
  d$model24_promoted <- promote
  d$signal_model_enabled_24 <- isTRUE(sig$enabled)
  d$risk_score_24 <- wk24_num(d$model_disagreement_23) +
    0.70 * abs(wk24_num(d$signal_correction_24)) +
    0.30 * abs(wk24_num(d$response_adjustment_raw_24)) +
    0.25 * wk24_num(d$roll3_fppg_sd) +
    0.40 * wk24_num(d$talent_weight_24) * abs(wk24_num(d$talent_gap_weighted_24))

  unplayed <- d$is_actual == 0
  # First restore the strict locked benchmark. Only then may 2.4 overwrite it.
  d$projected_weekly_fppg_pre_availability[unplayed] <- d$locked_base_pre_availability_24[unplayed]
  d$projected_weekly_fppg[unplayed] <- d$locked_final_fppg_24[unplayed]
  if (promote) {
    d$projected_weekly_fppg_pre_availability[unplayed] <- d$projected_weekly_fppg_pre_availability_24[unplayed]
    d$projected_weekly_fppg[unplayed] <- d$projected_weekly_fppg_24[unplayed]
    cc <- assign_conf24(d$risk_score_24, pos)
    d$projection_confidence[unplayed] <- cc$label[unplayed]
    d$expected_abs_error[unplayed] <- cc$expected[unplayed]
  }

  rr <- residual_for24(pos, promote, locked_method)
  dist <- empirical_distribution22(d$projected_weekly_fppg, rr, pos)
  for (nm in names(dist)) d[[nm]] <- dist[[nm]]
  out_mask <- wk24_num(d$availability_factor) <= 0
  if (any(out_mask)) {
    d$weekly_median[out_mask] <- 0; d$weekly_floor[out_mask] <- 0; d$weekly_ceiling[out_mask] <- 0
    d$boom_probability[out_mask] <- 0; d$bust_probability[out_mask] <- 1
  }
  future24_rows[[length(future24_rows) + 1]] <- d
}

future_pred <- dplyr::bind_rows(future24_rows) |>
  dplyr::group_by(week, position) |> dplyr::arrange(dplyr::desc(projected_weekly_fppg), .by_group = TRUE) |>
  dplyr::mutate(weekly_position_rank = dplyr::row_number()) |> dplyr::ungroup() |>
  dplyr::arrange(week, dplyr::desc(projected_weekly_fppg))

add_topn24 <- function(d) {
  if (!nrow(d)) return(d)
  pos <- as.character(d$position[1]); cutoff <- as.numeric(WEEKLY_22_TOP_PROB_CUTOFF[[pos]])
  promote <- any(d$model24_promoted %in% TRUE)
  locked_method <- as.character(d$locked_method_24[1])
  rr <- residual_for24(pos, promote, locked_method)
  if (length(rr) < 30 || !is.finite(cutoff)) { d$topN_probability <- NA_real_; return(d) }
  draws <- min(1000L, WEEKLY_22_SIM_DRAWS); counts <- rep(0, nrow(d))
  set.seed(SEED + as.integer(d$week[1]) * 1000 + match(pos, POSITIONS) + 24)
  for (b in seq_len(draws)) {
    sim <- pmax(0, d$projected_weekly_fppg + sample(rr, nrow(d), replace = TRUE))
    counts <- counts + as.numeric(rank(-sim, ties.method = "min") <= cutoff)
  }
  d$topN_probability <- counts / draws
  d
}
future_pred <- dplyr::bind_rows(lapply(split(future_pred, interaction(future_pred$week, future_pred$position, drop = TRUE)), add_topn24)) |>
  dplyr::arrange(week, dplyr::desc(projected_weekly_fppg))

future_pred$season_ledger_points <- ifelse(future_pred$is_actual == 1, future_pred$actual_weekly_fppg, future_pred$projected_weekly_fppg)
readr::write_csv(future_pred, paste0("output/weekly_", CURRENT_SEASON, "_projections.csv"))

week_now <- future_pred |> dplyr::filter(week == current_week, is_actual == 0) |> dplyr::arrange(dplyr::desc(projected_weekly_fppg))
readr::write_csv(week_now, paste0("output/week_", current_week, "_rankings.csv"))

# Add the signal/talent decomposition to the prospective history. This is the
# evidence future versions can use after outcomes are observed.
hist_path24 <- paste0("output/projection_history_", CURRENT_SEASON, ".csv")
if (nrow(week_now)) {
  snap <- week_now |> dplyr::transmute(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), model_version = "2.4",
    week, player_id, player_display_name, position, team, opponent,
    projected_weekly_fppg, projected_weekly_fppg_24, locked_final_fppg_24, locked_method_24,
    signal_correction_24, response_adjustment_raw_24, talent_prior_fppg_24, talent_weight_24,
    talent_gap_weighted_24, archetype_24, model24_promoted, risk_score_24,
    weekly_floor, weekly_ceiling, projection_confidence, expected_abs_error,
    calibrated_matchup_delta_23, model_disagreement_23)
  old <- if (file.exists(hist_path24)) tryCatch(readr::read_csv(hist_path24, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()

  # History files span multiple model generations. readr can infer a column such
  # as model_version as numeric when all prior values look like 2.3/2.4, while
  # the new snapshot intentionally stores versions as labels. Normalize the
  # stable identity/label fields before row-binding so an old CSV can never
  # break a new live projection run solely because of type inference.
  history_character_cols24 <- c(
    "generated_at", "model_version", "player_id", "player_display_name",
    "position", "team", "opponent", "projection_confidence",
    "locked_method_24"
  )
  for (nm in intersect(history_character_cols24, names(old))) old[[nm]] <- as.character(old[[nm]])
  for (nm in intersect(history_character_cols24, names(snap))) snap[[nm]] <- as.character(snap[[nm]])

  readr::write_csv(dplyr::bind_rows(old, snap), hist_path24)
}

ros <- future_pred |>
  dplyr::group_by(player_id, player_display_name, position, team) |>
  dplyr::summarise(actual_points_to_date = sum(dplyr::if_else(is_actual == 1, actual_weekly_fppg, 0), na.rm = TRUE),
    projected_remaining_points = sum(dplyr::if_else(is_actual == 0, projected_weekly_fppg, 0), na.rm = TRUE),
    projected_full_season_points = sum(season_ledger_points, na.rm = TRUE), remaining_games = sum(is_actual == 0), .groups = "drop") |>
  dplyr::group_by(position) |> dplyr::arrange(dplyr::desc(projected_full_season_points), .by_group = TRUE) |>
  dplyr::mutate(ros_position_rank = dplyr::row_number()) |> dplyr::ungroup() |> dplyr::arrange(dplyr::desc(projected_full_season_points))
readr::write_csv(ros, paste0("output/rest_of_season_", CURRENT_SEASON, ".csv"))

cat("\n[2.4 LIVE] Strict locked benchmark restored, then 2.4 applied only where it earned promotion.\n")
cat("[2.4 LIVE] Current week: ", current_week, " | ranked players: ", nrow(week_now), "\n", sep = "")
print(promotion24 |> dplyr::select(position, promoted_for_2026, locked_MAE, model24_MAE, locked_RMSE, model24_RMSE, locked_correlation, model24_correlation))
