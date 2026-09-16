# ---- Model 2.5 project-root bootstrap ---------------------------------------
.fm25_project_root <- function() {
  script_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  candidates <- character()
  if (!is.null(script_path) && length(script_path) == 1 && nzchar(script_path)) {
    p <- normalizePath(script_path, winslash = "/", mustWork = FALSE)
    d <- dirname(p)
    candidates <- c(candidates, d, dirname(d))
  }
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  p <- wd
  for (i in 0:6) {
    candidates <- c(candidates, p)
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  candidates <- unique(candidates[nzchar(candidates)])
  ok <- vapply(candidates, function(x) {
    file.exists(file.path(x, "config.R")) &&
      dir.exists(file.path(x, "R")) &&
      dir.exists(file.path(x, "pipeline"))
  }, logical(1))
  hits <- candidates[ok]
  if (!length(hits)) {
    stop(
      "Could not locate the Fantasy Model project root. Expected a folder containing config.R, R/, and pipeline/. ",
      "Current working directory: ", getwd()
    )
  }
  project_root <- normalizePath(hits[[1]], winslash = "/", mustWork = TRUE)
  if (!identical(normalizePath(getwd(), winslash = "/", mustWork = TRUE), project_root)) {
    setwd(project_root)
  }
  cat("[2.5] Project root: ", project_root, "\n", sep = "")
  invisible(project_root)
}
.fm25_project_root()
source("config.R")
# -----------------------------------------------------------------------------

# ============================================================
# FANTASY MODEL 2.5 - GUARDED LIVE 2026 PROJECTION
# ============================================================
# Rebuild the current 2.4 *state grid* first, then allow the 2.5 direct
# challenger to score ONLY player-weeks that have not been played. Completed
# player-weeks are historical ledger rows: they are finalized once, then frozen.
# This matters during partial NFL weeks too (for example TNF completed while
# Sunday/Monday players remain prospective).

# Preserve already-finalized rows before the upstream state-grid refresh. The
# upstream pipeline may reconstruct its internal grid so it can ingest newly
# completed games, but 2.5 never re-predicts or overwrites a row that was
# already marked actual in the prior production file.
.weekly_path25 <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
.frozen_completed25 <- if (file.exists(.weekly_path25)) {
  tryCatch({
    z <- readr::read_csv(.weekly_path25, show_col_types = FALSE, progress = FALSE)
    if (nrow(z) && "is_actual" %in% names(z)) z[suppressWarnings(as.numeric(z$is_actual)) == 1, , drop = FALSE] else data.frame()
  }, error = function(e) data.frame())
} else data.frame()

source("pipeline/16_project_weekly_2026_2_4.R")
ensure_packages(c("dplyr", "readr", "xgboost"))
source("R/weekly_engine_25.R")
source("R/live_error_feedback_engine_25.R")

# Restore rows that were already finalized in the previous production file.
# Newly completed games are intentionally allowed through once so their actual
# result can be ingested; on the following refresh they join this frozen set.
if (nrow(.frozen_completed25) && exists("future_pred") && nrow(future_pred) &&
    all(c("week", "player_id") %in% names(.frozen_completed25)) &&
    all(c("week", "player_id") %in% names(future_pred))) {
  .frozen_keys25 <- paste(.frozen_completed25$week, .frozen_completed25$player_id, sep = "::")
  .grid_keys25 <- paste(future_pred$week, future_pred$player_id, sep = "::")
  .keep_grid25 <- !(.grid_keys25 %in% .frozen_keys25)

  # The refreshed schedule may return `gameday` as Date while the prior weekly
  # CSV is read back as character. Frozen rows and refreshed rows represent the
  # same schema, so normalize date/time columns before restoring the immutable
  # completed ledger. This is intentionally generic so future nflreadr type
  # changes do not break bind_rows() on another date/time field.
  .grid_unplayed25 <- future_pred[.keep_grid25, , drop = FALSE]
  .common_cols25 <- intersect(names(.grid_unplayed25), names(.frozen_completed25))
  for (.nm25 in .common_cols25) {
    .grid_is_dt25 <- inherits(.grid_unplayed25[[.nm25]], "Date") || inherits(.grid_unplayed25[[.nm25]], "POSIXt")
    .frozen_is_dt25 <- inherits(.frozen_completed25[[.nm25]], "Date") || inherits(.frozen_completed25[[.nm25]], "POSIXt")
    if (.grid_is_dt25 || .frozen_is_dt25) {
      .grid_unplayed25[[.nm25]] <- as.character(.grid_unplayed25[[.nm25]])
      .frozen_completed25[[.nm25]] <- as.character(.frozen_completed25[[.nm25]])
    }
  }
  # `gameday` is the known nflreadr/CSV boundary case. Keep it character even
  # if neither side is currently typed as Date so subsequent writes are stable.
  if ("gameday" %in% names(.grid_unplayed25)) .grid_unplayed25$gameday <- as.character(.grid_unplayed25$gameday)
  if ("gameday" %in% names(.frozen_completed25)) .frozen_completed25$gameday <- as.character(.frozen_completed25$gameday)

  future_pred <- dplyr::bind_rows(.grid_unplayed25, .frozen_completed25) |>
    dplyr::arrange(week, dplyr::desc(projected_weekly_fppg))
  cat("[2.5 LIVE] Frozen completed rows restored: ", nrow(.frozen_completed25), "\n", sep = "")
}

req25 <- c(
  "output/weekly_2_5_promotion.csv",
  "output/weekly_2_5_champion_manifest.csv",
  "output/weekly_2_5_residual_pool.csv"
)
miss <- req25[!file.exists(req25)]
if (length(miss)) stop("Missing 2.5 validation artifact(s): ", paste(miss, collapse = ", "), ". Run pipeline/22_train_validate_accuracy_tournament_2_5.R first.")

promotion25 <- readr::read_csv("output/weekly_2_5_promotion.csv", show_col_types = FALSE, progress = FALSE)
manifest25 <- readr::read_csv("output/weekly_2_5_champion_manifest.csv", show_col_types = FALSE, progress = FALSE)
res25 <- readr::read_csv("output/weekly_2_5_residual_pool.csv", show_col_types = FALSE, progress = FALSE)

pred25_rows <- list()
for (pos in POSITIONS) {
  d <- future_pred |> dplyr::filter(position == pos)
  if (!nrow(d)) next
  pr <- promotion25[promotion25$position == pos, , drop = FALSE]
  mf <- manifest25[manifest25$position == pos, , drop = FALSE]
  promote <- nrow(pr) && isTRUE(wk25_bool(pr$promoted_for_2026[1]))
  alpha <- if (nrow(mf) && is.finite(as.numeric(mf$final_alpha_2026[1]))) as.numeric(mf$final_alpha_2026[1]) else 0

  model_path <- paste0("models/weekly_2_5_direct_", pos, ".json")
  meta_path <- paste0("models/weekly_2_5_direct_", pos, "_meta.rds")
  if (!file.exists(model_path) || !file.exists(meta_path)) stop("Missing final 2.5 direct model for ", pos)
  model <- xgboost::xgb.load(model_path)
  meta <- readRDS(meta_path)
  obj <- list(model = model, features = meta$features)
  unplayed <- if ("is_actual" %in% names(d)) wk25_num(d$is_actual) == 0 else rep(TRUE, nrow(d))

  # Forward-only scoring: never call the 2.5 learner on completed player-weeks.
  # Completed rows remain an immutable actual/history ledger.
  d$direct_fppg_25 <- NA_real_
  d$blend_alpha_25 <- alpha
  d$model25_promoted <- promote
  d$incumbent_pre_availability_25 <- NA_real_
  d$candidate_pre_availability_25 <- NA_real_
  d$incumbent_fppg_25 <- NA_real_
  d$candidate_fppg_25 <- NA_real_
  d$direct_gap_25 <- NA_real_
  avail <- if ("availability_factor" %in% names(d)) wk25_num(d$availability_factor, 1) else rep(1, nrow(d))

  if (any(unplayed)) {
    du <- d[unplayed, , drop = FALSE]
    direct_u <- wk25_predict_direct(obj, du)
    incumbent_pre_u <- if ("projected_weekly_fppg_pre_availability" %in% names(du)) wk25_num(du$projected_weekly_fppg_pre_availability) else wk25_num(du$projected_weekly_fppg)
    candidate_pre_u <- pmax(0, incumbent_pre_u + alpha * (direct_u - incumbent_pre_u))
    avail_u <- avail[unplayed]

    d$direct_fppg_25[unplayed] <- direct_u
    d$incumbent_pre_availability_25[unplayed] <- incumbent_pre_u
    d$candidate_pre_availability_25[unplayed] <- candidate_pre_u
    # Freeze the exact incumbent production forecast BEFORE the 2.5 blend so
    # live scoring can tell us whether Model 2.5 beats what it replaced.
    d$incumbent_fppg_25[unplayed] <- incumbent_pre_u * avail_u
    d$candidate_fppg_25[unplayed] <- candidate_pre_u * avail_u
    d$direct_gap_25[unplayed] <- direct_u - incumbent_pre_u

    if (promote) {
      d$projected_weekly_fppg_pre_availability[unplayed] <- candidate_pre_u
      d$projected_weekly_fppg[unplayed] <- candidate_pre_u * avail_u

      rr <- res25$residual[res25$position == pos]
      rr <- wk25_num(rr, NA_real_); rr <- rr[is.finite(rr)]
      if (length(rr) >= 30) {
        dist_u <- empirical_distribution22(d$projected_weekly_fppg[unplayed], rr, pos)
        for (nm in names(dist_u)) {
          if (!nm %in% names(d)) d[[nm]] <- NA_real_
          d[[nm]][unplayed] <- dist_u[[nm]]
        }
        out_mask <- unplayed & avail <= 0
        if (any(out_mask)) {
          d$weekly_median[out_mask] <- 0; d$weekly_floor[out_mask] <- 0; d$weekly_ceiling[out_mask] <- 0
          d$boom_probability[out_mask] <- 0; d$bust_probability[out_mask] <- 1
        }
      }
    }
  }
  pred25_rows[[length(pred25_rows) + 1]] <- d
}

future_pred <- dplyr::bind_rows(pred25_rows)

# -------------------------------------------------------------------------
# Final-forecast error feedback (optional, honest-validation gated)
# -------------------------------------------------------------------------
# The upstream 2.3.2 controller already uses prior errors from its own forecast.
# This layer is different: it learns from errors of the FINAL Model 2.5
# production forecast that users actually saw. Only completed prior
# player-weeks are eligible, and only positions whose feedback layer passed its
# own chronological validation are adjusted.
.feedback_scored25_path <- paste0("output/live_accuracy_player_weeks_", CURRENT_SEASON, ".csv")
.feedback_errors25 <- if (file.exists(.feedback_scored25_path)) {
  tryCatch(readr::read_csv(.feedback_scored25_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
} else data.frame()
if (nrow(.feedback_errors25)) {
  if (!"season" %in% names(.feedback_errors25)) .feedback_errors25$season <- CURRENT_SEASON
  .feedback_errors25 <- .feedback_errors25 |>
    dplyr::transmute(
      season = as.integer(fb25_num(season, CURRENT_SEASON)),
      week = as.integer(fb25_num(week)),
      player_id = fb25_chr(player_id),
      position = fb25_chr(position),
      error = fb25_num(error)
    ) |>
    dplyr::filter(position %in% POSITIONS, nzchar(player_id), is.finite(week), is.finite(error))
}

.feedback_rows25 <- list()
for (.pos25 in POSITIONS) {
  .d25 <- future_pred |> dplyr::filter(position == .pos25)
  if (!nrow(.d25)) next
  if (!"season" %in% names(.d25)) .d25$season <- CURRENT_SEASON
  .d25$feedback_base_projection_25 <- wk25_num(.d25$projected_weekly_fppg)
  .d25$feedback_player_gain_25 <- 0
  .d25$feedback_position_gain_25 <- 0
  .d25$feedback_promoted_25 <- FALSE
  .d25$feedback_player_bias_25 <- 0
  .d25$feedback_position_bias_25 <- 0
  .d25$feedback_raw_correction_25 <- 0
  .d25$feedback_horizon_decay_25 <- 1
  .d25$feedback_correction_25 <- 0
  .d25$feedback_post_projection_25 <- .d25$feedback_base_projection_25
  for (.state_nm25 in c(
    "player_error_n", "player_last_error", "player_error_ewma", "player_error_roll3",
    "player_abs_error_roll3", "player_error_trend", "position_error_n",
    "position_error_bias", "position_abs_error"
  )) {
    if (!.state_nm25 %in% names(.d25)) .d25[[.state_nm25]] <- NA_real_
  }

  .fb_model_path25 <- paste0("models/weekly_2_5_error_feedback_", .pos25, ".rds")
  .fb_obj25 <- if (file.exists(.fb_model_path25)) tryCatch(readRDS(.fb_model_path25), error = function(e) NULL) else NULL
  .fb_promote25 <- !is.null(.fb_obj25) && isTRUE(.fb_obj25$promoted_for_2026)
  .unplayed_fb25 <- wk25_num(.d25$is_actual, 0) == 0

  if (.fb_promote25 && any(.unplayed_fb25) && nrow(.feedback_errors25)) {
    .du25 <- .d25[.unplayed_fb25, , drop = FALSE]
    .du25 <- fb25_build_features(.du25, .feedback_errors25)
    .du25$feedback_base_projection_25 <- wk25_num(.du25$projected_weekly_fppg)
    .du25 <- fb25_apply_correction(
      .du25,
      player_gain = as.numeric(.fb_obj25$player_gain),
      position_gain = as.numeric(.fb_obj25$position_gain),
      current_week = current_week
    )
    .d25$feedback_player_gain_25[.unplayed_fb25] <- as.numeric(.fb_obj25$player_gain)
    .d25$feedback_position_gain_25[.unplayed_fb25] <- as.numeric(.fb_obj25$position_gain)
    .d25$feedback_promoted_25[.unplayed_fb25] <- TRUE
    for (.nm25 in c(
      "player_error_n", "player_last_error", "player_error_ewma", "player_error_roll3",
      "player_abs_error_roll3", "player_error_trend", "position_error_n",
      "position_error_bias", "position_abs_error", "feedback_player_bias_25",
      "feedback_position_bias_25", "feedback_raw_correction_25",
      "feedback_horizon_decay_25", "feedback_correction_25", "feedback_post_projection_25"
    )) {
      if (!.nm25 %in% names(.d25)) .d25[[.nm25]] <- NA_real_
      .d25[[.nm25]][.unplayed_fb25] <- .du25[[.nm25]]
    }

    # Point correction is applied on the final fantasy-point scale and then
    # reflected back to pre-availability only when that inverse is well-defined.
    .d25$projected_weekly_fppg[.unplayed_fb25] <- pmax(0, .du25$feedback_post_projection_25)
    .av25 <- if ("availability_factor" %in% names(.d25)) wk25_num(.d25$availability_factor, 1) else rep(1, nrow(.d25))
    .inv25 <- .unplayed_fb25 & .av25 > 0
    if ("projected_weekly_fppg_pre_availability" %in% names(.d25) && any(.inv25)) {
      .d25$projected_weekly_fppg_pre_availability[.inv25] <- .d25$projected_weekly_fppg[.inv25] / .av25[.inv25]
    }

    # Error itself is information about uncertainty too. Blend the historical
    # calibrated expected error with recent realized absolute error rather than
    # pretending confidence is static throughout the season.
    if ("expected_abs_error" %in% names(.d25)) {
      .recent_abs25 <- dplyr::coalesce(
        wk25_num(.d25$player_abs_error_roll3, NA_real_),
        wk25_num(.d25$position_abs_error, NA_real_)
      )
      .has_abs25 <- .unplayed_fb25 & is.finite(.recent_abs25) & .recent_abs25 > 0
      .base_e25 <- wk25_num(.d25$expected_abs_error, NA_real_)
      .d25$expected_abs_error[.has_abs25] <- ifelse(
        is.finite(.base_e25[.has_abs25]),
        0.70 * .base_e25[.has_abs25] + 0.30 * .recent_abs25[.has_abs25],
        .recent_abs25[.has_abs25]
      )
    }
    cat("[2.5 FEEDBACK] ", .pos25, " final-forecast feedback applied to ",
        sum(.unplayed_fb25), " unplayed rows; gains P=",
        .fb_obj25$player_gain, ", Pos=", .fb_obj25$position_gain, ".\n", sep = "")
  }
  .feedback_rows25[[length(.feedback_rows25) + 1L]] <- .d25
}
future_pred <- dplyr::bind_rows(.feedback_rows25)

# If final-error feedback moved the point forecast, recenter the empirical
# distribution around that final forecast for unplayed rows only.
.recenter_feedback25 <- function(d) {
  if (!nrow(d)) return(d)
  .pos <- as.character(d$position[1])
  .idx <- wk25_num(d$is_actual, 0) == 0 & wk25_bool(d$feedback_promoted_25)
  if (!any(.idx)) return(d)
  .rr <- wk25_num(res25$residual[res25$position == .pos], NA_real_)
  .rr <- .rr[is.finite(.rr)]
  if (length(.rr) < 30) return(d)
  .dist <- empirical_distribution22(d$projected_weekly_fppg[.idx], .rr, .pos)
  for (.nm in names(.dist)) {
    if (!.nm %in% names(d)) d[[.nm]] <- NA_real_
    d[[.nm]][.idx] <- .dist[[.nm]]
  }
  .avail <- if ("availability_factor" %in% names(d)) wk25_num(d$availability_factor, 1) else rep(1, nrow(d))
  .out <- .idx & .avail <= 0
  if (any(.out)) {
    d$weekly_median[.out] <- 0; d$weekly_floor[.out] <- 0; d$weekly_ceiling[.out] <- 0
    d$boom_probability[.out] <- 0; d$bust_probability[.out] <- 1
  }
  d
}
future_pred <- dplyr::bind_rows(lapply(split(future_pred, interaction(future_pred$week, future_pred$position, drop = TRUE)), .recenter_feedback25))

# Rank only prospective rows. Historical rows retain their frozen historical rank
# when one exists; this avoids "reranking" a completed Thursday player after
# Sunday/Monday information arrives.
.rank_unplayed25 <- function(d) {
  if (!"weekly_position_rank" %in% names(d)) d$weekly_position_rank <- NA_real_
  idx <- wk25_num(d$is_actual, 0) == 0
  if (any(idx)) d$weekly_position_rank[idx] <- rank(-wk25_num(d$projected_weekly_fppg[idx]), ties.method = "min", na.last = "keep")
  d
}
future_pred <- dplyr::bind_rows(lapply(split(future_pred, interaction(future_pred$week, future_pred$position, drop = TRUE)), .rank_unplayed25)) |>
  dplyr::arrange(week, dplyr::desc(projected_weekly_fppg))

# Recompute top-N probability only for promoted positions using honest 2.5 residuals.
add_topn25 <- function(d) {
  if (!nrow(d)) return(d)
  pos <- as.character(d$position[1])
  promote <- any(wk25_bool(d$model25_promoted))
  idx <- wk25_num(d$is_actual, 0) == 0
  if (!promote || !any(idx)) return(d)
  cutoff <- as.numeric(WEEKLY_22_TOP_PROB_CUTOFF[[pos]])
  rr <- wk25_num(res25$residual[res25$position == pos], NA_real_); rr <- rr[is.finite(rr)]
  if (length(rr) < 30 || !is.finite(cutoff)) return(d)
  draws <- min(1000L, WEEKLY_22_SIM_DRAWS); counts <- rep(0, sum(idx))
  base <- wk25_num(d$projected_weekly_fppg[idx])
  set.seed(SEED + as.integer(d$week[1]) * 1000 + match(pos, POSITIONS) + 25)
  for (b in seq_len(draws)) {
    sim <- pmax(0, base + sample(rr, length(base), replace = TRUE))
    counts <- counts + as.numeric(rank(-sim, ties.method = "min") <= cutoff)
  }
  if (!"topN_probability" %in% names(d)) d$topN_probability <- NA_real_
  d$topN_probability[idx] <- counts / draws
  d

}
future_pred <- dplyr::bind_rows(lapply(split(future_pred, interaction(future_pred$week, future_pred$position, drop = TRUE)), add_topn25)) |>
  dplyr::arrange(week, dplyr::desc(projected_weekly_fppg))

future_pred$season_ledger_points <- ifelse(wk25_num(future_pred$is_actual) == 1, wk25_num(future_pred$actual_weekly_fppg), wk25_num(future_pred$projected_weekly_fppg))
readr::write_csv(future_pred, paste0("output/weekly_", CURRENT_SEASON, "_projections.csv"))

week_now <- future_pred |> dplyr::filter(week == current_week, is_actual == 0) |> dplyr::arrange(dplyr::desc(projected_weekly_fppg))
readr::write_csv(week_now, paste0("output/week_", current_week, "_rankings.csv"))

ros <- future_pred |>
  dplyr::group_by(player_id, player_display_name, position, team) |>
  dplyr::summarise(
    actual_points_to_date = sum(dplyr::if_else(is_actual == 1, actual_weekly_fppg, 0), na.rm = TRUE),
    projected_remaining_points = sum(dplyr::if_else(is_actual == 0, projected_weekly_fppg, 0), na.rm = TRUE),
    projected_full_season_points = sum(season_ledger_points, na.rm = TRUE),
    remaining_games = sum(is_actual == 0), .groups = "drop"
  ) |>
  dplyr::group_by(position) |>
  dplyr::arrange(dplyr::desc(projected_full_season_points), .by_group = TRUE) |>
  dplyr::mutate(ros_position_rank = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(projected_full_season_points))
readr::write_csv(ros, paste0("output/rest_of_season_", CURRENT_SEASON, ".csv"))

# Append an auditable snapshot of the new champion/challenger decomposition.
hist_path25 <- paste0("output/projection_history_", CURRENT_SEASON, ".csv")
if (nrow(week_now)) {
  snap <- week_now |> dplyr::transmute(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), model_version = "2.5",
    week, player_id, player_display_name, position, team, opponent,
    projected_weekly_fppg, direct_fppg_25, incumbent_pre_availability_25,
    candidate_pre_availability_25, blend_alpha_25, direct_gap_25, model25_promoted,
    feedback_base_projection_25, feedback_promoted_25, feedback_player_gain_25,
    feedback_position_gain_25, player_error_n, player_last_error, player_error_ewma,
    player_error_roll3, player_abs_error_roll3, player_error_trend,
    position_error_n, position_error_bias, position_abs_error,
    feedback_player_bias_25, feedback_position_bias_25, feedback_correction_25,
    feedback_horizon_decay_25, feedback_post_projection_25,
    weekly_floor, weekly_ceiling, projection_confidence, expected_abs_error
  )
  old <- if (file.exists(hist_path25)) tryCatch(readr::read_csv(hist_path25, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()

  # Projection history is intentionally shared across model generations.
  # Normalize label/identity columns before bind_rows() because readr may infer
  # model_version as numeric from older files (for example 2.3) while 2.5 writes
  # the version as a character label.
  history_character_cols25 <- c(
    "generated_at", "model_version", "player_id", "player_display_name",
    "position", "team", "opponent", "projection_confidence",
    "locked_method_24"
  )
  for (nm in intersect(history_character_cols25, names(old))) old[[nm]] <- as.character(old[[nm]])
  for (nm in intersect(history_character_cols25, names(snap))) snap[[nm]] <- as.character(snap[[nm]])

  readr::write_csv(dplyr::bind_rows(old, snap), hist_path25)
}

cat("\n[2.5 LIVE] Guarded Model 2.5 projection complete.\n")
print(manifest25 |> dplyr::select(position, incumbent, challenger, promoted_for_2026, final_alpha_2026, incumbent_starter_MAE, challenger_starter_MAE, incumbent_starter_RMSE, challenger_starter_RMSE))
cat("[2.5 LIVE] Forward-only refresh complete: completed player-weeks frozen; only unplayed rows + ROS were updated.\n")
