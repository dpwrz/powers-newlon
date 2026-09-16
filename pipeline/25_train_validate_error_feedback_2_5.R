# ============================================================
# FANTASY MODEL 2.5 - FINAL-FORECAST ERROR FEEDBACK TOURNAMENT
# ============================================================
# Learns whether PRIOR completed-week errors add incremental value after the
# validated Model 2.5 production forecast. All state is lagged; a target week
# never sees its own outcome or any other outcome from that target week.

source("config.R")
ensure_packages(c("dplyr", "readr"))
source("R/weekly_engine_25.R")
source("R/live_error_feedback_engine_25.R")

req <- c(
  "output/weekly_2_5_validation_predictions.csv",
  "output/weekly_2_5_champion_manifest.csv"
)
miss <- req[!file.exists(req)]
if (length(miss)) stop("Missing 2.5 validation artifact(s): ", paste(miss, collapse = ", "), ". Run the 2.5 accuracy tournament first.")

val <- readr::read_csv(req[1], show_col_types = FALSE, progress = FALSE)
manifest <- readr::read_csv(req[2], show_col_types = FALSE, progress = FALSE)
for (nm in c("season", "week", "player_id", "position", "actual_fppg", "production_base_25", "candidate_fppg_25")) {
  if (!nm %in% names(val)) stop("2.5 feedback validation requires column: ", nm)
}

# Use the exact position-level production choice that would have been deployed.
promo_map <- setNames(fb25_bool(manifest$promoted_for_2026), as.character(manifest$position))
val$feedback_base_projection_25 <- ifelse(
  promo_map[as.character(val$position)] %in% TRUE,
  fb25_num(val$candidate_fppg_25),
  fb25_num(val$production_base_25)
)
val <- val |>
  dplyr::filter(position %in% POSITIONS, is.finite(fb25_num(actual_fppg)), is.finite(fb25_num(feedback_base_projection_25))) |>
  dplyr::arrange(position, season, week, player_id)

build_lagged <- function(d) {
  out <- list(); errors <- data.frame()
  keys <- unique(paste(as.integer(d$season), as.integer(d$week), sep = "::"))
  for (key in keys) {
    sp <- strsplit(key, "::", fixed = TRUE)[[1]]
    yy <- as.integer(sp[1]); ww <- as.integer(sp[2])
    cur <- d[as.integer(d$season) == yy & as.integer(d$week) == ww, , drop = FALSE]
    cur <- fb25_build_features(cur, errors)
    out[[length(out) + 1L]] <- cur
    # Only after every target row in the week has been featurized do those
    # outcomes become available to the next week.
    err <- cur |>
      dplyr::transmute(
        season = as.integer(season), week = as.integer(week),
        player_id = fb25_chr(player_id), position = fb25_chr(position),
        error = fb25_num(actual_fppg) - fb25_num(feedback_base_projection_25)
      )
    errors <- dplyr::bind_rows(errors, err)
  }
  dplyr::bind_rows(out)
}

feature_rows <- list()
for (pos in POSITIONS) {
  d <- val |> dplyr::filter(position == pos) |> dplyr::arrange(season, week, player_id)
  if (!nrow(d)) next
  feature_rows[[length(feature_rows) + 1L]] <- build_lagged(d)
}
feat <- dplyr::bind_rows(feature_rows) |> dplyr::arrange(position, season, week, player_id)

# Honest year-by-year gain selection: target season gains are selected only
# from prior OOF seasons.
pred_rows <- list(); gain_rows <- list()
for (pos in POSITIONS) {
  d <- feat |> dplyr::filter(position == pos)
  years <- sort(unique(as.integer(d$season)))
  for (yy in years) {
    prior <- d |> dplyr::filter(as.integer(season) < yy)
    cur <- d |> dplyr::filter(as.integer(season) == yy)
    gains <- fb25_select_gains(prior)
    cur <- fb25_apply_correction(cur, gains[["player_gain"]], gains[["position_gain"]], current_week = NULL)
    cur$feedback_player_gain_25 <- gains[["player_gain"]]
    cur$feedback_position_gain_25 <- gains[["position_gain"]]
    pred_rows[[length(pred_rows) + 1L]] <- cur
    gain_rows[[length(gain_rows) + 1L]] <- data.frame(
      position = pos, target_year = yy,
      prior_years = length(unique(as.integer(prior$season))),
      player_gain = gains[["player_gain"]], position_gain = gains[["position_gain"]],
      stringsAsFactors = FALSE
    )
  }
}
oof <- dplyr::bind_rows(pred_rows) |> dplyr::arrange(position, season, week, player_id)
readr::write_csv(oof, "output/weekly_2_5_feedback_validation_predictions.csv")
readr::write_csv(dplyr::bind_rows(gain_rows), "output/weekly_2_5_feedback_gain_schedule.csv")

metric_one <- function(d, mask, label) {
  d <- d[mask, , drop = FALSE]
  if (!nrow(d)) return(data.frame(cohort = label, n = 0L, base_MAE = NA_real_, candidate_MAE = NA_real_, base_RMSE = NA_real_, candidate_RMSE = NA_real_))
  a <- fb25_num(d$actual_fppg); b <- fb25_num(d$feedback_base_projection_25); c <- fb25_num(d$feedback_post_projection_25)
  data.frame(
    cohort = label, n = nrow(d),
    base_MAE = mean(abs(a - b)), candidate_MAE = mean(abs(a - c)),
    base_RMSE = sqrt(mean((a - b)^2)), candidate_RMSE = sqrt(mean((a - c)^2))
  )
}

metrics <- list(); promotions <- list(); finals <- list(); by_year <- list()
for (pos in POSITIONS) {
  d <- oof |> dplyr::filter(position == pos)
  if (!nrow(d)) next
  allm <- metric_one(d, rep(TRUE, nrow(d)), "All")
  relm <- metric_one(d, if ("relevant_cohort" %in% names(d)) fb25_bool(d$relevant_cohort) else rep(TRUE, nrow(d)), "Relevant")
  stam <- metric_one(d, if ("starter_cohort" %in% names(d)) fb25_bool(d$starter_cohort) else rep(FALSE, nrow(d)), "Starter")
  mm <- dplyr::bind_rows(allm, relm, stam) |>
    dplyr::mutate(
      position = pos,
      MAE_improvement_pct = 100 * (base_MAE - candidate_MAE) / pmax(base_MAE, 1e-9),
      RMSE_improvement_pct = 100 * (base_RMSE - candidate_RMSE) / pmax(base_RMSE, 1e-9)
    ) |>
    dplyr::select(position, dplyr::everything())
  metrics[[length(metrics) + 1L]] <- mm

  yr <- dplyr::bind_rows(lapply(sort(unique(as.integer(d$season))), function(yy) {
    z <- d |> dplyr::filter(as.integer(season) == yy)
    s <- metric_one(z, if ("starter_cohort" %in% names(z)) fb25_bool(z$starter_cohort) else rep(FALSE, nrow(z)), "Starter")
    s$season <- yy; s$position <- pos
    s$MAE_improvement_pct <- 100 * (s$base_MAE - s$candidate_MAE) / pmax(s$base_MAE, 1e-9)
    s$RMSE_improvement_pct <- 100 * (s$base_RMSE - s$candidate_RMSE) / pmax(s$base_RMSE, 1e-9)
    s
  }))
  by_year[[length(by_year) + 1L]] <- yr

  a <- mm[mm$cohort == "All", , drop = FALSE]
  r <- mm[mm$cohort == "Relevant", , drop = FALSE]
  s <- mm[mm$cohort == "Starter", , drop = FALSE]
  # Ignore the first bootstrap year (no prior seasons => gains are forced to 0)
  # when assessing year consistency.
  yr_eval <- yr[yr$season > min(yr$season), , drop = FALSE]
  mae_wins <- sum(yr_eval$MAE_improvement_pct > 0, na.rm = TRUE)
  rmse_wins <- sum(yr_eval$RMSE_improvement_pct >= 0, na.rm = TRUE)
  need_wins <- if (nrow(yr_eval) >= 3) 2 else max(1, nrow(yr_eval))
  checks <- c(
    all_MAE = nrow(a) && a$MAE_improvement_pct[1] > 0,
    all_RMSE = nrow(a) && a$RMSE_improvement_pct[1] >= 0,
    relevant_MAE = nrow(r) && r$MAE_improvement_pct[1] >= 0,
    relevant_RMSE = nrow(r) && r$RMSE_improvement_pct[1] >= 0,
    starter_MAE = nrow(s) && s$MAE_improvement_pct[1] > 0,
    starter_RMSE = nrow(s) && s$RMSE_improvement_pct[1] >= 0,
    year_MAE_stability = mae_wins >= need_wins,
    year_RMSE_stability = rmse_wins >= need_wins
  )
  final_gain <- fb25_select_gains(d)
  promoted <- all(checks) && (final_gain[["player_gain"]] > 0 || final_gain[["position_gain"]] > 0)
  promotions[[length(promotions) + 1L]] <- data.frame(
    position = pos, promoted_for_2026 = promoted,
    player_gain_2026 = final_gain[["player_gain"]], position_gain_2026 = final_gain[["position_gain"]],
    starter_MAE_gain_pct = if (nrow(s)) s$MAE_improvement_pct[1] else NA_real_,
    starter_RMSE_gain_pct = if (nrow(s)) s$RMSE_improvement_pct[1] else NA_real_,
    relevant_MAE_gain_pct = if (nrow(r)) r$MAE_improvement_pct[1] else NA_real_,
    all_MAE_gain_pct = if (nrow(a)) a$MAE_improvement_pct[1] else NA_real_,
    failed_checks = paste(names(checks)[!checks], collapse = ";"),
    stringsAsFactors = FALSE
  )
  saveRDS(list(
    position = pos, promoted_for_2026 = promoted,
    player_gain = final_gain[["player_gain"]], position_gain = final_gain[["position_gain"]],
    horizon_decay = FEEDBACK25_HORIZON_DECAY, cap = unname(FEEDBACK25_CAP[[pos]])
  ), paste0("models/weekly_2_5_error_feedback_", pos, ".rds"))
}

readr::write_csv(dplyr::bind_rows(metrics), "output/weekly_2_5_feedback_metrics.csv")
readr::write_csv(dplyr::bind_rows(by_year), "output/weekly_2_5_feedback_year_metrics.csv")
readr::write_csv(dplyr::bind_rows(promotions), "output/weekly_2_5_feedback_promotion.csv")

cat("\n[2.5 FEEDBACK] Validation complete. Final 2.5 errors are used only if they improve honest OOF starter/relevant accuracy.\n")
print(dplyr::bind_rows(promotions))
