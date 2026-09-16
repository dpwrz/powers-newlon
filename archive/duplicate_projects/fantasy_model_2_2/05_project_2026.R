# ============================================================
# STEP 5 - BUILD VALIDATED DIRECT-FPPG SAFETY PROJECTION
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr", "rpart"))
projection_path <- paste0("data/processed/projection_table_", CURRENT_SEASON, ".csv")
if (!file.exists(projection_path)) stop("Missing projection table. Run 02_build_features.R first.")
latest <- readr::read_csv(projection_path, show_col_types = FALSE)

predictions <- list()
for (pos in POSITIONS) {
  reg_path <- paste0("models/model_", pos, ".rds")
  tier_path <- paste0("models/tier_model_", pos, ".rds")
  breakout_path <- paste0("models/breakout_model_", pos, ".rds")
  if (!file.exists(reg_path)) next

  reg_obj <- readRDS(reg_path)
  d <- latest |> dplyr::filter(position == pos)
  if (nrow(d) == 0) next

  tuned_weight <- get_position_blend_weight(pos)
  calibration <- get_position_calibration(pos)
  cat("[PROJECT] ", pos,
      " | model weight=", sprintf("%.2f", tuned_weight),
      " | calibration=", sprintf("%+.2f + %.2fx", calibration$intercept, calibration$slope),
      "...\n", sep = "")

  detail <- predict_fantasy_model_detail(reg_obj, d)
  d$model_only_fppg <- pmax(0, detail$model_prediction)
  d$model_disagreement_sd <- detail$model_disagreement_sd
  d$blend_model_weight <- ifelse(d$is_rookie == 1, 1, tuned_weight)
  d$blend_recent_weight <- 1 - d$blend_model_weight
  d$raw_blended_fppg <- blend_projection(d$model_only_fppg, d, model_weight = tuned_weight)
  d$projected_fppg <- apply_linear_calibration(
    d$raw_blended_fppg, calibration$intercept, calibration$slope
  )

  # 1.2 decision probabilities.
  d$elite_probability <- NA_real_
  d$starter_probability <- NA_real_
  d$breakout_probability <- NA_real_

  if (file.exists(tier_path)) {
    tier_obj <- readRDS(tier_path)
    tier_prob <- predict_fantasy_classifier(tier_obj, d)
    d$elite_probability <- tier_prob[, "Elite"]
    d$starter_probability <- pmin(1, tier_prob[, "Starter"] + tier_prob[, "Elite"])
  }
  if (file.exists(breakout_path)) {
    breakout_obj <- readRDS(breakout_path)
    breakout_prob <- predict_fantasy_classifier(breakout_obj, d)
    d$breakout_probability <- breakout_prob[, "Yes"]
  }

  d$elite_probability[!is.finite(d$elite_probability)] <- 0
  d$starter_probability[!is.finite(d$starter_probability)] <- 0
  d$breakout_probability[!is.finite(d$breakout_probability)] <- 0

  elite_cal <- get_probability_calibration(pos, "Elite")
  starter_cal <- get_probability_calibration(pos, "Starter+")
  breakout_cal <- get_probability_calibration(pos, "Breakout/Upside")
  d$raw_elite_probability <- d$elite_probability
  d$raw_starter_probability <- d$starter_probability
  d$raw_breakout_probability <- d$breakout_probability
  d$elite_probability <- apply_probability_calibration(d$elite_probability, elite_cal$intercept, elite_cal$slope)
  d$starter_probability <- apply_probability_calibration(d$starter_probability, starter_cal$intercept, starter_cal$slope)
  d$breakout_probability <- apply_probability_calibration(d$breakout_probability, breakout_cal$intercept, breakout_cal$slope)
  # Starter+ should never be less likely than Elite after separate calibration fits.
  d$starter_probability <- pmax(d$starter_probability, d$elite_probability)

  # Availability-adjusted season points while keeping per-game strength separate.
  d$projected_games <- dplyr::case_when(
    d$is_rookie == 1 ~ 15.0,
    d$prior_games <= 0 ~ 14.0,
    TRUE ~ pmin(17, pmax(8, 0.65 * d$prior_games + 0.35 * 17))
  )
  d$projected_fantasy_points <- d$projected_fppg * d$projected_games
  d$full_17_game_points <- d$projected_fppg * 17

  # Approximate 80% projection interval from historical residual error + tree disagreement.
  d$calibration_residual_sd <- calibration$residual_sd
  d$total_uncertainty_sd <- sqrt(pmax(0, d$model_disagreement_sd^2 + calibration$residual_sd^2))
  d$projection_floor_fppg <- pmax(0, d$projected_fppg - PROJECTION_INTERVAL_Z * d$total_uncertainty_sd)
  d$projection_ceiling_fppg <- d$projected_fppg + PROJECTION_INTERVAL_Z * d$total_uncertainty_sd

  d$projection_season <- CURRENT_SEASON
  d$model_engine <- reg_obj$engine
  d$model_trees <- length(reg_obj$trees)
  d$confidence <- dplyr::case_when(
    d$is_rookie == 1 & d$total_uncertainty_sd <= 3.0 ~ "Rookie-Medium",
    d$is_rookie == 1 ~ "Rookie-Low",
    d$prior_games >= 14 & d$total_uncertainty_sd <= 3.0 ~ "High",
    d$prior_games >= 8 & d$total_uncertainty_sd <= 4.5 ~ "Medium",
    TRUE ~ "Low"
  )
  d$fantasy_outlook <- dplyr::case_when(
    d$elite_probability >= 0.50 ~ "Elite ceiling",
    d$starter_probability >= 0.65 & d$breakout_probability >= 0.40 ~ "Starter + upside",
    d$starter_probability >= 0.65 ~ "Strong starter",
    d$breakout_probability >= 0.50 ~ "Breakout/upside",
    d$total_uncertainty_sd >= 5 ~ "High variance",
    TRUE ~ "Depth/uncertain"
  )

  predictions[[length(predictions) + 1]] <- d
}

projection <- dplyr::bind_rows(predictions)
if (nrow(projection) == 0) stop("No projections were created.")

projection <- projection |>
  dplyr::group_by(position) |>
  dplyr::arrange(dplyr::desc(projected_fppg), dplyr::desc(elite_probability), .by_group = TRUE) |>
  dplyr::mutate(position_rank = dplyr::row_number()) |>
  dplyr::ungroup()

readr::write_csv(projection, paste0("output/", CURRENT_SEASON, "_projections.csv"))
message(CURRENT_SEASON, " Fantasy Model 2.0 direct guardrail projections complete: ", nrow(projection), " players.")
