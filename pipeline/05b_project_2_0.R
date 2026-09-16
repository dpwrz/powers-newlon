# ============================================================
# STEP 5B - PROJECT 2026 WITH FANTASY MODEL 2.0 ARCHITECTURE
# ============================================================
source("config.R")
source("R/opportunity_engine.R")
ensure_packages(c("dplyr", "readr", "rpart"))

opp_path <- paste0("data/processed/opportunity_projection_table_2_0_", CURRENT_SEASON, ".csv")
direct_path <- paste0("output/", CURRENT_SEASON, "_projections.csv")
params_path <- "output/selected_2_0_shrinkage_parameters.csv"
weights_path <- "output/selected_2_0_architecture_weights.csv"
metrics_path <- "output/validation_metrics_2_0.csv"
if (!file.exists(opp_path)) stop("Missing 2.0 projection feature table.")
if (!file.exists(direct_path)) stop("Missing 1.2 direct projection. Run 05_project_2026.R first.")
if (!file.exists(params_path) || !file.exists(weights_path)) stop("Run 04b_validate_2_0.R first.")

opp <- readr::read_csv(opp_path, show_col_types = FALSE, progress = FALSE)
direct <- readr::read_csv(direct_path, show_col_types = FALSE, progress = FALSE)
rate_params <- readr::read_csv(params_path, show_col_types = FALSE, progress = FALSE)
weights <- readr::read_csv(weights_path, show_col_types = FALSE, progress = FALSE)
metrics <- if (file.exists(metrics_path)) readr::read_csv(metrics_path, show_col_types = FALSE, progress = FALSE) else data.frame()

# Preserve the validated direct output before 2.0 becomes the public projection file.
readr::write_csv(direct, paste0("output/", CURRENT_SEASON, "_projections_1_2_direct.csv"))

rows <- list()
for (i in seq_along(POSITIONS)) {
  pos <- POSITIONS[i]
  d_direct <- direct |> dplyr::filter(position == pos)
  d_opp <- opp |> dplyr::filter(position == pos)
  if (nrow(d_direct) == 0 || nrow(d_opp) == 0) next

  # Direct table remains the canonical player/decision-model row. Add only fields
  # that the 2.0 opportunity builder created and the direct table does not contain.
  extra <- setdiff(names(d_opp), names(d_direct))
  d <- d_direct |>
    dplyr::left_join(d_opp |> dplyr::select(player_id, dplyr::all_of(extra)), by = "player_id")
  d$direct_1_2_fppg <- d$projected_fppg

  opp_model_path <- paste0("models/opportunity_models_2_0_", pos, ".rds")
  if (!file.exists(opp_model_path)) stop("Missing final 2.0 opportunity models for ", pos)
  opp_models <- readRDS(opp_model_path)
  opp_pred <- predict_opportunity_models20(opp_models, d)$predictions
  pos_params <- rate_params |> dplyr::filter(position == pos)
  rate_pred <- predict_all_rates20(d, pos, pos_params)
  statline <- compose_statline20(pos, opp_pred, rate_pred, d)
  d <- dplyr::bind_cols(d, statline)

  cal <- get_2_0_calibration(pos)
  d$opportunity_fppg_calibrated <- apply_linear_calibration(d$opportunity_fppg_raw, cal$intercept, cal$slope)
  d$base_direct_gap <- d$direct_1_2_fppg - d$opportunity_fppg_calibrated

  residual_path <- paste0("models/residual_model_2_0_", pos, ".rds")
  residual_fit <- if (file.exists(residual_path)) readRDS(residual_path) else NULL
  d$residual_correction_20 <- predict_residual20(residual_fit, d)
  d$opportunity_residual_fppg <- pmax(0, d$opportunity_fppg_calibrated + d$residual_correction_20)

  wrow <- weights |> dplyr::filter(position == pos) |> dplyr::slice(1)
  w <- if (nrow(wrow) == 0) 0 else num20(wrow$selected_opportunity_weight)
  if (!is.finite(w)) w <- 0
  d$opportunity_weight <- w
  d$direct_guardrail_weight <- 1 - w
  d$projected_fppg <- pmax(0, w * d$opportunity_residual_fppg + (1 - w) * d$direct_1_2_fppg)

  # Backward-compatible aliases keep the existing structured-stat-aware mobile app/stat
  # scoring layer functional while exposing the new 2.0 names too.
  d$component_fppg_calibrated <- d$opportunity_fppg_calibrated
  d$component_weight <- d$opportunity_weight

  d$projection_architecture <- dplyr::case_when(
    w <= 0 ~ "1.2 direct guardrail",
    w >= 0.999 ~ "2.0 opportunity + shrinkage + residual",
    TRUE ~ paste0("2.0 hybrid (", round(100 * w), "% opportunity architecture)")
  )

  d$projected_fantasy_points <- d$projected_fppg * d$projected_games
  d$full_17_game_points <- d$projected_fppg * 17
  vm <- metrics |> dplyr::filter(position == pos) |> dplyr::slice(1)
  validation_sd <- if (nrow(vm) > 0) num20(vm$model_RMSE) else cal$residual_sd
  if (!is.finite(validation_sd) || validation_sd <= 0) validation_sd <- 3
  d$total_uncertainty_sd <- validation_sd
  d$projection_floor_fppg <- pmax(0, d$projected_fppg - PROJECTION_INTERVAL_Z * validation_sd)
  d$projection_ceiling_fppg <- d$projected_fppg + PROJECTION_INTERVAL_Z * validation_sd

  d$model_engine <- "2.0 opportunity/shrinkage/residual + 1.2 guardrail"
  d$model_version <- PROJECT_VERSION
  d$confidence <- dplyr::case_when(
    d$is_rookie == 1 & validation_sd <= 3.0 ~ "Rookie-Medium",
    d$is_rookie == 1 ~ "Rookie-Low",
    d$prior_games >= 14 & validation_sd <= 3.0 ~ "High",
    d$prior_games >= 8 & validation_sd <= 4.5 ~ "Medium",
    TRUE ~ "Low"
  )

  rows[[length(rows) + 1]] <- d
}

projection <- dplyr::bind_rows(rows)
if (nrow(projection) == 0) stop("Fantasy Model 2.0 produced no projections.")
projection <- projection |>
  dplyr::group_by(position) |>
  dplyr::arrange(dplyr::desc(projected_fppg), dplyr::desc(elite_probability), .by_group = TRUE) |>
  dplyr::mutate(position_rank = dplyr::row_number()) |>
  dplyr::ungroup()

readr::write_csv(projection, paste0("output/", CURRENT_SEASON, "_projections_2_0.csv"))
readr::write_csv(projection, paste0("output/", CURRENT_SEASON, "_projections.csv"))
cat("[2.0 PROJECT] Final 2.0 projections complete: ", nrow(projection), " players.\n", sep = "")
