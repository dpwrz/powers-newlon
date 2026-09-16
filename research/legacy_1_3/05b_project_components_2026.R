# ============================================================
# STEP 5B - APPLY 1.3 COMPONENT PROJECTIONS + VALIDATED HYBRID
# ============================================================
source("config.R")
source("research/legacy_1_3/R/component_engine.R")
ensure_packages(c("dplyr", "readr", "rpart"))

base_path <- paste0("output/", CURRENT_SEASON, "_projections.csv")
component_path <- paste0("data/processed/component_projection_table_", CURRENT_SEASON, ".csv")
if (!file.exists(base_path)) stop("Missing direct 1.2-style projection output. Run 05_project_2026.R first.")
if (!file.exists(component_path)) stop("Missing component projection table. Run 02b_build_component_data.R first.")

base <- readr::read_csv(base_path, show_col_types = FALSE)
current <- readr::read_csv(component_path, show_col_types = FALSE)
rows <- list()

for (pos in POSITIONS) {
  model_path <- paste0("models/component_models_", pos, ".rds")
  d <- current |> dplyr::filter(position == pos)
  if (nrow(d) == 0 || !file.exists(model_path)) next
  models <- readRDS(model_path)
  pd <- predict_component_models(models, d)
  derived <- component_fantasy_projection(pos, pd$predictions, d)
  out <- dplyr::bind_cols(
    d |> dplyr::select(player_id, position),
    derived
  )
  # Preserve component-level predictions for diagnostics/player pages.
  for (nm in names(pd$predictions)) out[[paste0("component_", nm)]] <- pd$predictions[[nm]]
  comp_sd <- if (ncol(pd$disagreement) > 0) sqrt(rowMeans(as.matrix(pd$disagreement)^2, na.rm = TRUE)) else rep(0, nrow(d))
  comp_sd[!is.finite(comp_sd)] <- 0
  out$component_model_disagreement <- comp_sd
  rows[[length(rows) + 1]] <- out
}

components <- dplyr::bind_rows(rows)
if (nrow(components) == 0) stop("No 1.3 component projections were created.")

projection <- base |>
  dplyr::rename(direct_1_2_fppg = projected_fppg) |>
  dplyr::left_join(components, by = c("player_id", "position"))

projection$component_fppg_calibrated <- projection$component_fppg_raw
projection$component_weight <- 0
projection$direct_1_2_weight <- 1
projection$component_calibration_residual_sd <- 0

for (pos in POSITIONS) {
  idx <- which(projection$position == pos)
  if (length(idx) == 0) next
  cal <- get_component_calibration(pos)
  w <- get_selected_component_weight(pos)
  raw <- projection$component_fppg_raw[idx]
  raw[!is.finite(raw)] <- projection$direct_1_2_fppg[idx][!is.finite(raw)]
  projection$component_fppg_calibrated[idx] <- apply_linear_calibration(raw, cal$intercept, cal$slope)
  projection$component_weight[idx] <- w
  projection$direct_1_2_weight[idx] <- 1 - w
  projection$component_calibration_residual_sd[idx] <- cal$residual_sd
}

projection <- projection |>
  dplyr::mutate(
    component_fppg_calibrated = dplyr::coalesce(component_fppg_calibrated, direct_1_2_fppg),
    projected_fppg = pmax(0, component_weight * component_fppg_calibrated + direct_1_2_weight * direct_1_2_fppg),
    projected_fantasy_points = projected_fppg * projected_games,
    full_17_game_points = projected_fppg * 17,
    component_model_disagreement = dplyr::coalesce(component_model_disagreement, 0),
    component_calibration_residual_sd = dplyr::coalesce(component_calibration_residual_sd, 0),
    total_uncertainty_sd = sqrt(
      pmax(0,
        (direct_1_2_weight * dplyr::coalesce(total_uncertainty_sd, 0))^2 +
        (component_weight * component_calibration_residual_sd)^2 +
        (0.35 * component_weight * component_model_disagreement)^2
      )
    ),
    projection_floor_fppg = pmax(0, projected_fppg - PROJECTION_INTERVAL_Z * total_uncertainty_sd),
    projection_ceiling_fppg = projected_fppg + PROJECTION_INTERVAL_Z * total_uncertainty_sd,
    model_architecture = "1.3 component-hybrid",
    projection_source = dplyr::case_when(
      component_weight >= 0.99 ~ "Component model",
      component_weight <= 0.01 ~ "Validated 1.2 direct model",
      TRUE ~ "Validated component/direct hybrid"
    )
  ) |>
  dplyr::group_by(position) |>
  dplyr::arrange(dplyr::desc(projected_fppg), dplyr::desc(elite_probability), .by_group = TRUE) |>
  dplyr::mutate(position_rank = dplyr::row_number()) |>
  dplyr::ungroup()

readr::write_csv(projection, paste0("output/", CURRENT_SEASON, "_component_projections.csv"))
readr::write_csv(projection, base_path)
message(CURRENT_SEASON, " Fantasy Model 1.3 component-hybrid projections complete: ", nrow(projection), " players.")
