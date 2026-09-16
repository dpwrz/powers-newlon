# ============================================================
# STEP 3B - TRAIN FINAL FANTASY MODEL 2.0 OPPORTUNITY / RESIDUAL MODELS
# ============================================================
source("config.R")
source("R/opportunity_engine.R")
ensure_packages(c("dplyr", "readr", "rpart"))

path <- "data/processed/opportunity_model_table_2_0.csv"
if (!file.exists(path)) stop("Missing 2.0 opportunity table. Run 02b_build_opportunity_data.R first.")
df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

importance_rows <- list()
for (i in seq_along(POSITIONS)) {
  pos <- POSITIONS[i]
  d <- df |> dplyr::filter(position == pos)
  if (nrow(d) < 50) next
  cat("[2.0 TRAIN] ", pos, " opportunity models...\n", sep = "")
  models <- fit_opportunity_models20(d, pos, n_trees = OPPORTUNITY_FINAL_TREES, seed = SEED + i * 10000)
  if (length(models) == 0) next
  saveRDS(models, paste0("models/opportunity_models_2_0_", pos, ".rds"))
  for (nm in names(models)) {
    imp <- get_feature_importance(models[[nm]])
    if (nrow(imp) == 0) next
    imp$position <- pos
    imp$component <- nm
    importance_rows[[length(importance_rows) + 1]] <- imp
  }
}
importance <- dplyr::bind_rows(importance_rows)
if (nrow(importance) > 0) readr::write_csv(importance, "output/opportunity_feature_importance.csv")

# Refit final shrinkage params in case this script is run independently of validation.
rate_rows <- list()
for (pos in POSITIONS) {
  d <- df |> dplyr::filter(position == pos)
  if (nrow(d) < 50) next
  rate_rows[[length(rate_rows) + 1]] <- fit_all_rate_params20(d, pos)
}
rate_params <- dplyr::bind_rows(rate_rows)
if (nrow(rate_params) > 0) readr::write_csv(rate_params, "output/selected_2_0_shrinkage_parameters.csv")

# Refit the final residual models on honest OOF rows using the final 2026 calibration.
oof_path <- "output/validation_2_0_predictions.csv"
cal_path <- "output/selected_2_0_calibration.csv"
if (file.exists(oof_path) && file.exists(cal_path)) {
  oof <- readr::read_csv(oof_path, show_col_types = FALSE, progress = FALSE)
  cal_all <- readr::read_csv(cal_path, show_col_types = FALSE, progress = FALSE)
  for (i in seq_along(POSITIONS)) {
    pos <- POSITIONS[i]
    d <- oof |> dplyr::filter(position == pos)
    cal <- cal_all |> dplyr::filter(position == pos) |> dplyr::slice(1)
    if (nrow(d) < RESIDUAL_MIN_ROWS || nrow(cal) == 0) next
    d$opportunity_fppg_calibrated <- apply_linear_calibration(d$opportunity_fppg_raw, cal$intercept, cal$slope)
    d$base_direct_gap <- d$direct_1_2_fppg - d$opportunity_fppg_calibrated
    d$residual_target_20 <- d$target_fppg - d$opportunity_fppg_calibrated
    fit <- fit_residual_model20(d, pos, n_trees = RESIDUAL_FINAL_TREES, seed = SEED + i * 300000)
    if (!is.null(fit)) saveRDS(fit, paste0("models/residual_model_2_0_", pos, ".rds"))
  }
}

cat("[2.0 TRAIN] Final opportunity/shrinkage/residual models complete.\n")
