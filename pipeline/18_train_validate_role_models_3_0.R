# ============================================================
# STEP 18 - HONEST WALK-FORWARD VALIDATION OF 3.0 ROLE MODELS
# ============================================================
source("config.R")
source("R/role_engine_30.R")
ensure_packages(c("dplyr", "readr", "purrr", "tibble", "rpart"))

if (!file.exists(ROLE30_OUTPUT_TABLE)) source("pipeline/17_build_role_regime_features_3_0.R")
d <- readr::read_csv(ROLE30_OUTPUT_TABLE, show_col_types = FALSE)

oof_rows <- list()
metric_rows <- list()
manifest_rows <- list()

for (pos in names(ROLE30_TARGETS)) {
  for (target in ROLE30_TARGETS[[pos]]) {
    if (!target %in% names(d)) next
    cat("[3.0 ROLE] Validating ", pos, " ", target, "...\n", sep = "")
    pos_oof <- list()
    for (yy in seq.int(ROLE30_VALIDATION_START, ROLE30_VALIDATION_END)) {
      train <- d |> dplyr::filter(.data$position == .env$pos, .data$season < .env$yy)
      test <- d |> dplyr::filter(.data$position == .env$pos, .data$season == .env$yy)
      if (nrow(train) < ROLE30_MIN_TRAIN_ROWS || nrow(test) < 20) next
      feats <- fm30_role_features(pos, names(train))
      if (length(feats) < 8) next
      fit <- tryCatch(
        fit_fantasy_model(train, feats, target, n_trees = ROLE30_VALIDATION_TREES),
        error = function(e) NULL
      )
      if (is.null(fit)) next
      model_pred <- fm30_predict_role_model(fit, test, target)
      baseline_pred <- fm30_baseline_for_target(test, pos, target)
      # Capture the target vector before entering tibble(). tibble evaluates
      # columns sequentially, so creating a column named `target` first would
      # otherwise mask the scalar loop variable `target` in test[[target]].
      target_name <- as.character(target)[1]
      actual_target <- fm30_num(test[[target_name]], NA_real_)
      pos_oof[[length(pos_oof) + 1]] <- tibble::tibble(
        season = test$season, week = test$week, player_id = test$player_id,
        player_display_name = test$player_display_name, position = pos,
        target = target_name, actual = actual_target,
        baseline_pred = baseline_pred, model_pred = model_pred
      )
    }
    oof <- dplyr::bind_rows(pos_oof)
    if (!nrow(oof)) next
    bm <- fm30_metrics(oof$actual, oof$baseline_pred)
    mm <- fm30_metrics(oof$actual, oof$model_pred)
    promoted <- fm30_promotion(bm, mm)
    metric_rows[[length(metric_rows) + 1]] <- tibble::tibble(
      position = pos, target = target, n = nrow(oof),
      baseline_MAE = bm[["MAE"]], model_MAE = mm[["MAE"]],
      mae_gain_pct = ifelse(is.finite(bm[["MAE"]]) && bm[["MAE"]] > 0, (bm[["MAE"]] - mm[["MAE"]]) / bm[["MAE"]], NA_real_),
      baseline_RMSE = bm[["RMSE"]], model_RMSE = mm[["RMSE"]],
      baseline_cor = bm[["Cor"]], model_cor = mm[["Cor"]],
      promoted = promoted
    )
    oof$promoted <- promoted
    oof_rows[[length(oof_rows) + 1]] <- oof

    full_train <- d |> dplyr::filter(.data$position == .env$pos, .data$season <= .env$TRAIN_END)
    feats <- fm30_role_features(pos, names(full_train))
    final_model <- tryCatch(
      fit_fantasy_model(full_train, feats, target, n_trees = ROLE30_ENSEMBLE_TREES),
      error = function(e) NULL
    )
    model_path <- paste0("models/role_3_0_", pos, "_", target, ".rds")
    if (!is.null(final_model)) saveRDS(final_model, model_path)
    manifest_rows[[length(manifest_rows) + 1]] <- tibble::tibble(
      position = pos, target = target, promoted = promoted,
      model_path = if (!is.null(final_model)) model_path else "",
      feature_count = length(feats), trained_through = TRAIN_END
    )
  }
}

metrics <- dplyr::bind_rows(metric_rows)
oof_all <- dplyr::bind_rows(oof_rows)
manifest <- dplyr::bind_rows(manifest_rows)
readr::write_csv(metrics, "output/role_3_0_validation_metrics.csv")
readr::write_csv(oof_all, "output/role_3_0_validation_predictions.csv")
readr::write_csv(manifest, "output/role_3_0_model_manifest.csv")
cat("[3.0 ROLE] Validation complete. Promoted components: ", sum(manifest$promoted %in% TRUE), "/", nrow(manifest), ".\n", sep = "")
