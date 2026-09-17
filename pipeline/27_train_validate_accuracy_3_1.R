# ============================================================
# FANTASY MODEL 3.1 - DECISION-WEIGHTED ACCURACY TOURNAMENT
# ============================================================
# Honest outer walk-forward. Hyperparameters are chosen only from seasons that
# precede the outer target season. 2026 outcomes are not used for fitting.

source("config.R")
ensure_packages(c("dplyr", "readr", "xgboost"))
source("R/weekly_engine_31.R")

dir.create("models", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

req <- c(
  "data/processed/weekly_model_table_2_3.csv",
  "output/weekly_2_5_validation_predictions.csv",
  "output/weekly_2_5_champion_manifest.csv"
)
miss <- req[!file.exists(req)]
if (length(miss)) stop("3.1 accuracy tournament missing prerequisite(s): ", paste(miss, collapse = ", "))

weekly <- readr::read_csv(req[1], show_col_types = FALSE, progress = FALSE)
val25 <- readr::read_csv(req[2], show_col_types = FALSE, progress = FALSE)
manifest25 <- readr::read_csv(req[3], show_col_types = FALSE, progress = FALSE)

weekly$season <- as.integer(weekly$season)
weekly$week <- as.integer(weekly$week)
weekly$player_id <- as.character(weekly$player_id)
val25$season <- as.integer(val25$season)
val25$week <- as.integer(val25$week)
val25$player_id <- as.character(val25$player_id)

missing_safe <- setdiff(MODEL25_SAFE_FEATURES, names(weekly))
if (length(missing_safe)) stop("Historical store missing safe 3.1 input(s): ", paste(missing_safe, collapse = ", "))

promo25 <- setNames(wk25_bool(manifest25$promoted_for_2026), as.character(manifest25$position))
val25$production_base_31 <- ifelse(
  promo25[as.character(val25$position)] %in% TRUE,
  wk31_num(val25$candidate_fppg_25),
  wk31_num(val25$production_base_25)
)

# Upgrade the benchmark to the exact deployed 2.5 feedback forecast where that
# feedback layer passed its own honest promotion gate.
fb_path <- "output/weekly_2_5_feedback_validation_predictions.csv"
fb_promo_path <- "output/weekly_2_5_feedback_promotion.csv"
if (file.exists(fb_path) && file.exists(fb_promo_path)) {
  fb <- readr::read_csv(fb_path, show_col_types = FALSE, progress = FALSE)
  fp <- readr::read_csv(fb_promo_path, show_col_types = FALSE, progress = FALSE)
  fp_map <- setNames(wk25_bool(fp$promoted_for_2026), as.character(fp$position))
  keep <- intersect(c("season","week","player_id","position",
                      "feedback_base_projection_25","feedback_post_projection_25"), names(fb))
  fb2 <- fb[, keep, drop = FALSE]
  val25 <- dplyr::left_join(val25, fb2, by = c("season","week","player_id","position"))
  use_fb <- fp_map[as.character(val25$position)] %in% TRUE &
    is.finite(wk31_num(val25$feedback_post_projection_25, NA_real_))
  val25$production_base_31[use_fb] <- wk31_num(val25$feedback_post_projection_25[use_fb])
}

years <- sort(unique(as.integer(val25$season)))
years <- years[is.finite(years)]
pred_rows <- list()
config_rows <- list()
alpha_rows <- list()

cat("[3.1] Honest OOF seasons: ", paste(years, collapse = ", "), "\n", sep = "")

for (pos in POSITIONS) {
  prior_oof <- data.frame()
  for (yr in years) {
    train <- weekly |> dplyr::filter(position == pos, season < yr)
    test <- weekly |> dplyr::filter(position == pos, season == yr)
    base <- val25 |> dplyr::filter(position == pos, season == yr)
    if (!nrow(base) || !nrow(test)) next
    if (nrow(train) < 100) stop("Insufficient 3.1 chronological training rows for ", pos, " before ", yr)

    tune <- wk31_select_config(train, pos, seed = SEED + yr + 310L * match(pos, POSITIONS))
    selected <- tune[tune$selected %in% TRUE, , drop = FALSE]
    if (!nrow(selected)) selected <- tune[1, , drop = FALSE]

    tune$outer_target_year <- yr
    tune$position <- pos
    config_rows[[length(config_rows) + 1]] <- tune

    fit <- wk31_fit(
      train, pos, selected,
      seed = SEED + 3100L + yr + 100L * match(pos, POSITIONS)
    )
    direct <- wk31_predict(fit, test)
    ptab <- data.frame(
      season = as.integer(test$season),
      week = as.integer(test$week),
      player_id = as.character(test$player_id),
      position = as.character(test$position),
      direct_fppg_31 = direct,
      stringsAsFactors = FALSE
    )
    cur <- dplyr::left_join(base, ptab, by = c("season","week","player_id","position"))
    if (any(!is.finite(wk31_num(cur$direct_fppg_31, NA_real_)))) {
      stop("3.1 direct OOF coverage failure for ", yr, " ", pos)
    }

    alpha <- if (!nrow(prior_oof)) 0.10 else wk31_select_alpha(prior_oof, pos, final = FALSE)
    cur$blend_alpha_31 <- alpha
    cur$candidate_fppg_31 <- pmax(
      0,
      wk31_num(cur$production_base_31) +
        alpha * (wk31_num(cur$direct_fppg_31) - wk31_num(cur$production_base_31))
    )
    cur$direct_gap_31 <- cur$direct_fppg_31 - cur$production_base_31
    pred_rows[[length(pred_rows) + 1]] <- cur

    alpha_rows[[length(alpha_rows) + 1]] <- data.frame(
      position = pos,
      target_year = yr,
      prior_oof_years = length(unique(as.integer(prior_oof$season))),
      blend_alpha_31 = alpha,
      selected_config_id = as.character(selected$config_id[1]),
      stringsAsFactors = FALSE
    )
    prior_oof <- dplyr::bind_rows(prior_oof, cur)
  }
}

val31 <- dplyr::bind_rows(pred_rows) |>
  dplyr::arrange(position, season, week, player_id)
if (!nrow(val31)) stop("3.1 validation produced no rows.")

readr::write_csv(val31, "output/weekly_3_1_validation_predictions.csv")
readr::write_csv(dplyr::bind_rows(config_rows), "output/weekly_3_1_tuning_results.csv")
readr::write_csv(dplyr::bind_rows(alpha_rows), "output/weekly_3_1_oof_blend_schedule.csv")

metric_rows <- list()
year_rows <- list()
promotion_rows <- list()
manifest_rows <- list()
bootstrap_rows <- list()
importance_rows <- list()

for (pos in POSITIONS) {
  d <- val31 |> dplyr::filter(position == pos)
  if (!nrow(d)) next

  mm <- wk31_cohort_metrics(d, "candidate_fppg_31", "production_base_31")
  mm$position <- pos
  mm$MAE_improvement_pct <- 100 * (mm$base_MAE - mm$candidate_MAE) / pmax(mm$base_MAE, 1e-9)
  mm$RMSE_improvement_pct <- 100 * (mm$base_RMSE - mm$candidate_RMSE) / pmax(mm$base_RMSE, 1e-9)
  metric_rows[[length(metric_rows) + 1]] <- mm

  yy <- lapply(sort(unique(as.integer(d$season))), function(yr) {
    z <- d[d$season == yr, , drop = FALSE]
    m <- wk31_cohort_metrics(z, "candidate_fppg_31", "production_base_31")
    m$season <- yr
    m$position <- pos
    m$MAE_improvement_pct <- 100 * (m$base_MAE - m$candidate_MAE) / pmax(m$base_MAE, 1e-9)
    m$RMSE_improvement_pct <- 100 * (m$base_RMSE - m$candidate_RMSE) / pmax(m$base_RMSE, 1e-9)
    m
  })
  year_rows[[length(year_rows) + 1]] <- dplyr::bind_rows(yy)

  final_alpha <- wk31_select_alpha(d, pos, final = TRUE)
  gate <- wk31_promotion_gate(d, pos)
  gate$final_alpha_2026 <- final_alpha
  promotion_rows[[length(promotion_rows) + 1]] <- gate

  bs <- wk31_bootstrap_gain(d, "Starter", seed = SEED + 3110L + match(pos, POSITIONS))
  br <- wk31_bootstrap_gain(d, "Relevant", seed = SEED + 3120L + match(pos, POSITIONS))
  bs$position <- pos; br$position <- pos
  bootstrap_rows[[length(bootstrap_rows) + 1]] <- dplyr::bind_rows(bs, br)

  # Final hyperparameters are selected using only historical seasons through
  # TRAIN_END. The newest historical year acts as the inner validation year.
  full_train <- weekly |> dplyr::filter(position == pos, season <= TRAIN_END)
  tune <- wk31_select_config(full_train, pos, seed = SEED + 3130L + match(pos, POSITIONS))
  selected <- tune[tune$selected %in% TRUE, , drop = FALSE]
  if (!nrow(selected)) selected <- tune[1, , drop = FALSE]
  fit <- wk31_fit(full_train, pos, selected,
                  seed = SEED + 3140L + 100L * match(pos, POSITIONS))
  model_path <- paste0("models/weekly_3_1_direct_", pos, ".json")
  meta_path <- paste0("models/weekly_3_1_direct_", pos, "_meta.rds")
  xgboost::xgb.save(fit$model, model_path)
  saveRDS(list(
    position = pos,
    features = fit$features,
    config = selected,
    nrounds = fit$nrounds,
    n_train = fit$n_train,
    final_alpha_2026 = final_alpha,
    promoted_for_2026 = isTRUE(gate$promoted_for_2026[1]),
    trained_through = TRAIN_END
  ), meta_path)

  imp <- tryCatch(
    xgboost::xgb.importance(feature_names = fit$features, model = fit$model),
    error = function(e) data.frame()
  )
  if (nrow(imp)) {
    imp$position <- pos
    importance_rows[[length(importance_rows) + 1]] <- as.data.frame(imp)
  }

  starter <- mm[mm$cohort == "Starter", , drop = FALSE]
  relevant <- mm[mm$cohort == "Relevant", , drop = FALSE]
  allm <- mm[mm$cohort == "All", , drop = FALSE]
  manifest_rows[[length(manifest_rows) + 1]] <- data.frame(
    position = pos,
    incumbent = "2.5-production-plus-feedback",
    challenger = "3.1-decision-weighted-tuned-adaptive",
    promoted_for_2026 = isTRUE(gate$promoted_for_2026[1]),
    final_alpha_2026 = final_alpha,
    selected_config_id = as.character(selected$config_id[1]),
    incumbent_starter_MAE = starter$base_MAE[1],
    challenger_starter_MAE = starter$candidate_MAE[1],
    incumbent_starter_RMSE = starter$base_RMSE[1],
    challenger_starter_RMSE = starter$candidate_RMSE[1],
    incumbent_relevant_MAE = relevant$base_MAE[1],
    challenger_relevant_MAE = relevant$candidate_MAE[1],
    incumbent_MAE = allm$base_MAE[1],
    challenger_MAE = allm$candidate_MAE[1],
    starter_bootstrap_p_improves = gate$starter_bootstrap_p_improves[1],
    failed_checks = gate$failed_checks[1],
    stringsAsFactors = FALSE
  )
}

metrics31 <- dplyr::bind_rows(metric_rows)
years31 <- dplyr::bind_rows(year_rows)
promotion31 <- dplyr::bind_rows(promotion_rows)
manifest31 <- dplyr::bind_rows(manifest_rows)
bootstrap31 <- dplyr::bind_rows(bootstrap_rows)

readr::write_csv(metrics31, "output/weekly_3_1_cohort_metrics.csv")
readr::write_csv(years31, "output/weekly_3_1_year_metrics.csv")
readr::write_csv(promotion31, "output/weekly_3_1_promotion.csv")
readr::write_csv(manifest31, "output/weekly_3_1_champion_manifest.csv")
readr::write_csv(bootstrap31, "output/weekly_3_1_bootstrap_confidence.csv")
if (length(importance_rows)) {
  readr::write_csv(dplyr::bind_rows(importance_rows), "output/weekly_3_1_direct_importance.csv")
}

res31 <- val31 |>
  dplyr::transmute(
    season, week, player_id, player_display_name, position,
    actual_fppg, production_base_31, direct_fppg_31, candidate_fppg_31,
    residual = actual_fppg - candidate_fppg_31,
    abs_error = abs(residual),
    starter_cohort, relevant_cohort
  )
readr::write_csv(res31, "output/weekly_3_1_residual_pool.csv")

lines <- c(
  "FANTASY MODEL 3.1 - PERFORMANCE / ACCURACY TOURNAMENT",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  "",
  "CHANGES",
  "- Decision-weighted training uses only pre-kickoff relevance proxies.",
  "- XGBoost hyperparameters are selected independently by position.",
  "- Inner tuning uses only seasons earlier than each outer OOF target season.",
  "- Adaptive prior features accelerate/decelerate preseason-prior decay from lagged role evidence.",
  "- Promotion compares against the exact deployed 2.5 + validated feedback baseline.",
  "- 2026 outcomes are not used for fitting or hyperparameter selection.",
  "",
  "PROMOTION SUMMARY"
)
for (i in seq_len(nrow(manifest31))) {
  r <- manifest31[i, ]
  lines <- c(lines, sprintf(
    "%s: %s | alpha %.2f | starter MAE %.4f -> %.4f | starter RMSE %.4f -> %.4f | config %s | %s",
    r$position,
    ifelse(r$promoted_for_2026, "PROMOTE", "KEEP 2.5"),
    r$final_alpha_2026,
    r$incumbent_starter_MAE, r$challenger_starter_MAE,
    r$incumbent_starter_RMSE, r$challenger_starter_RMSE,
    r$selected_config_id,
    ifelse(nchar(r$failed_checks), paste0("failed: ", r$failed_checks), "all gates passed")
  ))
}
writeLines(lines, "output/weekly_3_1_model_quality_report.txt")

cat("\n[3.1] Accuracy tournament complete. Production changes only where promotion gates passed.\n")
print(manifest31)
