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
# -----------------------------------------------------------------------------

# ============================================================
# FANTASY MODEL 2.5 - HONEST ACCURACY TOURNAMENT
# ============================================================
# Modern direct XGBoost challenger vs the CURRENT position-specific production
# champion. Training is chronological. Blend alpha for a target season is
# selected only from earlier OOF seasons; the first OOF season uses a fixed
# conservative bootstrap alpha.

source("config.R")
ensure_packages(c("dplyr", "readr", "xgboost"))
source("R/weekly_engine_25.R")

set.seed(SEED)
dir.create("models", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

req <- c(
  "data/processed/weekly_model_table_2_3.csv",
  "output/weekly_2_4_validation_predictions.csv",
  "output/weekly_2_4_promotion.csv"
)
miss <- req[!file.exists(req)]
if (length(miss)) stop(
  "Missing 2.5 prerequisite(s): ", paste(miss, collapse = ", "),
  ". Restore the completed 2.4 historical artifacts or rerun the 2.4 validation first."
)

cat("\n[2.5] Loading chronological weekly history + current champion OOF...\n")
weekly <- readr::read_csv("data/processed/weekly_model_table_2_3.csv", show_col_types = FALSE, progress = FALSE)
oof24 <- readr::read_csv("output/weekly_2_4_validation_predictions.csv", show_col_types = FALSE, progress = FALSE)
promo24 <- readr::read_csv("output/weekly_2_4_promotion.csv", show_col_types = FALSE, progress = FALSE)

weekly$season <- as.integer(weekly$season); weekly$week <- as.integer(weekly$week)
oof24$season <- as.integer(oof24$season); oof24$week <- as.integer(oof24$week)
weekly$player_id <- as.character(weekly$player_id); oof24$player_id <- as.character(oof24$player_id)

missing_safe <- setdiff(MODEL25_SAFE_FEATURES, names(weekly))
if (length(missing_safe)) stop("Historical feature store is missing 2.5 safe feature(s): ", paste(missing_safe, collapse = ", "))

promoted24 <- as.character(promo24$position[wk25_bool(promo24$promoted_for_2026)])
oof24$incumbent_model_25 <- ifelse(oof24$position %in% promoted24, "2.4", "locked-2.3/2.3.2")
oof24$production_base_25 <- ifelse(
  oof24$position %in% promoted24,
  wk25_num(oof24$projected_weekly_fppg_24),
  wk25_num(oof24$locked_base_fppg_24)
)

validation_years <- sort(unique(oof24$season))
validation_years <- validation_years[is.finite(validation_years)]
cat("[2.5] Honest OOF seasons: ", paste(validation_years, collapse = ", "), "\n", sep = "")
cat("[2.5] Production incumbent by position: ", paste(POSITIONS, ifelse(POSITIONS %in% promoted24, "2.4", "locked"), sep = "=" , collapse = ", "), "\n", sep = "")

pred_rows <- list(); alpha_rows <- list(); importance_rows <- list()

for (pos in POSITIONS) {
  prior_oof25 <- data.frame()
  for (yr in validation_years) {
    train <- weekly |> dplyr::filter(position == pos, season < yr)
    test_features <- weekly |> dplyr::filter(position == pos, season == yr)
    cur_oof <- oof24 |> dplyr::filter(position == pos, season == yr)
    if (!nrow(cur_oof)) next
    if (nrow(train) < 100) stop("Insufficient chronological 2.5 training rows for ", pos, " before ", yr)

    cat("[2.5] ", yr, " ", pos, " | train rows ", nrow(train), " | OOF rows ", nrow(cur_oof), "\n", sep = "")
    fit <- wk25_fit_direct(train, "weekly_fppg", MODEL25_SAFE_FEATURES, seed = SEED + yr + match(pos, POSITIONS) * 100L)
    direct <- wk25_predict_direct(fit, test_features)
    ptab <- data.frame(
      season = as.integer(test_features$season), week = as.integer(test_features$week),
      player_id = as.character(test_features$player_id), position = as.character(test_features$position),
      direct_fppg_25 = direct, stringsAsFactors = FALSE
    )
    cur <- dplyr::left_join(cur_oof, ptab, by = c("season", "week", "player_id", "position"))
    if (any(!is.finite(cur$direct_fppg_25))) stop("2.5 direct OOF coverage failure for ", yr, " ", pos)

    alpha <- if (!nrow(prior_oof25)) MODEL25_BOOTSTRAP_ALPHA else wk25_select_alpha(prior_oof25, pos, final = FALSE)
    cur$blend_alpha_25 <- alpha
    cur$candidate_fppg_25 <- pmax(0, wk25_num(cur$production_base_25) + alpha * (wk25_num(cur$direct_fppg_25) - wk25_num(cur$production_base_25)))
    cur$direct_gap_25 <- cur$direct_fppg_25 - cur$production_base_25
    pred_rows[[length(pred_rows) + 1]] <- cur
    alpha_rows[[length(alpha_rows) + 1]] <- data.frame(
      position = pos, target_year = yr,
      prior_oof_years = if (nrow(prior_oof25)) length(unique(as.integer(prior_oof25$season))) else 0L,
      blend_alpha_25 = alpha
    )
    prior_oof25 <- dplyr::bind_rows(prior_oof25, cur)
  }
}

val25 <- dplyr::bind_rows(pred_rows) |> dplyr::arrange(position, season, week, player_id)
if (!nrow(val25)) stop("2.5 validation produced no rows.")
readr::write_csv(val25, "output/weekly_2_5_validation_predictions.csv")
readr::write_csv(dplyr::bind_rows(alpha_rows), "output/weekly_2_5_oof_blend_schedule.csv")

cohort_rows <- list(); year_rows <- list(); promotion_rows <- list(); manifest_rows <- list(); bootstrap_rows <- list()
for (pos in POSITIONS) {
  d <- val25 |> dplyr::filter(position == pos)
  cmp <- wk25_compare(d)
  cmp$position <- pos
  cohort_rows[[length(cohort_rows) + 1]] <- cmp

  yy <- wk25_year_metrics(d); yy$position <- pos
  year_rows[[length(year_rows) + 1]] <- yy

  gate <- wk25_promotion_gate(d, pos)
  final_alpha <- wk25_select_alpha(d, pos, final = TRUE)
  gate$final_alpha_2026 <- final_alpha
  promotion_rows[[length(promotion_rows) + 1]] <- gate

  boot_s <- wk25_cluster_bootstrap(d, cohort = "Starter", seed = SEED + match(pos, POSITIONS) * 10L); boot_s$position <- pos
  boot_r <- wk25_cluster_bootstrap(d, cohort = "Relevant", seed = SEED + match(pos, POSITIONS) * 20L); boot_r$position <- pos
  bootstrap_rows[[length(bootstrap_rows) + 1]] <- dplyr::bind_rows(boot_s, boot_r)

  allrow <- cmp[cmp$cohort == "All", , drop = FALSE]
  strow <- cmp[cmp$cohort == "Starter", , drop = FALSE]
  manifest_rows[[length(manifest_rows) + 1]] <- data.frame(
    position = pos,
    incumbent = as.character(d$incumbent_model_25[1]),
    challenger = "2.5-xgboost-direct-blend",
    promoted_for_2026 = gate$promoted_for_2026[1],
    final_alpha_2026 = final_alpha,
    incumbent_MAE = allrow$base_MAE[1], challenger_MAE = allrow$candidate_MAE[1],
    incumbent_RMSE = allrow$base_RMSE[1], challenger_RMSE = allrow$candidate_RMSE[1],
    incumbent_starter_MAE = strow$base_MAE[1], challenger_starter_MAE = strow$candidate_MAE[1],
    incumbent_starter_RMSE = strow$base_RMSE[1], challenger_starter_RMSE = strow$candidate_RMSE[1],
    starter_bootstrap_p_improves = gate$starter_bootstrap_p_improves[1],
    failed_checks = gate$failed_checks[1], stringsAsFactors = FALSE
  )
}

cohort25 <- dplyr::bind_rows(cohort_rows) |> dplyr::select(position, dplyr::everything())
year25 <- dplyr::bind_rows(year_rows) |> dplyr::select(position, season, dplyr::everything())
promotion25 <- dplyr::bind_rows(promotion_rows)
manifest25 <- dplyr::bind_rows(manifest_rows)
bootstrap25 <- dplyr::bind_rows(bootstrap_rows) |> dplyr::select(position, dplyr::everything())

readr::write_csv(cohort25, "output/weekly_2_5_cohort_metrics.csv")
readr::write_csv(year25, "output/weekly_2_5_year_metrics.csv")
readr::write_csv(promotion25, "output/weekly_2_5_promotion.csv")
readr::write_csv(manifest25, "output/weekly_2_5_champion_manifest.csv")
readr::write_csv(bootstrap25, "output/weekly_2_5_bootstrap_confidence.csv")

# Compact position-level validation table for dashboards/reports.
metrics25 <- manifest25 |> dplyr::mutate(
  MAE_improvement_pct = 100 * (incumbent_MAE - challenger_MAE) / pmax(incumbent_MAE, 1e-9),
  RMSE_improvement_pct = 100 * (incumbent_RMSE - challenger_RMSE) / pmax(incumbent_RMSE, 1e-9),
  starter_MAE_improvement_pct = 100 * (incumbent_starter_MAE - challenger_starter_MAE) / pmax(incumbent_starter_MAE, 1e-9),
  starter_RMSE_improvement_pct = 100 * (incumbent_starter_RMSE - challenger_starter_RMSE) / pmax(incumbent_starter_RMSE, 1e-9)
)
readr::write_csv(metrics25, "output/weekly_2_5_validation_metrics.csv")

# Residual pool follows the honest year-by-year blend, not a refit on outcomes.
res25 <- val25 |> dplyr::transmute(
  season, week, player_id, player_display_name, position,
  actual_fppg, production_base_25, direct_fppg_25, candidate_fppg_25,
  residual = actual_fppg - candidate_fppg_25,
  abs_error = abs(residual), starter_cohort, relevant_cohort
)
readr::write_csv(res25, "output/weekly_2_5_residual_pool.csv")

# Final live challenger: train once on every completed historical season through TRAIN_END.
cat("\n[2.5] Training final 2026 direct challengers...\n")
for (pos in POSITIONS) {
  train <- weekly |> dplyr::filter(position == pos, season <= TRAIN_END)
  fit <- wk25_fit_direct(train, "weekly_fppg", MODEL25_SAFE_FEATURES, seed = SEED + 2500L + match(pos, POSITIONS) * 100L)
  model_path <- paste0("models/weekly_2_5_direct_", pos, ".json")
  xgboost::xgb.save(fit$model, model_path)
  saveRDS(list(features = fit$features, nrounds = fit$nrounds, n_train = fit$n_train), paste0("models/weekly_2_5_direct_", pos, "_meta.rds"))
  imp <- tryCatch(xgboost::xgb.importance(feature_names = fit$features, model = fit$model), error = function(e) data.frame())
  if (nrow(imp)) {
    imp$position <- pos
    importance_rows[[length(importance_rows) + 1]] <- as.data.frame(imp)
  }
}
if (length(importance_rows)) readr::write_csv(dplyr::bind_rows(importance_rows), "output/weekly_2_5_direct_importance.csv")

lines <- c(
  "FANTASY MODEL 2.5 - ACCURACY TOURNAMENT",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  "",
  "WHAT CHANGED",
  "- Current production champion remains the benchmark per position.",
  "- Challenger is a direct XGBoost model trained only on explicit pre-kickoff features.",
  "- Every target season is predicted by a model trained only on earlier seasons.",
  "- Blend strength for a target year is selected only from earlier OOF seasons.",
  "- Promotion weights starter/relevant-player accuracy and requires year stability + paired confidence.",
  "",
  "PROMOTION SUMMARY"
)
for (i in seq_len(nrow(manifest25))) {
  r <- manifest25[i, ]
  lines <- c(lines, sprintf("%s: %s | alpha %.2f | starter MAE %.4f -> %.4f | starter RMSE %.4f -> %.4f | %s",
    r$position, ifelse(r$promoted_for_2026, "PROMOTE", "KEEP INCUMBENT"), r$final_alpha_2026,
    r$incumbent_starter_MAE, r$challenger_starter_MAE, r$incumbent_starter_RMSE, r$challenger_starter_RMSE,
    ifelse(nchar(r$failed_checks), paste0("failed: ", r$failed_checks), "all gates passed")))
}
writeLines(lines, "output/weekly_2_5_model_quality_report.txt")

cat("\n[2.5] VALIDATION COMPLETE\n")
print(metrics25 |> dplyr::select(position, promoted_for_2026, final_alpha_2026, MAE_improvement_pct, RMSE_improvement_pct, starter_MAE_improvement_pct, starter_RMSE_improvement_pct, starter_bootstrap_p_improves))
cat("\n[2.5] Production has NOT been rewritten by this validation step.\n")
cat("[2.5] Run pipeline/23_project_weekly_2026_2_5.R only after reviewing the promotion table, or use the guarded complete runner.\n")
