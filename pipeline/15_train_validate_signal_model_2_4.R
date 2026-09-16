# ============================================================
# FANTASY MODEL 2.4 - HONEST SIGNAL ATTRIBUTION + VALIDATION
# ============================================================
# Uses the locked 2.3 / 2.3.2 OOF forecasts as the benchmark, then asks which
# additional pre-kickoff signals add UNIQUE value. Player-vs-defense response
# profiles are cross-fitted by season so a holdout never helps estimate its own
# matchup response.

source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr"))
source("R/weekly_engine.R")
source("R/weekly_engine_22.R")
source("R/weekly_engine_23.R")
source("R/weekly_engine_232.R")
source("R/weekly_engine_24.R")

set.seed(SEED)
dir.create("models", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

req <- c("output/weekly_2_3_validation_predictions.csv",
         "output/weekly_2_3_2_validation_predictions.csv",
         "data/processed/player_talent_profiles_2_4.csv",
         "data/processed/player_talent_raw_2_4.csv")
miss <- req[!file.exists(req)]
if (length(miss)) stop("Missing 2.4 prerequisite(s): ", paste(miss, collapse = ", "), ". Run 14_build_talent_context_2_4.R first.")

feature_path <- if (file.exists("data/processed/weekly_model_table_2_3.csv")) "data/processed/weekly_model_table_2_3.csv" else "data/processed/weekly_model_table_2_2.csv"
if (!file.exists(feature_path)) stop("Missing historical weekly feature store. Do NOT rebuild PBP; restore data/processed/weekly_model_table_2_3.csv from the completed 2.3 project.")

cat("\n[2.4 VALIDATE] Loading locked OOF predictions + pre-kickoff feature stores...\n")
oof <- readr::read_csv("output/weekly_2_3_2_validation_predictions.csv", show_col_types = FALSE, progress = FALSE)
weekly <- readr::read_csv(feature_path, show_col_types = FALSE, progress = FALSE)
talent_raw <- readr::read_csv("data/processed/player_talent_raw_2_4.csv", show_col_types = FALSE, progress = FALSE)
talent_oof <- if (file.exists("data/processed/talent_prior_oof_2_4.csv")) readr::read_csv("data/processed/talent_prior_oof_2_4.csv", show_col_types = FALSE, progress = FALSE) else data.frame()
defense <- if (file.exists("data/processed/defense_style_context_2_4.csv")) tryCatch(readr::read_csv("data/processed/defense_style_context_2_4.csv", show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
xfp <- if (file.exists("data/processed/expected_opportunity_history_2_4.csv")) tryCatch(readr::read_csv("data/processed/expected_opportunity_history_2_4.csv", show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()

if (!nrow(oof)) stop("2.3.2 OOF file is empty.")
oof$season <- as.integer(oof$season); oof$week <- as.integer(oof$week)
weekly$season <- as.integer(weekly$season); weekly$week <- as.integer(weekly$week)

# Attach the full set of EXISTING model-whitelisted pre-kickoff variables, not
# raw current-game box-score outcomes.
feature_union <- unique(c(
  unlist(lapply(POSITIONS, function(p) get_weekly_features21(p, names(weekly)))),
  unlist(lapply(POSITIONS, function(p) get_features22(p, "full", names(weekly)))),
  intersect(c("weekly_age", "weekly_experience", "weekly_is_rookie", "games_played_prior", "season_week",
              "starter_cohort", "relevant_cohort", "baseline_rank", "player_display_name", "team", "opponent"), names(weekly))
))
keys <- c("season", "week", "player_id")
extra <- setdiff(intersect(feature_union, names(weekly)), names(oof))
ctx <- weekly |> dplyr::select(dplyr::all_of(unique(c(keys, extra)))) |> dplyr::distinct(season, week, player_id, .keep_all = TRUE)
oof <- oof |> dplyr::left_join(ctx, by = keys)
rm(ctx, weekly); gc()

# Historical talent features use chronological draft-class OOF talent prior.
# Raw combine/college measurements themselves are pre-NFL facts, so they are
# safe; weekly entry into the final model is multiplied by the experience gate.
talent_hist <- talent_raw
if (nrow(talent_oof)) {
  to <- talent_oof |> dplyr::select(player_id, talent_prior_fppg_24, talent_prior_source_24) |> dplyr::distinct(player_id, .keep_all = TRUE)
  talent_hist <- talent_hist |> dplyr::left_join(to, by = "player_id")
}
if (!"talent_prior_fppg_24" %in% names(talent_hist)) talent_hist$talent_prior_fppg_24 <- 0

# Attach lagged opponent style by opponent, and lagged xFP by player-week.
if (nrow(defense) && all(c("season", "week", "defense") %in% names(defense)) && "opponent" %in% names(oof)) {
  defense$season <- as.integer(defense$season); defense$week <- as.integer(defense$week)
  dk <- intersect(c("season", "week", "defense", MODEL24_DEFENSE_STYLE_FEATURES, "n_prior_games_24"), names(defense))
  oof <- oof |> dplyr::left_join(defense[, dk, drop = FALSE], by = c("season", "week", "opponent" = "defense"))
}
if (nrow(xfp) && all(c("season", "week", "player_id") %in% names(xfp))) {
  xfp$season <- as.integer(xfp$season); xfp$week <- as.integer(xfp$week)
  xk <- intersect(c("season", "week", "player_id", "xfp_roll3_24", "xfp_roll5_24", "fpoe_roll3_24", "fpoe_roll5_24"), names(xfp))
  oof <- oof |> dplyr::left_join(xfp[, xk, drop = FALSE] |> dplyr::distinct(season, week, player_id, .keep_all = TRUE), by = c("season", "week", "player_id"))
}

# Keep explicit NA for missing sources until ridge preparation. This lets the
# missingness flags / experience gate do their jobs instead of fabricating data.
oof <- wk24_add_talent_features(oof, talent_hist)
oof$archetype_24 <- wk24_archetype(oof)

# 2.3.2 engineered expectation/disagreement features are also pre-kickoff.
oof <- engineer_signal_features232(oof, "honest_final_fppg")

# A player-specific response feature in a training year must be built only from
# EARLIER seasons. This function creates the leakage-safe response history used
# to fit the signal model.
crossfit_responses24 <- function(d, pos, base_col = "locked_base_fppg_24") {
  if (!nrow(d)) return(d)
  years <- sort(unique(as.integer(d$season))); parts <- list()
  for (yy in years) {
    tr <- d[as.integer(d$season) < yy, , drop = FALSE]
    va <- d[as.integer(d$season) == yy, , drop = FALSE]
    if (!nrow(va)) next
    mp <- wk24_fit_response_map(tr, pos, base_col)
    va <- wk24_apply_response_map(va, mp, pos)
    parts[[length(parts) + 1]] <- va
  }
  dplyr::bind_rows(parts)
}

validation_years <- sort(unique(as.integer(oof$season)))
cat("[2.4 VALIDATE] Honest weekly seasons: ", paste(validation_years, collapse = ", "), "\n", sep = "")
cat("[2.4 VALIDATE] Signal rule: target year can use only earlier OOF outcomes.\n")

pred_rows <- list(); base_rows <- list(); model_rows <- list(); rolling_audit_rows <- list(); rolling_perm_rows <- list(); rolling_coef_rows <- list()

for (yr in validation_years) {
  for (pos in POSITIONS) {
    prior <- oof |> dplyr::filter(position == pos, season < yr)
    cur <- oof |> dplyr::filter(position == pos, season == yr)
    if (nrow(cur) < 15) next

    lock <- if (nrow(prior) >= 60) wk24_choose_locked_base(prior) else list(column = "honest_final_fppg", method = "2.3")
    base_col <- lock$column
    if (!base_col %in% names(cur)) { base_col <- "honest_final_fppg"; lock$method <- "2.3" }
    prior$locked_base_fppg_24 <- if (nrow(prior)) wk24_num(prior[[base_col]]) else numeric()
    cur$locked_base_fppg_24 <- wk24_num(cur[[base_col]])

    # Recompute gap features relative to the actual locked benchmark chosen for
    # this target year.
    if (nrow(prior)) prior <- engineer_signal_features232(prior, "locked_base_fppg_24")
    cur <- engineer_signal_features232(cur, "locked_base_fppg_24")

    cat("[2.4 VALIDATE] ", yr, " ", pos, " | prior OOF ", nrow(prior), " | locked ", lock$method, "\n", sep = "")
    base_rows[[length(base_rows) + 1]] <- data.frame(target_year = yr, position = pos, prior_oof_n = nrow(prior),
      locked_method = lock$method, locked_column = base_col, stringsAsFactors = FALSE)

    # Cross-fitted player/archetype response features in prior history.
    prior_cf <- if (nrow(prior)) crossfit_responses24(prior, pos, "locked_base_fppg_24") else prior
    sig <- wk24_fit_signal_model(prior_cf, pos, "locked_base_fppg_24")

    # Current target-year player response is estimated on ALL earlier OOF rows,
    # which is legal because none of the target-year outcomes enter the map.
    response_map <- wk24_fit_response_map(prior, pos, "locked_base_fppg_24")
    cur <- wk24_apply_response_map(cur, response_map, pos)
    correction <- wk24_predict_signal(sig, cur, pos)
    cur$signal_correction_24 <- correction
    cur$projected_weekly_fppg_24 <- pmax(0, cur$locked_base_fppg_24 + correction)
    cur$locked_method_24 <- lock$method
    cur$signal_model_enabled_24 <- isTRUE(sig$enabled)
    cur$signal_lambda_24 <- ifelse(is.null(sig$lambda), NA_real_, sig$lambda)
    cur$signal_inner_utility_24 <- ifelse(is.null(sig$inner_utility), 0, sig$inner_utility)
    cur$error_24 <- wk24_num(cur$actual_fppg) - wk24_num(cur$projected_weekly_fppg_24)
    cur$abs_error_24 <- abs(cur$error_24)
    pred_rows[[length(pred_rows) + 1]] <- cur

    model_rows[[length(model_rows) + 1]] <- data.frame(target_year = yr, position = pos, locked_method = lock$method,
      signal_enabled = isTRUE(sig$enabled), lambda = ifelse(is.null(sig$lambda), NA_real_, sig$lambda),
      inner_utility = ifelse(is.null(sig$inner_utility), 0, sig$inner_utility),
      n_signal_features = length(sig$features), signal_features = paste(sig$features, collapse = ";"),
      n_response_features = length(response_map$features), stringsAsFactors = FALSE)

    if (!is.null(sig$audit) && nrow(sig$audit)) rolling_audit_rows[[length(rolling_audit_rows) + 1]] <- sig$audit |> dplyr::mutate(target_year = yr)
    cc <- wk24_coefficient_table(sig, pos)
    if (nrow(cc)) rolling_coef_rows[[length(rolling_coef_rows) + 1]] <- cc |> dplyr::mutate(target_year = yr)
    pi <- wk24_permutation_importance(sig, cur, pos, "locked_base_fppg_24")
    if (nrow(pi)) rolling_perm_rows[[length(rolling_perm_rows) + 1]] <- pi |> dplyr::mutate(target_year = yr)
  }
}

val <- dplyr::bind_rows(pred_rows)
if (!nrow(val)) stop("2.4 produced no validation predictions.")
readr::write_csv(val, "output/weekly_2_4_validation_predictions.csv")
readr::write_csv(dplyr::bind_rows(base_rows), "output/weekly_2_4_rolling_locked_base.csv")
readr::write_csv(dplyr::bind_rows(model_rows), "output/weekly_2_4_rolling_signal_models.csv")
if (length(rolling_audit_rows)) readr::write_csv(dplyr::bind_rows(rolling_audit_rows), "output/weekly_2_4_signal_attribution_by_year.csv")
if (length(rolling_perm_rows)) readr::write_csv(dplyr::bind_rows(rolling_perm_rows), "output/weekly_2_4_permutation_importance_by_year.csv")
if (length(rolling_coef_rows)) readr::write_csv(dplyr::bind_rows(rolling_coef_rows), "output/weekly_2_4_coefficients_by_year.csv")

# ------------------------------------------------------------
# Honest validation metrics + strict promotion
# ------------------------------------------------------------
metric_rows <- list(); cohort_rows <- list()
for (pos in POSITIONS) {
  d <- val |> dplyr::filter(position == pos)
  if (!nrow(d)) next
  starter <- if ("starter_cohort" %in% names(d)) d$starter_cohort %in% TRUE else rep(FALSE, nrow(d))
  mb <- wk24_metrics(d$actual_fppg, d$locked_base_fppg_24, starter)
  m24 <- wk24_metrics(d$actual_fppg, d$projected_weekly_fppg_24, starter)
  promote <- wk24_strict_promote(mb, m24)
  metric_rows[[length(metric_rows) + 1]] <- data.frame(
    position = pos, seasons_tested = length(unique(d$season)), n = nrow(d),
    locked_MAE = mb$MAE, model24_MAE = m24$MAE,
    locked_RMSE = mb$RMSE, model24_RMSE = m24$RMSE,
    locked_correlation = mb$correlation, model24_correlation = m24$correlation,
    locked_rank_correlation = mb$rank_correlation, model24_rank_correlation = m24$rank_correlation,
    locked_starter_MAE = mb$starter_MAE, model24_starter_MAE = m24$starter_MAE,
    MAE_improvement_pct = 100 * (mb$MAE - m24$MAE) / mb$MAE,
    RMSE_improvement_pct = 100 * (mb$RMSE - m24$RMSE) / mb$RMSE,
    promoted_for_2026 = promote, stringsAsFactors = FALSE)

  for (coh in c("All", "Relevant", "Starter")) {
    dd <- d
    if (coh == "Relevant" && "relevant_cohort" %in% names(dd)) dd <- dd[dd$relevant_cohort %in% TRUE, , drop = FALSE]
    if (coh == "Starter" && "starter_cohort" %in% names(dd)) dd <- dd[dd$starter_cohort %in% TRUE, , drop = FALSE]
    if (nrow(dd) < 10) next
    a <- wk24_metrics(dd$actual_fppg, dd$locked_base_fppg_24)
    b <- wk24_metrics(dd$actual_fppg, dd$projected_weekly_fppg_24)
    cohort_rows[[length(cohort_rows) + 1]] <- data.frame(position = pos, cohort = coh, n = nrow(dd),
      locked_MAE = a$MAE, model24_MAE = b$MAE, locked_RMSE = a$RMSE, model24_RMSE = b$RMSE,
      locked_correlation = a$correlation, model24_correlation = b$correlation,
      locked_rank_correlation = a$rank_correlation, model24_rank_correlation = b$rank_correlation, stringsAsFactors = FALSE)
  }
}
metrics <- dplyr::bind_rows(metric_rows)
cohorts <- dplyr::bind_rows(cohort_rows)
readr::write_csv(metrics, "output/weekly_2_4_validation_metrics.csv")
readr::write_csv(cohorts, "output/weekly_2_4_cohort_metrics.csv")
readr::write_csv(metrics |> dplyr::select(position, promoted_for_2026, dplyr::everything()), "output/weekly_2_4_promotion.csv")

# Honest residual pool + confidence calibration. The risk score is deliberately
# simple and pre-kickoff: model disagreement, correction magnitude, response
# magnitude, recent volatility and rookie/talent uncertainty.
val$risk_score_24 <- wk24_num(val$model_disagreement_23) +
  0.70 * abs(wk24_num(val$signal_correction_24)) +
  0.30 * abs(wk24_num(val$response_adjustment_raw_24)) +
  0.25 * wk24_num(val$roll3_fppg_sd) +
  0.40 * wk24_num(val$talent_weight_24) * abs(wk24_num(val$talent_gap_weighted_24))
res_pool24 <- val |> dplyr::transmute(position, season, week, player_id, projected_weekly_fppg_24,
  residual = actual_fppg - projected_weekly_fppg_24, risk_score_24, signal_correction_24,
  response_adjustment_raw_24, talent_weight_24)
readr::write_csv(res_pool24, "output/weekly_2_4_residual_pool.csv")
conf_rows <- list()
for (pos in POSITIONS) {
  dd <- val |> dplyr::filter(position == pos)
  if (nrow(dd) < 60) next
  q <- stats::quantile(dd$risk_score_24, probs = c(1/3, 2/3), na.rm = TRUE, names = FALSE)
  dd$confidence_24 <- ifelse(dd$risk_score_24 <= q[1], "High", ifelse(dd$risk_score_24 <= q[2], "Medium", "Low"))
  zz <- dd |> dplyr::group_by(confidence_24) |> dplyr::summarise(n = dplyr::n(), expected_abs_error = mean(abs_error_24, na.rm = TRUE), .groups = "drop")
  zz$position <- pos; zz$q33_risk <- q[1]; zz$q67_risk <- q[2]
  conf_rows[[length(conf_rows) + 1]] <- zz |> dplyr::select(position, confidence = confidence_24, n, expected_abs_error, q33_risk, q67_risk)
}
readr::write_csv(dplyr::bind_rows(conf_rows), "output/weekly_2_4_confidence_calibration.csv")

# ------------------------------------------------------------
# Final 2026 training objects + comprehensive signal map
# ------------------------------------------------------------
final_model_rows <- list(); final_audit_rows <- list(); final_coef_rows <- list(); response_summary_rows <- list(); locked_rows <- list()
for (pos in POSITIONS) {
  d <- oof |> dplyr::filter(position == pos)
  if (!nrow(d)) next
  lock <- wk24_choose_locked_base(d)
  base_col <- lock$column
  if (!base_col %in% names(d)) { base_col <- "honest_final_fppg"; lock$method <- "2.3" }
  d$locked_base_fppg_24 <- wk24_num(d[[base_col]])
  d <- engineer_signal_features232(d, "locked_base_fppg_24")
  d_cf <- crossfit_responses24(d, pos, "locked_base_fppg_24")
  sig <- wk24_fit_signal_model(d_cf, pos, "locked_base_fppg_24")
  rsp <- wk24_fit_response_map(d, pos, "locked_base_fppg_24")
  saveRDS(sig, paste0("models/weekly_2_4_signal_", pos, ".rds"))
  saveRDS(rsp, paste0("models/weekly_2_4_response_", pos, ".rds"))

  locked_rows[[length(locked_rows) + 1]] <- data.frame(position = pos, locked_method_2026 = lock$method,
    locked_column_2026 = base_col, stringsAsFactors = FALSE)
  final_model_rows[[length(final_model_rows) + 1]] <- data.frame(position = pos, locked_method_2026 = lock$method,
    signal_enabled = isTRUE(sig$enabled), lambda = ifelse(is.null(sig$lambda), NA_real_, sig$lambda),
    inner_utility = ifelse(is.null(sig$inner_utility), 0, sig$inner_utility), n_signal_features = length(sig$features),
    signal_features = paste(sig$features, collapse = ";"), stringsAsFactors = FALSE)
  if (!is.null(sig$audit) && nrow(sig$audit)) final_audit_rows[[length(final_audit_rows) + 1]] <- sig$audit
  cc <- wk24_coefficient_table(sig, pos); if (nrow(cc)) final_coef_rows[[length(final_coef_rows) + 1]] <- cc

  if (length(rsp$maps)) for (f in names(rsp$maps)) {
    mp <- rsp$maps[[f]]
    response_summary_rows[[length(response_summary_rows) + 1]] <- data.frame(position = pos, feature = f, level = "position", key = pos,
      n = nrow(d), slope = mp$position_slope, feature_mean = mp$mean, stringsAsFactors = FALSE)
    if (nrow(mp$archetype)) response_summary_rows[[length(response_summary_rows) + 1]] <- mp$archetype |>
      dplyr::transmute(position = pos, feature = f, level = "archetype", key, n, slope, feature_mean = mp$mean)
    if (nrow(mp$player)) response_summary_rows[[length(response_summary_rows) + 1]] <- mp$player |>
      dplyr::transmute(position = pos, feature = f, level = "player", key, n, slope, feature_mean = mp$mean)
  }
}

final_models <- dplyr::bind_rows(final_model_rows)
final_audit <- dplyr::bind_rows(final_audit_rows)
final_coef <- dplyr::bind_rows(final_coef_rows)
response_summary <- dplyr::bind_rows(response_summary_rows)
locked_final <- dplyr::bind_rows(locked_rows)
readr::write_csv(final_models, "output/weekly_2_4_selected_signal_model.csv")
readr::write_csv(locked_final, "output/weekly_2_4_locked_benchmark.csv")
readr::write_csv(response_summary, "output/weekly_2_4_player_response_summary.csv")

# Aggregate only the truly honest target-year permutation tests, then combine
# with descriptive correlations and standardized conditional ridge weights.
perm_all <- dplyr::bind_rows(rolling_perm_rows)
perm_ag <- if (nrow(perm_all)) perm_all |> dplyr::group_by(position, feature) |>
  dplyr::summarise(permutation_years = dplyr::n_distinct(target_year),
    oof_delta_MAE_if_permuted = mean(delta_MAE_if_permuted, na.rm = TRUE),
    oof_delta_RMSE_if_permuted = mean(delta_RMSE_if_permuted, na.rm = TRUE),
    oof_delta_correlation_if_permuted = mean(delta_correlation_if_permuted, na.rm = TRUE),
    oof_delta_rank_correlation_if_permuted = mean(delta_rank_correlation_if_permuted, na.rm = TRUE), .groups = "drop") else data.frame()
attr <- final_audit
if (nrow(final_coef)) attr <- dplyr::full_join(attr, final_coef, by = c("position", "feature"))
if (nrow(perm_ag)) attr <- dplyr::full_join(attr, perm_ag, by = c("position", "feature"))
if (nrow(attr)) attr <- attr |> dplyr::group_by(position) |>
  dplyr::mutate(unique_predictive_rank_24 = dplyr::min_rank(dplyr::desc(dplyr::coalesce(oof_delta_MAE_if_permuted, 0)))) |>
  dplyr::ungroup() |> dplyr::arrange(position, unique_predictive_rank_24, dplyr::desc(signal_score))
readr::write_csv(attr, "output/weekly_2_4_signal_attribution.csv")

# Human-readable quality report emphasizes the exact distinction the user asked
# for: raw relationship vs conditional unique value vs player-specific effect.
lines <- c(
  paste0("FANTASY MODEL 2.4 SIGNAL-FIRST QUALITY REPORT - ", CURRENT_SEASON), "",
  "WHAT 2.4 MEASURES",
  "1) Raw Pearson/Spearman relationship to actual weekly fantasy output.",
  "2) Partial/residual relationship after controlling the locked forecast.",
  "3) Year-to-year direction stability.",
  "4) Standardized ridge coefficient = conditional model weight after redundancy pruning.",
  "5) OOF permutation delta = how much MAE/RMSE/correlation degrades when a signal is destroyed.",
  "6) Player/archetype response slopes = whether the same defense style affects different player types differently.",
  "7) College/combine/draft features are experience-gated so their weekly influence fades as NFL evidence accumulates.",
  "", "HONEST POSITION RESULTS")
for (i in seq_len(nrow(metrics))) {
  r <- metrics[i, ]
  lines <- c(lines, sprintf("%s | n=%d | locked MAE %.3f -> 2.4 %.3f | RMSE %.3f -> %.3f | corr %.3f -> %.3f | rank %.3f -> %.3f | starter MAE %.3f -> %.3f | promote=%s",
    r$position, r$n, r$locked_MAE, r$model24_MAE, r$locked_RMSE, r$model24_RMSE,
    r$locked_correlation, r$model24_correlation, r$locked_rank_correlation, r$model24_rank_correlation,
    r$locked_starter_MAE, r$model24_starter_MAE, ifelse(r$promoted_for_2026, "YES", "NO")))
}
lines <- c(lines, "", "KEY FILES",
  "weekly_2_4_signal_attribution.csv = global + conditional + OOF unique predictive value by variable",
  "weekly_2_4_player_response_summary.csv = position/archetype/player response to blitz/pressure/coverage style",
  "rookie_talent_signal_audit_2_4.csv = college/combine/draft relationships and standardized rookie talent weights",
  "weekly_2_4_validation_metrics.csv = strict apples-to-apples locked benchmark vs 2.4",
  "weekly_2_4_promotion.csv = strict production gate")
writeLines(lines, "output/weekly_2_4_model_quality_report.txt")

cat("\n[2.4 VALIDATE] COMPLETE. Honest rows: ", nrow(val), "\n", sep = "")
print(metrics)
