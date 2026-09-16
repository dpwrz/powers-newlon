# ============================================================
# STEP 19 - PROJECT CURRENT-WEEK 3.0 ROLE CONTEXT (SHADOW)
# ============================================================
source("config.R")
source("R/role_engine_30.R")
ensure_packages(c("dplyr", "readr"))

weekly_path <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
if (!file.exists(weekly_path)) stop("Missing ", weekly_path, ". Run the weekly projection pipeline first.")
if (!file.exists("output/role_3_0_model_manifest.csv")) source("pipeline/18_train_validate_role_models_3_0.R")

live <- readr::read_csv(weekly_path, show_col_types = FALSE)
live <- fm30_add_live_static_context(live, CURRENT_SEASON)
# Live team injury counts already exist in the weekly projection table. Historical
# absence-pressure features are zero until a validated current-season injury feed
# is available; Sleeper current injury metadata is consumed by the dynasty app.
live <- fm30_enrich_weekly_table(live, NULL)
manifest <- readr::read_csv("output/role_3_0_model_manifest.csv", show_col_types = FALSE)

for (i in seq_len(nrow(manifest))) {
  pos <- as.character(manifest$position[i]); target <- as.character(manifest$target[i])
  idx <- which(live$position == pos)
  if (!length(idx)) next
  base <- fm30_baseline_for_target(live[idx, , drop = FALSE], pos, target)
  model <- NULL
  path <- as.character(manifest$model_path[i])
  if (nzchar(path) && file.exists(path)) model <- readRDS(path)
  mp <- fm30_predict_role_model(model, live[idx, , drop = FALSE], target)
  promoted <- isTRUE(manifest$promoted[i])
  final <- if (promoted && any(is.finite(mp))) mp else base
  prefix <- paste0("role30_", target)
  live[[paste0(prefix, "_baseline")]] <- if (!paste0(prefix, "_baseline") %in% names(live)) NA_real_ else live[[paste0(prefix, "_baseline")]]
  live[[paste0(prefix, "_model")]] <- if (!paste0(prefix, "_model") %in% names(live)) NA_real_ else live[[paste0(prefix, "_model")]]
  live[[paste0(prefix, "_forecast")]] <- if (!paste0(prefix, "_forecast") %in% names(live)) NA_real_ else live[[paste0(prefix, "_forecast")]]
  live[[paste0(prefix, "_promoted")]] <- if (!paste0(prefix, "_promoted") %in% names(live)) FALSE else live[[paste0(prefix, "_promoted")]]
  live[[paste0(prefix, "_baseline")]][idx] <- base
  live[[paste0(prefix, "_model")]][idx] <- mp
  live[[paste0(prefix, "_forecast")]][idx] <- final
  live[[paste0(prefix, "_promoted")]][idx] <- promoted
}

pick_col <- function(name, fallback = 0) {
  if (name %in% names(live)) fm30_num(live[[name]]) else rep(fallback, nrow(live))
}
live$projected_targets_30 <- dplyr::coalesce(pick_col("role30_targets_forecast", NA_real_), pick_col("projected_targets", 0))
live$projected_carries_30 <- dplyr::coalesce(pick_col("role30_carries_forecast", NA_real_), pick_col("projected_carries", 0))
live$projected_pass_attempts_30 <- dplyr::coalesce(pick_col("role30_pass_attempts_forecast", NA_real_), pick_col("projected_pass_attempts", 0))
live$projected_target_share_30 <- dplyr::coalesce(pick_col("role30_target_share_week_forecast", NA_real_), pick_col("roll3_target_share", 0))
live$projected_carry_share_30 <- dplyr::coalesce(pick_col("role30_carry_share_week_forecast", NA_real_), pick_col("roll3_carry_share", 0))
live$projected_pass_share_30 <- dplyr::coalesce(pick_col("role30_pass_attempt_share_week_forecast", NA_real_), pick_col("roll3_pass_attempt_share", 0))
live$projected_snap_share_30 <- dplyr::coalesce(pick_col("role30_offense_pct_forecast", NA_real_), pick_col("roll3_offense_pct", 0))
live$role_uncertainty_30 <- pmin(1, 0.55 * fm30_num(live$role_regime_probability_30) + 0.45 * pmin(1, abs(fm30_num(live$role_regime_direction_30))))

readr::write_csv(live, ROLE30_LIVE_OUTPUT)
cat("[3.0 ROLE] Wrote shadow current-week role forecasts to ", ROLE30_LIVE_OUTPUT, ".\n", sep = "")
