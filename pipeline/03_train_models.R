# ============================================================
# STEP 3 - TRAIN VALIDATED 1.2 DIRECT-FPPG GUARDRAIL + DECISION ENSEMBLES
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr", "rpart"))
engine <- resolve_model_engine()

df <- readr::read_csv("data/processed/model_table.csv", show_col_types = FALSE)
df <- add_v7_targets(df)
cat("[MODEL] Engine: ", engine,
    " | regression trees/position: ", RPART_ENSEMBLE_TREES,
    " | tier trees: ", TIER_ENSEMBLE_TREES,
    " | breakout trees: ", BREAKOUT_ENSEMBLE_TREES, "\n", sep = "")
cat("[MODEL] Final models use the full training window through ", TRAIN_END, ".\n", sep = "")

reg_importance_rows <- list()
class_importance_rows <- list()

feature_manifest <- dplyr::bind_rows(lapply(POSITIONS, function(pos) {
  feats <- get_model_features(pos, names(df))
  data.frame(
    position = pos,
    feature = feats,
    feature_group = ifelse(feats %in% POSITION_CONTEXT_FEATURES[[pos]], "position_context", "core"),
    stringsAsFactors = FALSE
  )
}))
readr::write_csv(feature_manifest, "output/position_feature_manifest.csv")

for (i in seq_along(POSITIONS)) {
  pos <- POSITIONS[i]
  d <- df |> dplyr::filter(position == pos, is.finite(target_fppg))
  if (nrow(d) < 50) { warning("Not enough data for ", pos); next }
  feature_cols <- get_model_features(pos, names(df))

  cat("[MODEL] Training ", pos, " regression ensemble (", nrow(d), " player-seasons, ",
      length(feature_cols), " features: ", length(intersect(feature_cols, POSITION_CONTEXT_FEATURES[[pos]])),
      " position-context)...\n", sep = "")
  reg_obj <- fit_fantasy_model(
    d, feature_cols, "target_fppg", engine,
    seed = SEED + i * 100,
    n_trees = RPART_ENSEMBLE_TREES
  )
  reg_obj$position <- pos
  saveRDS(reg_obj, paste0("models/model_", pos, ".rds"))

  cat("[MODEL] Training ", pos, " tier classifier...\n", sep = "")
  tier_obj <- fit_fantasy_classifier(
    d, feature_cols, "tier_target",
    class_levels = c("Depth", "Starter", "Elite"),
    seed = SEED + i * 100 + 10000,
    n_trees = TIER_ENSEMBLE_TREES
  )
  tier_obj$position <- pos
  saveRDS(tier_obj, paste0("models/tier_model_", pos, ".rds"))

  cat("[MODEL] Training ", pos, " breakout/upside classifier...\n", sep = "")
  breakout_obj <- fit_fantasy_classifier(
    d, feature_cols, "breakout_target",
    class_levels = c("No", "Yes"),
    seed = SEED + i * 100 + 20000,
    n_trees = BREAKOUT_ENSEMBLE_TREES
  )
  breakout_obj$position <- pos
  saveRDS(breakout_obj, paste0("models/breakout_model_", pos, ".rds"))

  imp <- get_feature_importance(reg_obj)
  if (nrow(imp) > 0) {
    imp$position <- pos
    imp$model_type <- "FPPG regression"
    reg_importance_rows[[length(reg_importance_rows) + 1]] <- imp
  }

  tier_imp <- get_classifier_importance(tier_obj)
  if (nrow(tier_imp) > 0) {
    tier_imp$position <- pos
    tier_imp$model_type <- "Fantasy tier classifier"
    class_importance_rows[[length(class_importance_rows) + 1]] <- tier_imp
  }

  breakout_imp <- get_classifier_importance(breakout_obj)
  if (nrow(breakout_imp) > 0) {
    breakout_imp$position <- pos
    breakout_imp$model_type <- "Breakout/upside classifier"
    class_importance_rows[[length(class_importance_rows) + 1]] <- breakout_imp
  }

  cat("[MODEL] ", pos, " complete.\n", sep = "")
}

reg_importance <- dplyr::bind_rows(reg_importance_rows)
if (nrow(reg_importance) > 0) readr::write_csv(reg_importance, "output/feature_importance.csv")
class_importance <- dplyr::bind_rows(class_importance_rows)
if (nrow(class_importance) > 0) readr::write_csv(class_importance, "output/classifier_feature_importance.csv")
message("Fantasy Model 2.0 direct-FPPG guardrail + fantasy decision ensembles trained.")
