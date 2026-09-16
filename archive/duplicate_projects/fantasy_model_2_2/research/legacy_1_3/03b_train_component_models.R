# ============================================================
# STEP 3B - TRAIN 1.3 POSITION COMPONENT ENSEMBLES
# ============================================================
source("config.R")
source("R/component_engine.R")
ensure_packages(c("dplyr", "readr", "rpart"))

path <- "data/processed/component_model_table.csv"
if (!file.exists(path)) stop("Missing component_model_table.csv. Run 02b_build_component_data.R first.")
df <- readr::read_csv(path, show_col_types = FALSE)

importance_rows <- list()
manifest_rows <- list()
for (i in seq_along(POSITIONS)) {
  pos <- POSITIONS[i]
  d <- df |> dplyr::filter(position == pos)
  if (nrow(d) < 50) next
  cat("[1.3 COMPONENT] Training ", pos, " component system on ", nrow(d), " player-seasons...\n", sep = "")
  models <- fit_component_models(d, pos, n_trees = COMPONENT_FINAL_TREES, seed = SEED + i * 50000)
  if (length(models) == 0) { warning("No component models trained for ", pos); next }
  saveRDS(models, paste0("models/component_models_", pos, ".rds"))

  imp <- component_model_importance(models, pos)
  if (nrow(imp) > 0) importance_rows[[length(importance_rows) + 1]] <- imp
  for (nm in names(models)) {
    manifest_rows[[length(manifest_rows) + 1]] <- data.frame(
      position = pos, component = nm, feature = models[[nm]]$features,
      stringsAsFactors = FALSE
    )
  }
  cat("[1.3 COMPONENT] ", pos, " components: ", paste(names(models), collapse = ", "), "\n", sep = "")
}

imp_all <- dplyr::bind_rows(importance_rows)
if (nrow(imp_all) > 0) readr::write_csv(imp_all, "output/component_feature_importance.csv")
manifest <- dplyr::bind_rows(manifest_rows)
if (nrow(manifest) > 0) readr::write_csv(manifest, "output/component_feature_manifest.csv")
message("Fantasy Model 1.3 component ensembles trained.")
