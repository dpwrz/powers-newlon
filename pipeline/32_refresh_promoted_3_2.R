# ============================================================
# FANTASY MODEL 3.2 - REFIT PROMOTED ARCHITECTURES
# ============================================================
# Architecture/factors are locked by pipeline/31. This step only refreshes the
# weights with newly finalized weeks. It cannot promote a new position or alter
# the selected feature architecture.

source("config.R")
ensure_packages(c("dplyr","readr","xgboost"))
source("R/weekly_engine_31.R")
source("R/weekly_engine_32.R")

promotion_path <- "output/weekly_3_2_promotion.csv"
selection_path <- "output/weekly_3_2_selection.csv"
historical_path <- "data/processed/weekly_model_table_2_3.csv"
live_path <- file.path("data","processed",paste0("weekly_training_live_", CURRENT_SEASON, ".csv"))
config_path <- "output/weekly_3_1_live_config.csv"

required <- c(promotion_path, selection_path, historical_path, live_path, config_path)
missing <- required[!file.exists(required)]
if (length(missing)) {
  cat("[3.2 REFIT] Missing prerequisite(s): ", paste(missing, collapse = ", "),
      ". Keeping existing 3.2 models.\n", sep = "")
  return(invisible(NULL))
}

promotion <- readr::read_csv(promotion_path, show_col_types = FALSE, progress = FALSE)
selection <- readr::read_csv(selection_path, show_col_types = FALSE, progress = FALSE)
historical <- readr::read_csv(historical_path, show_col_types = FALSE, progress = FALSE)
live <- readr::read_csv(live_path, show_col_types = FALSE, progress = FALSE)
config31 <- readr::read_csv(config_path, show_col_types = FALSE, progress = FALSE)

norm32_refit <- function(d) {
  if (!nrow(d)) return(d)
  d$season <- suppressWarnings(as.integer(d$season))
  d$week <- suppressWarnings(as.integer(d$week))
  d$position <- toupper(as.character(d$position))
  d$player_id <- as.character(d$player_id)
  d
}
historical <- norm32_refit(historical) |>
  dplyr::filter(season <= TRAIN_END, position %in% POSITIONS)
live <- norm32_refit(live) |>
  dplyr::filter(season == CURRENT_SEASON, position %in% POSITIONS)
trained_week <- max(live$week, na.rm = TRUE)
if (!is.finite(trained_week)) {
  cat("[3.2 REFIT] No finalized live rows; keeping existing models.\n")
  return(invisible(NULL))
}

config_for_refit32 <- function(pos) {
  cc <- config31[config31$position == pos, , drop = FALSE]
  grid <- wk31_param_grid(pos)
  if (nrow(cc) && "config_id" %in% names(cc)) {
    z <- grid[grid$config_id == as.character(cc$config_id[1]), , drop = FALSE]
    if (nrow(z)) return(z[1, , drop = FALSE])
  }
  grid[1, , drop = FALSE]
}

stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
manifest_rows <- list()

for (pos in POSITIONS) {
  pr <- promotion[promotion$position == pos, , drop = FALSE]
  promote <- nrow(pr) && "promoted_32" %in% names(pr) &&
    isTRUE(wk25_bool(pr$promoted_32[1]))
  if (!promote) next

  sel <- selection[selection$position == pos, , drop = FALSE]
  if (!nrow(sel)) stop("[3.2 REFIT] Missing locked selection for ", pos)

  variant <- as.character(sel$selected_variant[1])
  sf <- if ("selected_factors" %in% names(sel)) as.character(sel$selected_factors[1]) else ""
  if (is.na(sf)) sf <- ""
  factors <- if (nzchar(sf)) strsplit(sf, "\\+", fixed = FALSE)[[1]] else character()

  train <- dplyr::bind_rows(
    historical |> dplyr::filter(position == pos),
    live |> dplyr::filter(position == pos, week <= trained_week)
  )
  train <- wk32_add_factors(train)
  features <- unique(c(wk32_variant_features(train, pos, variant), factors))
  cfg <- config_for_refit32(pos)

  fit <- wk31_fit(
    train, pos, cfg, features = features,
    seed = SEED + 33200L + trained_week * 100L + match(pos, POSITIONS)
  )

  model_path <- paste0("models/weekly_3_2_live_", pos, ".json")
  meta_path <- paste0("models/weekly_3_2_live_", pos, "_meta.rds")

  if (file.exists(model_path) || file.exists(meta_path)) {
    archive_dir <- file.path("models","archive","weekly_3_2_live",stamp)
    dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
    if (file.exists(model_path)) file.copy(model_path, file.path(archive_dir, basename(model_path)), overwrite = FALSE)
    if (file.exists(meta_path)) file.copy(meta_path, file.path(archive_dir, basename(meta_path)), overwrite = FALSE)
  }

  alpha <- as.numeric(MODEL31_WEEKLY_ALPHA[[pos]])
  xgboost::xgb.save(fit$model, model_path)
  saveRDS(list(
    position = pos,
    features = features,
    selected_variant = variant,
    selected_factors = sf,
    n_features = length(features),
    config = cfg,
    nrounds = fit$nrounds,
    n_train = fit$n_train,
    trained_through_season = CURRENT_SEASON,
    trained_through_week = trained_week,
    blend_alpha = alpha,
    architecture_locked_by = "pipeline/31_optimize_signals_3_2.R",
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ), meta_path)

  manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
    position = pos,
    trained_through_week = trained_week,
    selected_variant = variant,
    selected_factors = sf,
    n_features = length(features),
    n_train = fit$n_train,
    blend_alpha = alpha,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  cat("[3.2 REFIT] ", pos, " refreshed through Week ", trained_week,
      " | variant=", variant, " | factors=", ifelse(nzchar(sf), sf, "none"),
      " | features=", length(features), ".\n", sep = "")
}

manifest <- dplyr::bind_rows(manifest_rows)
if (nrow(manifest)) {
  readr::write_csv(manifest, "output/weekly_3_2_refresh_manifest.csv")
}
invisible(manifest)
