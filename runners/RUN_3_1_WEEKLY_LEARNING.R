# ============================================================
# FANTASY MODEL 3.1 - GUARDED WEEKLY ONLINE LEARNING
# ============================================================
# Retrains fixed, historically validated 3.1 XGBoost configurations using:
#   immutable 2016-2025 history + fully captured completed 2026 weeks.
#
# Historical data is checksum-protected and never rewritten.
# Current-season data is append-only/revisioned.
# Weekly candidates are versioned. Production changes only after OOS gates pass.

find_project_root31_learning <- function(start = getwd()) {
  cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(cur, "config.R")) &&
        dir.exists(file.path(cur, "R")) &&
        dir.exists(file.path(cur, "pipeline"))) return(cur)
    parent <- dirname(cur)
    if (identical(parent, cur)) stop("Could not locate Fantasy Model project root.")
    cur <- parent
  }
}

root <- find_project_root31_learning()
setwd(root)

source("config.R")
ensure_packages(c("dplyr","readr","xgboost","jsonlite"))
source("R/weekly_engine_31.R")
source("R/weekly_learning_31.R")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create("models/weekly_3_1_incremental", recursive = TRUE, showWarnings = FALSE)
dir.create("models/weekly_3_1_production_history", recursive = TRUE, showWarnings = FALSE)

season <- CURRENT_SEASON
policy_version <- "31_online_v1"
current_weight <- suppressWarnings(as.numeric(Sys.getenv("FM_CURRENT_SEASON_WEIGHT", unset = "1.5")))
if (!is.finite(current_weight)) current_weight <- 1.5
current_weight <- pmin(3, pmax(1, current_weight))

historical_path <- "data/processed/weekly_model_table_2_3.csv"
feature_path <- file.path("data","processed",paste0("weekly_learning_features_", season, ".csv"))
accuracy_path <- file.path("output",paste0("live_accuracy_player_weeks_", season, ".csv"))
ledger_path <- file.path("data","processed",paste0("weekly_learning_ledger_", season, ".csv"))
current_path <- file.path("data","processed",paste0("weekly_learning_current_", season, ".csv"))
coverage_path <- file.path("output",paste0("weekly_3_1_online_coverage_", season, ".csv"))
oos_path <- file.path("output",paste0("weekly_3_1_online_oos_", season, ".csv"))
metric_path <- file.path("output",paste0("weekly_3_1_online_metrics_", season, ".csv"))
manifest_path <- file.path("output",paste0("weekly_3_1_online_learning_manifest_", season, ".csv"))
history_path <- file.path("output",paste0("weekly_3_1_online_learning_history_", season, ".csv"))

if (!file.exists(historical_path)) stop("Missing immutable historical weekly table: ", historical_path)
hist_md5_before <- wl31_file_md5(historical_path)

cat("\n============================================================\n")
cat(" FANTASY MODEL 3.1 - GUARDED WEEKLY ONLINE LEARNING\n")
cat("============================================================\n")
cat("[3.1 LEARNING] Historical base MD5: ", hist_md5_before, "\n", sep = "")
cat("[3.1 LEARNING] Current-season row weight: ", current_weight, "\n", sep = "")

current <- wl31_update_ledger(
  feature_path = feature_path,
  accuracy_path = accuracy_path,
  ledger_path = ledger_path,
  current_path = current_path,
  season = season,
  current_season_weight = current_weight
)

accuracy <- wl31_read_csv(accuracy_path)
coverage <- wl31_coverage_by_week(current, accuracy)
if (nrow(coverage)) wl31_atomic_write_csv(coverage, coverage_path)
eligible_weeks <- wl31_eligible_weeks(coverage)

cat("[3.1 LEARNING] Fully captured eligible weeks: ",
    if (length(eligible_weeks)) paste(eligible_weeks, collapse = ",") else "none",
    "\n", sep = "")

if (!nrow(current) || !length(eligible_weeks)) {
  status <- data.frame(
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    policy_version = policy_version,
    season = season,
    trained_through_week = NA_integer_,
    historical_rows = NA_integer_,
    current_season_rows = nrow(current),
    eligible_weeks = if (length(eligible_weeks)) paste(eligible_weeks, collapse = ",") else "",
    decision = "NO_ELIGIBLE_COMPLETED_WEEK",
    stringsAsFactors = FALSE
  )
  wl31_atomic_write_csv(status, manifest_path)
  hist_md5_after <- wl31_file_md5(historical_path)
  if (!identical(hist_md5_before, hist_md5_after)) stop("Historical training table changed unexpectedly.")
  cat("[3.1 LEARNING] Nothing eligible to retrain yet. Historical data preserved.\n")
  quit(save = "no", status = 0)
}

historical <- readr::read_csv(historical_path, show_col_types = FALSE, progress = FALSE)
historical$season <- suppressWarnings(as.integer(historical$season))
historical$week <- suppressWarnings(as.integer(historical$week))
historical$player_id <- as.character(historical$player_id)
historical$position <- toupper(as.character(historical$position))

if (any(historical$season > TRAIN_END, na.rm = TRUE)) {
  stop("Immutable historical table unexpectedly contains rows after TRAIN_END=", TRAIN_END)
}

required_hist <- c("season","week","player_id","position","weekly_fppg",MODEL31_SAFE_FEATURES)
missing_hist <- setdiff(required_hist, names(historical))
missing_current <- setdiff(required_hist, names(current))
if (length(missing_hist)) stop("Historical table missing required 3.1 column(s): ", paste(missing_hist, collapse = ", "))
if (length(missing_current)) stop("Current learning table missing required 3.1 column(s): ", paste(missing_current, collapse = ", "))

hist_train <- historical |>
  dplyr::select(dplyr::all_of(required_hist)) |>
  dplyr::mutate(online_recency_weight_31 = 1)

cur_train <- current |>
  dplyr::filter(week %in% eligible_weeks) |>
  dplyr::select(dplyr::all_of(required_hist), dplyr::any_of("online_recency_weight_31")) |>
  dplyr::mutate(online_recency_weight_31 = current_weight)

promotion_hist <- wl31_read_csv("output/weekly_3_1_promotion.csv")
manifest_hist <- wl31_read_csv("output/weekly_3_1_champion_manifest.csv")

get_hist_promoted <- function(pos) {
  z <- promotion_hist[promotion_hist$position == pos, , drop = FALSE]
  nrow(z) > 0 && isTRUE(wl31_bool(z$promoted_for_2026[1]))
}

get_alpha31 <- function(pos, meta = NULL) {
  if (!is.null(meta) && is.list(meta) && !is.null(meta$final_alpha_2026)) {
    a <- suppressWarnings(as.numeric(meta$final_alpha_2026))
    if (is.finite(a)) return(a)
  }
  z <- manifest_hist[manifest_hist$position == pos, , drop = FALSE]
  if (nrow(z) && "final_alpha_2026" %in% names(z)) {
    a <- suppressWarnings(as.numeric(z$final_alpha_2026[1]))
    if (is.finite(a)) return(a)
  }
  0
}

get_config31 <- function(pos, meta = NULL) {
  if (!is.null(meta) && is.list(meta) && !is.null(meta$config) && nrow(meta$config)) {
    return(meta$config[1, , drop = FALSE])
  }
  z <- manifest_hist[manifest_hist$position == pos, , drop = FALSE]
  id <- if (nrow(z) && "selected_config_id" %in% names(z)) as.character(z$selected_config_id[1]) else ""
  g <- wk31_param_grid(pos)
  if (nzchar(id) && id %in% g$config_id) return(g[g$config_id == id, , drop = FALSE][1, , drop = FALSE])
  g[1, , drop = FALSE]
}

load_prod_meta31 <- function(pos) {
  p <- paste0("models/weekly_3_1_direct_", pos, "_meta.rds")
  if (file.exists(p)) tryCatch(readRDS(p), error = function(e) NULL) else NULL
}

# -------------------------------------------------------------------------
# Honest current-season OOS policy evaluation.
# Week W is predicted by a challenger fitted only through Week W-1.
# -------------------------------------------------------------------------
oos_old <- wl31_read_csv(oos_path)
if (nrow(oos_old) && !"policy_version" %in% names(oos_old)) oos_old$policy_version <- "legacy"
done_weeks <- integer()
if (nrow(oos_old)) {
  done_weeks <- unique(suppressWarnings(as.integer(
    oos_old$week[oos_old$policy_version == policy_version]
  )))
  done_weeks <- done_weeks[is.finite(done_weeks)]
}

new_oos <- list()
for (eval_week in eligible_weeks[!eligible_weeks %in% done_weeks]) {
  prior_current <- cur_train |> dplyr::filter(week < eval_week)
  train_eval <- dplyr::bind_rows(hist_train, prior_current)
  test_eval <- current |> dplyr::filter(week == eval_week)

  cat("[3.1 LEARNING] OOS Week ", eval_week,
      " | prior current-season rows=", nrow(prior_current), "\n", sep = "")

  for (pos in POSITIONS) {
    tr <- train_eval |> dplyr::filter(position == pos)
    te <- test_eval |> dplyr::filter(position == pos)
    if (!nrow(te)) next

    prod_meta <- load_prod_meta31(pos)
    cfg <- get_config31(pos, prod_meta)
    alpha <- get_alpha31(pos, prod_meta)

    fit <- wk31_fit(
      tr, pos, cfg,
      seed = SEED + 3140L + 100L * match(pos, POSITIONS)
    )
    direct <- wk31_predict(fit, te)
    base <- wl31_num(te$production_base_31, 0)
    avail <- wl31_num(te$availability_factor, 1)
    cand <- pmax(0, base + alpha * (direct - base))
    cand[avail <= 0] <- 0

    new_oos[[length(new_oos) + 1]] <- data.frame(
      evaluated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      policy_version = policy_version,
      season = season,
      week = eval_week,
      trained_through_week = if (nrow(prior_current)) max(prior_current$week) else TRAIN_END,
      player_id = as.character(te$player_id),
      player_display_name = if ("player_display_name" %in% names(te)) as.character(te$player_display_name) else "",
      position = pos,
      team = if ("team" %in% names(te)) as.character(te$team) else "",
      opponent = if ("opponent" %in% names(te)) as.character(te$opponent) else "",
      direct_projection = direct,
      production_base_31 = base,
      blend_alpha_31 = alpha,
      candidate_projection = cand,
      incumbent_projection = wl31_num(te$production_projection, NA_real_),
      actual_fppg_at_evaluation = wl31_num(te$actual_fppg, NA_real_),
      feature_schema_hash = as.character(te$feature_schema_hash),
      stringsAsFactors = FALSE
    )
  }
}

if (length(new_oos)) {
  add <- dplyr::bind_rows(new_oos)
  if (nrow(oos_old)) {
    char_cols <- c("evaluated_at_utc","policy_version","player_id","player_display_name",
                   "position","team","opponent","feature_schema_hash")
    for (nm in intersect(char_cols, names(oos_old))) oos_old[[nm]] <- as.character(oos_old[[nm]])
    for (nm in intersect(char_cols, names(add))) add[[nm]] <- as.character(add[[nm]])
  }
  oos_all <- dplyr::bind_rows(oos_old, add)
  wl31_atomic_write_csv(oos_all, oos_path)
} else {
  oos_all <- oos_old
}

# Rejoin latest official actuals so stat corrections update metrics without
# rewriting the immutable OOS predictions themselves.
oos_eval <- oos_all |>
  dplyr::filter(policy_version == !!policy_version) |>
  dplyr::select(-dplyr::any_of(c("actual_fppg","actual_fppg_latest"))) |>
  dplyr::left_join(
    current |>
      dplyr::select(week, player_id, actual_fppg_latest = actual_fppg) |>
      dplyr::distinct(week, player_id, .keep_all = TRUE),
    by = c("week","player_id")
  ) |>
  dplyr::mutate(actual_fppg = wl31_num(actual_fppg_latest, NA_real_)) |>
  dplyr::filter(is.finite(actual_fppg), is.finite(candidate_projection),
                is.finite(incumbent_projection))

oos_eval <- wl31_add_cohorts(oos_eval)
metrics <- wl31_position_metrics(oos_eval)
if (nrow(metrics)) wl31_atomic_write_csv(metrics, metric_path)

# -------------------------------------------------------------------------
# Final weekly candidates: historical base + every fully captured completed
# current-season week. Hyperparameters remain fixed at the historical winner.
# -------------------------------------------------------------------------
latest_week <- max(eligible_weeks)
final_train <- dplyr::bind_rows(
  hist_train,
  cur_train |> dplyr::filter(week <= latest_week)
)

version_dir <- file.path(
  "models","weekly_3_1_incremental",
  paste0("season_", season),
  sprintf("through_week_%02d", latest_week)
)
dir.create(version_dir, recursive = TRUE, showWarnings = FALSE)

manifest_rows <- list()
for (pos in POSITIONS) {
  tr <- final_train |> dplyr::filter(position == pos)
  prod_meta <- load_prod_meta31(pos)
  cfg <- get_config31(pos, prod_meta)
  alpha <- get_alpha31(pos, prod_meta)

  fit <- wk31_fit(
    tr, pos, cfg,
    seed = SEED + 3140L + 100L * match(pos, POSITIONS)
  )

  cand_model <- file.path(version_dir, paste0("weekly_3_1_direct_", pos, ".json"))
  cand_meta <- file.path(version_dir, paste0("weekly_3_1_direct_", pos, "_meta.rds"))
  xgboost::xgb.save(fit$model, cand_model)

  metric_row <- metrics[metrics$position == pos, , drop = FALSE]
  gate <- wl31_online_gate(metric_row, get_hist_promoted(pos))

  meta_out <- list(
    position = pos,
    features = fit$features,
    config = cfg,
    nrounds = fit$nrounds,
    n_train = fit$n_train,
    final_alpha_2026 = alpha,
    historical_promoted_for_2026 = get_hist_promoted(pos),
    online_learning = TRUE,
    online_policy_version = policy_version,
    trained_through_season = season,
    trained_through_week = latest_week,
    historical_rows = sum(tr$season <= TRAIN_END, na.rm = TRUE),
    current_season_rows = sum(tr$season == season, na.rm = TRUE),
    current_season_weight = current_weight,
    historical_table_md5 = hist_md5_before,
    current_learning_table_md5 = wl31_file_md5(current_path),
    feature_schema_hash = wl31_schema_hash(),
    online_gate_passed = isTRUE(gate$promote),
    online_gate_failed_checks = gate$failed,
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  saveRDS(meta_out, cand_meta)

  prod_model <- paste0("models/weekly_3_1_direct_", pos, ".json")
  prod_meta_path <- paste0("models/weekly_3_1_direct_", pos, "_meta.rds")
  promoted <- isTRUE(gate$promote)

  if (promoted) {
    backup_dir <- file.path(
      "models","weekly_3_1_production_history",
      paste0("season_", season),
      sprintf("before_week_%02d", latest_week)
    )
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

    if (file.exists(prod_model)) {
      b <- file.path(backup_dir, basename(prod_model))
      if (!file.exists(b)) file.copy(prod_model, b, overwrite = FALSE)
    }
    if (file.exists(prod_meta_path)) {
      b <- file.path(backup_dir, basename(prod_meta_path))
      if (!file.exists(b)) file.copy(prod_meta_path, b, overwrite = FALSE)
    }

    if (!file.copy(cand_model, prod_model, overwrite = TRUE)) {
      stop("Could not promote online model for ", pos)
    }
    if (!file.copy(cand_meta, prod_meta_path, overwrite = TRUE)) {
      stop("Could not promote online metadata for ", pos)
    }
  }

  mr <- if (nrow(metric_row)) metric_row[1, , drop = FALSE] else data.frame()
  manifest_rows[[length(manifest_rows) + 1]] <- data.frame(
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    policy_version = policy_version,
    season = season,
    position = pos,
    trained_through_week = latest_week,
    candidate_path = cand_model,
    historical_rows = sum(tr$season <= TRAIN_END, na.rm = TRUE),
    current_season_rows = sum(tr$season == season, na.rm = TRUE),
    current_season_weight = current_weight,
    historical_gate = get_hist_promoted(pos),
    oos_weeks = if (nrow(mr)) mr$oos_weeks else 0,
    oos_n = if (nrow(mr)) mr$n else 0,
    oos_starter_n = if (nrow(mr)) mr$starter_n else 0,
    MAE_gain_pct = if (nrow(mr)) mr$MAE_gain_pct else NA_real_,
    RMSE_gain_pct = if (nrow(mr)) mr$RMSE_gain_pct else NA_real_,
    starter_MAE_gain_pct = if (nrow(mr)) mr$starter_MAE_gain_pct else NA_real_,
    week_wins = if (nrow(mr)) mr$week_wins else 0,
    promoted_to_production = promoted,
    failed_checks = gate$failed,
    stringsAsFactors = FALSE
  )
}

manifest <- dplyr::bind_rows(manifest_rows)
wl31_atomic_write_csv(manifest, manifest_path)

old_history <- wl31_read_csv(history_path)
hist_add <- manifest
if (nrow(old_history)) {
  char_cols <- c("generated_at_utc","policy_version","position","candidate_path","failed_checks")
  for (nm in intersect(char_cols, names(old_history))) old_history[[nm]] <- as.character(old_history[[nm]])
  for (nm in intersect(char_cols, names(hist_add))) hist_add[[nm]] <- as.character(hist_add[[nm]])
}
wl31_atomic_write_csv(dplyr::bind_rows(old_history, hist_add), history_path)

hist_md5_after <- wl31_file_md5(historical_path)
if (!identical(hist_md5_before, hist_md5_after)) {
  stop("Historical training table changed during online learning. Refusing completion.")
}

cat("\n[3.1 LEARNING] Weekly candidates trained through Week ", latest_week, ".\n", sep = "")
cat("[3.1 LEARNING] Historical base preserved: ", hist_md5_after, "\n", sep = "")
print(manifest |>
  dplyr::select(position, trained_through_week, historical_rows,
                current_season_rows, oos_weeks, MAE_gain_pct,
                starter_MAE_gain_pct, promoted_to_production, failed_checks))
