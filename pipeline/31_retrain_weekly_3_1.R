# ============================================================
# FANTASY MODEL 3.1 - GUARDED WEEKLY CURRENT-SEASON RETRAINING
# ============================================================
# 1) Historical validation remains frozen and leak-free.
# 2) Current-season rows are learned only after games are finalized.
# 3) A rolling 2026 shadow backtest trains each week only on earlier weeks.
# 4) Production promotion requires both the historical gate and live evidence.
# 5) Models are retrained on historical + finalized current-season data even
#    when they remain shadow challengers.

source("config.R")
ensure_packages(c("dplyr", "readr", "xgboost"))
source("R/weekly_engine_31.R")

training_path <- "data/processed/weekly_model_table_3_1_training.csv"
archive_path <- paste0("output/pregame_projection_archive_", CURRENT_SEASON, ".csv")
hist_manifest_path <- "output/weekly_3_1_champion_manifest.csv"
hist_promotion_path <- "output/weekly_3_1_promotion.csv"
hist_residual_path <- "output/weekly_3_1_residual_pool.csv"

if (!file.exists(training_path)) stop("Missing weekly learning table: ", training_path)

# Build the historical 3.1 benchmark once if it has never been generated.
# This always reads the immutable historical table and does not fit on 2026.
if (!file.exists(hist_manifest_path) || !file.exists(hist_promotion_path) ||
    !file.exists(hist_residual_path)) {
  cat("[3.1 LEARNING] Historical 3.1 benchmark absent; building it once before live learning.\n")
  source("pipeline/27_train_validate_accuracy_3_1.R", local = FALSE)
}

training <- readr::read_csv(training_path, show_col_types = FALSE, progress = FALSE)
manifest_hist <- readr::read_csv(hist_manifest_path, show_col_types = FALSE, progress = FALSE)
promotion_hist <- readr::read_csv(hist_promotion_path, show_col_types = FALSE, progress = FALSE)
residual_hist <- readr::read_csv(hist_residual_path, show_col_types = FALSE, progress = FALSE)

training$season <- as.integer(training$season)
training$week <- as.integer(training$week)
training$player_id <- as.character(training$player_id)
training$position <- as.character(training$position)

historical <- training |> dplyr::filter(season <= TRAIN_END)
current <- training |> dplyr::filter(season == CURRENT_SEASON)

if (!nrow(current)) stop("No current-season finalized rows are available for weekly retraining.")
if (nrow(historical) == 0) stop("Historical anchor is empty; refusing current-season-only training.")

latest_week <- max(current$week, na.rm = TRUE)
if (!is.finite(latest_week)) stop("Could not determine latest finalized current-season week.")

archive <- if (file.exists(archive_path)) {
  readr::read_csv(archive_path, show_col_types = FALSE, progress = FALSE)
} else data.frame()

if (nrow(archive)) {
  archive$week <- as.integer(archive$week)
  archive$player_id <- as.character(archive$player_id)
  archive$position <- as.character(archive$position)
  archive$captured_at <- as.character(archive$captured_at)
  archive <- archive |>
    dplyr::arrange(week, player_id, position, captured_at) |>
    dplyr::group_by(week, player_id, position) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()
}

safe_metric <- function(actual, pred, fun = c("mae","rmse")) {
  fun <- match.arg(fun)
  a <- suppressWarnings(as.numeric(actual))
  p <- suppressWarnings(as.numeric(pred))
  keep <- is.finite(a) & is.finite(p)
  if (!any(keep)) return(NA_real_)
  if (fun == "mae") mean(abs(a[keep] - p[keep])) else sqrt(mean((a[keep] - p[keep])^2))
}
gain_pct <- function(base, cand) {
  if (!is.finite(base) || base <= 0 || !is.finite(cand)) return(NA_real_)
  100 * (base - cand) / base
}

live_rows <- list()
gate_rows <- list()
model_rows <- list()

for (pos in POSITIONS) {
  mh <- manifest_hist[manifest_hist$position == pos, , drop = FALSE]
  ph <- promotion_hist[promotion_hist$position == pos, , drop = FALSE]
  hist_promoted <- nrow(ph) > 0 && isTRUE(wk25_bool(ph$promoted_for_2026[1]))

  config_id <- if (nrow(mh) && "selected_config_id" %in% names(mh)) {
    as.character(mh$selected_config_id[1])
  } else ""
  grid <- wk31_param_grid(pos)
  selected <- grid[grid$config_id == config_id, , drop = FALSE]
  if (!nrow(selected)) selected <- grid[1, , drop = FALSE]

  alpha <- if (nrow(mh) && "final_alpha_2026" %in% names(mh)) {
    suppressWarnings(as.numeric(mh$final_alpha_2026[1]))
  } else 0
  if (!is.finite(alpha)) alpha <- 0
  alpha <- pmin(as.numeric(MODEL31_FINAL_ALPHA_CAP[[pos]]), pmax(0, alpha))

  pos_current <- current |> dplyr::filter(position == pos)
  pos_hist <- historical |> dplyr::filter(position == pos)
  weeks <- sort(unique(pos_current$week[is.finite(pos_current$week)]))

  pos_oof <- list()
  if (nrow(archive) && length(weeks)) {
    for (ww in weeks) {
      train_w <- dplyr::bind_rows(
        pos_hist,
        pos_current |> dplyr::filter(week < ww)
      )
      test_w <- pos_current |> dplyr::filter(week == ww)
      if (nrow(train_w) < 100 || !nrow(test_w)) next

      fit_w <- tryCatch(
        wk31_fit(
          train_w, pos, selected,
          seed = SEED + 3160L + 100L * match(pos, POSITIONS) + ww
        ),
        error = function(e) {
          warning("[3.1 LEARNING] Live OOF fit failed for ", pos, " Week ", ww, ": ", conditionMessage(e))
          NULL
        }
      )
      if (is.null(fit_w)) next

      direct <- wk31_predict(fit_w, test_w)
      pred <- data.frame(
        week = as.integer(test_w$week),
        player_id = as.character(test_w$player_id),
        player_display_name = as.character(test_w$player_display_name),
        position = pos,
        actual_fppg = suppressWarnings(as.numeric(test_w$weekly_fppg)),
        direct_fppg_31 = direct,
        stringsAsFactors = FALSE
      )

      inc <- archive |>
        dplyr::filter(position == pos, week == ww) |>
        dplyr::transmute(
          week,
          player_id,
          incumbent_projection = suppressWarnings(as.numeric(projection))
        )

      z <- pred |>
        dplyr::inner_join(inc, by = c("week", "player_id")) |>
        dplyr::filter(
          is.finite(actual_fppg),
          is.finite(direct_fppg_31),
          is.finite(incumbent_projection)
        )
      if (!nrow(z)) next

      z$candidate_fppg_31 <- pmax(
        0,
        z$incumbent_projection + alpha * (z$direct_fppg_31 - z$incumbent_projection)
      )
      z$production_base_31 <- z$incumbent_projection
      z <- z |>
        dplyr::group_by(week, position) |>
        dplyr::mutate(
          incumbent_rank = rank(production_base_31, ties.method = "min") * -1
        ) |>
        dplyr::ungroup()

      # Recompute rank correctly in descending order.
      z <- z |>
        dplyr::group_by(week, position) |>
        dplyr::mutate(
          incumbent_rank = rank(-production_base_31, ties.method = "min"),
          starter_cohort = incumbent_rank <= as.numeric(REPLACEMENT_RANK[[pos]]),
          relevant_cohort = incumbent_rank <= 2 * as.numeric(REPLACEMENT_RANK[[pos]])
        ) |>
        dplyr::ungroup()
      pos_oof[[length(pos_oof) + 1]] <- z
    }
  }

  live <- dplyr::bind_rows(pos_oof)
  if (nrow(live)) {
    live$abs_error_incumbent <- abs(live$actual_fppg - live$production_base_31)
    live$abs_error_candidate <- abs(live$actual_fppg - live$candidate_fppg_31)
    live_rows[[length(live_rows) + 1]] <- live
  }

  all_base_mae <- if (nrow(live)) safe_metric(live$actual_fppg, live$production_base_31, "mae") else NA_real_
  all_cand_mae <- if (nrow(live)) safe_metric(live$actual_fppg, live$candidate_fppg_31, "mae") else NA_real_
  all_base_rmse <- if (nrow(live)) safe_metric(live$actual_fppg, live$production_base_31, "rmse") else NA_real_
  all_cand_rmse <- if (nrow(live)) safe_metric(live$actual_fppg, live$candidate_fppg_31, "rmse") else NA_real_

  starter <- if (nrow(live)) live[live$starter_cohort %in% TRUE, , drop = FALSE] else data.frame()
  starter_base_mae <- if (nrow(starter)) safe_metric(starter$actual_fppg, starter$production_base_31, "mae") else NA_real_
  starter_cand_mae <- if (nrow(starter)) safe_metric(starter$actual_fppg, starter$candidate_fppg_31, "mae") else NA_real_

  all_gain <- gain_pct(all_base_mae, all_cand_mae)
  starter_gain <- gain_pct(starter_base_mae, starter_cand_mae)
  rmse_gain <- gain_pct(all_base_rmse, all_cand_rmse)

  boot <- if (nrow(live)) {
    wk31_bootstrap_gain(
      live, cohort = "Starter", reps = 500L,
      seed = SEED + 3170L + match(pos, POSITIONS)
    )
  } else data.frame(p_improves = NA_real_)
  p_improves <- if (nrow(boot)) suppressWarnings(as.numeric(boot$p_improves[1])) else NA_real_

  n_weeks <- if (nrow(live)) length(unique(live$week)) else 0L
  n_live <- nrow(live)
  n_starter <- nrow(starter)

  checks <- c(
    historical_gate = hist_promoted,
    enough_live_weeks = n_weeks >= 2,
    enough_live_rows = n_live >= 40,
    enough_starter_rows = n_starter >= 10,
    live_all_MAE = is.finite(all_gain) && all_gain >= 0,
    live_starter_MAE = is.finite(starter_gain) && starter_gain >= 0,
    live_RMSE = is.finite(rmse_gain) && rmse_gain >= -0.5,
    paired_confidence = is.finite(p_improves) && p_improves >= 0.50
  )
  promote_live <- all(checks)

  # Retrain the shadow/final candidate on every finalized row available so far.
  full_train <- dplyr::bind_rows(pos_hist, pos_current)
  final_fit <- wk31_fit(
    full_train, pos, selected,
    seed = SEED + 3180L + 100L * match(pos, POSITIONS)
  )
  model_path <- paste0("models/weekly_3_1_direct_", pos, ".json")
  meta_path <- paste0("models/weekly_3_1_direct_", pos, "_meta.rds")
  xgboost::xgb.save(final_fit$model, model_path)
  saveRDS(list(
    position = pos,
    features = final_fit$features,
    config = selected,
    nrounds = final_fit$nrounds,
    n_train = final_fit$n_train,
    final_alpha_2026 = alpha,
    promoted_for_2026 = promote_live,
    historical_promoted_for_2026 = hist_promoted,
    trained_through_season = CURRENT_SEASON,
    trained_through_week = latest_week,
    historical_rows = nrow(pos_hist),
    current_season_rows = nrow(pos_current),
    current_season_weight = suppressWarnings(as.numeric(Sys.getenv("FM_31_CURRENT_SEASON_WEIGHT", unset = "1.5")))
  ), meta_path)

  gate_rows[[length(gate_rows) + 1]] <- data.frame(
    position = pos,
    promoted_for_2026 = promote_live,
    historical_promoted_for_2026 = hist_promoted,
    selected_config_id = selected$config_id[1],
    final_alpha_2026 = alpha,
    trained_through_week = latest_week,
    live_weeks = n_weeks,
    live_n = n_live,
    live_starter_n = n_starter,
    live_incumbent_MAE = all_base_mae,
    live_candidate_MAE = all_cand_mae,
    live_MAE_gain_pct = all_gain,
    live_incumbent_RMSE = all_base_rmse,
    live_candidate_RMSE = all_cand_rmse,
    live_RMSE_gain_pct = rmse_gain,
    live_incumbent_starter_MAE = starter_base_mae,
    live_candidate_starter_MAE = starter_cand_mae,
    live_starter_MAE_gain_pct = starter_gain,
    live_starter_bootstrap_p_improves = p_improves,
    historical_rows = nrow(pos_hist),
    current_season_rows = nrow(pos_current),
    failed_checks = paste(names(checks)[!checks], collapse = ";"),
    stringsAsFactors = FALSE
  )

  model_rows[[length(model_rows) + 1]] <- data.frame(
    position = pos,
    n_train = final_fit$n_train,
    trained_through_week = latest_week,
    promoted_for_2026 = promote_live,
    stringsAsFactors = FALSE
  )

  cat(
    "[3.1 LEARNING] ", pos,
    " | hist_gate=", hist_promoted,
    " | live_n=", n_live,
    " | live_gain=", ifelse(is.finite(all_gain), round(all_gain, 2), NA),
    "% | starter_gain=", ifelse(is.finite(starter_gain), round(starter_gain, 2), NA),
    "% | ", ifelse(promote_live, "PROMOTE", "SHADOW"),
    "\n", sep = ""
  )
}

live_validation <- dplyr::bind_rows(live_rows)
live_gate <- dplyr::bind_rows(gate_rows)
model_audit <- dplyr::bind_rows(model_rows)

readr::write_csv(
  live_validation,
  paste0("output/weekly_3_1_live_learning_validation_", CURRENT_SEASON, ".csv")
)
readr::write_csv(
  live_gate,
  paste0("output/weekly_3_1_live_learning_gate_", CURRENT_SEASON, ".csv")
)
readr::write_csv(live_gate, "output/weekly_3_1_promotion.csv")

# Preserve historical manifest metrics while surfacing the live weekly gate.
manifest_live <- manifest_hist |>
  dplyr::select(-dplyr::any_of(c(
    "promoted_for_2026", "live_weeks", "live_n", "live_starter_n",
    "live_MAE_gain_pct", "live_starter_MAE_gain_pct",
    "live_starter_bootstrap_p_improves", "trained_through_week",
    "historical_promoted_for_2026", "live_failed_checks"
  ))) |>
  dplyr::left_join(
    live_gate |>
      dplyr::transmute(
        position,
        promoted_for_2026,
        historical_promoted_for_2026,
        trained_through_week,
        live_weeks,
        live_n,
        live_starter_n,
        live_MAE_gain_pct,
        live_starter_MAE_gain_pct,
        live_starter_bootstrap_p_improves,
        live_failed_checks = failed_checks
      ),
    by = "position"
  )
readr::write_csv(manifest_live, hist_manifest_path)

# Historical residuals remain; current-season honest rolling residuals are
# appended to the derived residual pool for interval estimation.
if (nrow(live_validation)) {
  live_residual <- live_validation |>
    dplyr::transmute(
      season = CURRENT_SEASON,
      week,
      player_id,
      player_display_name,
      position,
      actual_fppg,
      production_base_31,
      direct_fppg_31,
      candidate_fppg_31,
      residual = actual_fppg - candidate_fppg_31,
      abs_error = abs(residual),
      starter_cohort,
      relevant_cohort
    )
  residual_combined <- dplyr::bind_rows(
    residual_hist |> dplyr::filter(as.integer(season) <= TRAIN_END),
    live_residual
  ) |>
    dplyr::arrange(season, week, position, player_id)
  readr::write_csv(residual_combined, hist_residual_path)
}

run_path <- paste0("output/weekly_3_1_learning_runs_", CURRENT_SEASON, ".csv")
run_row <- data.frame(
  trained_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  trained_through_week = latest_week,
  historical_rows = nrow(historical),
  current_season_rows = nrow(current),
  promoted_positions = paste(live_gate$position[live_gate$promoted_for_2026 %in% TRUE], collapse = ","),
  shadow_positions = paste(live_gate$position[!live_gate$promoted_for_2026 %in% TRUE], collapse = ","),
  stringsAsFactors = FALSE
)
old_runs <- if (file.exists(run_path)) {
  tryCatch(readr::read_csv(run_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
} else data.frame()
readr::write_csv(dplyr::bind_rows(old_runs, run_row), run_path)

cat("\n[3.1 LEARNING] Weekly retraining complete through Week ", latest_week, ".\n", sep = "")
print(live_gate)
