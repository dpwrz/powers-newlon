# ============================================================
# FANTASY MODEL 3.1 - GUARDED LIVE PROJECTION
# ============================================================
# Build the exact validated 2.5 production forecast first. 3.1 can alter only
# unplayed rows and only at positions whose 3.1 challenger passed honest OOF.

source("config.R")
source("pipeline/23_project_weekly_2026_2_5.R")
ensure_packages(c("dplyr", "readr", "xgboost"))
source("R/weekly_engine_31.R")
if (file.exists("R/weekly_engine_32.R")) source("R/weekly_engine_32.R")

req31 <- c(
  "output/weekly_3_1_promotion.csv",
  "output/weekly_3_1_champion_manifest.csv",
  "output/weekly_3_1_residual_pool.csv"
)
if (any(!file.exists(req31))) {
  cat("[3.1 LIVE] Validation artifacts not present. Keeping exact 2.5 production forecast.\n")
} else {
  promotion31 <- readr::read_csv(req31[1], show_col_types = FALSE, progress = FALSE)
  manifest31 <- readr::read_csv(req31[2], show_col_types = FALSE, progress = FALSE)
  res31 <- readr::read_csv(req31[3], show_col_types = FALSE, progress = FALSE)

  rows31 <- list()
  for (pos in POSITIONS) {
    d <- future_pred |> dplyr::filter(position == pos)
    if (!nrow(d)) next

    pr <- promotion31[promotion31$position == pos, , drop = FALSE]
    mf <- manifest31[manifest31$position == pos, , drop = FALSE]
    promote <- nrow(pr) && isTRUE(wk25_bool(pr$promoted_for_2026[1]))
    alpha <- if (nrow(mf)) as.numeric(mf$final_alpha_2026[1]) else 0
    if (!is.finite(alpha)) alpha <- 0

    d$production_base_31 <- wk31_num(d$projected_weekly_fppg)
    d$direct_fppg_31 <- NA_real_
    d$blend_alpha_31 <- alpha
    d$model31_promoted <- promote
    d$direct_gap_31 <- NA_real_
    for (nm in MODEL31_DERIVED_FEATURES) {
      if (!nm %in% names(d)) d[[nm]] <- NA_real_
    }

    unplayed <- wk31_num(d$is_actual, 0) == 0
    model_path <- paste0("models/weekly_3_1_direct_", pos, ".json")
    meta_path <- paste0("models/weekly_3_1_direct_", pos, "_meta.rds")

    if (promote && any(unplayed) && file.exists(model_path) && file.exists(meta_path)) {
      meta <- readRDS(meta_path)
      obj <- list(model = xgboost::xgb.load(model_path), features = meta$features)
      du <- wk31_add_features(d[unplayed, , drop = FALSE])
      direct <- wk31_predict(obj, du)
      cand <- pmax(0, wk31_num(du$projected_weekly_fppg) +
                     alpha * (direct - wk31_num(du$projected_weekly_fppg)))
      avail <- if ("availability_factor" %in% names(du)) wk31_num(du$availability_factor, 1) else rep(1, nrow(du))
      cand[avail <= 0] <- 0

      d$direct_fppg_31[unplayed] <- direct
      d$direct_gap_31[unplayed] <- direct - wk31_num(du$projected_weekly_fppg)
      d$projected_weekly_fppg[unplayed] <- cand
      if ("projected_weekly_fppg_pre_availability" %in% names(d)) {
        aa <- pmax(avail, 1e-6)
        pre <- cand / aa
        pre[avail <= 0] <- 0
        d$projected_weekly_fppg_pre_availability[unplayed] <- pre
      }

      du2 <- wk31_add_features(d[unplayed, , drop = FALSE])
      for (nm in MODEL31_DERIVED_FEATURES) d[[nm]][unplayed] <- du2[[nm]]

      rr <- wk31_num(res31$residual[res31$position == pos], NA_real_)
      rr <- rr[is.finite(rr)]
      if (length(rr) >= 30 && exists("empirical_distribution22", mode = "function")) {
        dist <- empirical_distribution22(d$projected_weekly_fppg[unplayed], rr, pos)
        for (nm in names(dist)) {
          if (!nm %in% names(d)) d[[nm]] <- NA_real_
          d[[nm]][unplayed] <- dist[[nm]]
        }
      }
      cat("[3.1 LIVE] ", pos, " challenger applied to ", sum(unplayed),
          " unplayed rows; alpha=", alpha, ".\n", sep = "")
    }
    rows31[[length(rows31) + 1]] <- d
  }

  future_pred <- dplyr::bind_rows(rows31)

  rerank31 <- function(d) {
    if (!"weekly_position_rank" %in% names(d)) d$weekly_position_rank <- NA_real_
    idx <- wk31_num(d$is_actual, 0) == 0
    if (any(idx)) d$weekly_position_rank[idx] <- rank(-wk31_num(d$projected_weekly_fppg[idx]), ties.method = "min", na.last = "keep")
    d
  }
  future_pred <- dplyr::bind_rows(
    lapply(split(future_pred, interaction(future_pred$week, future_pred$position, drop = TRUE)), rerank31)
  ) |>
    dplyr::arrange(week, dplyr::desc(projected_weekly_fppg))

  future_pred$season_ledger_points <- ifelse(
    wk31_num(future_pred$is_actual, 0) == 1,
    wk31_num(future_pred$actual_weekly_fppg),
    wk31_num(future_pred$projected_weekly_fppg)
  )
  readr::write_csv(future_pred, paste0("output/weekly_", CURRENT_SEASON, "_projections.csv"))

  week_now <- future_pred |>
    dplyr::filter(week == current_week, is_actual == 0) |>
    dplyr::arrange(dplyr::desc(projected_weekly_fppg))
  readr::write_csv(week_now, paste0("output/week_", current_week, "_rankings.csv"))

  ros <- future_pred |>
    dplyr::group_by(player_id, player_display_name, position, team) |>
    dplyr::summarise(
      actual_points_to_date = sum(dplyr::if_else(is_actual == 1, actual_weekly_fppg, 0), na.rm = TRUE),
      projected_remaining_points = sum(dplyr::if_else(is_actual == 0, projected_weekly_fppg, 0), na.rm = TRUE),
      projected_full_season_points = sum(season_ledger_points, na.rm = TRUE),
      remaining_games = sum(is_actual == 0), .groups = "drop"
    ) |>
    dplyr::group_by(position) |>
    dplyr::arrange(dplyr::desc(projected_full_season_points), .by_group = TRUE) |>
    dplyr::mutate(ros_position_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(projected_full_season_points))
  readr::write_csv(ros, paste0("output/rest_of_season_", CURRENT_SEASON, ".csv"))

  if (nrow(week_now)) {
    hist_path <- paste0("output/projection_history_", CURRENT_SEASON, ".csv")
    snap <- week_now |>
      dplyr::transmute(
        generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), model_version = "3.1",
        week, player_id, player_display_name, position, team, opponent,
        projected_weekly_fppg, production_base_31, direct_fppg_31,
        blend_alpha_31, direct_gap_31, model31_promoted,
        adaptive_prior_weight_31, adaptive_prior_fppg_31, role_change_score_31,
        weekly_floor, weekly_ceiling, projection_confidence, expected_abs_error
      )
    old <- if (file.exists(hist_path)) tryCatch(readr::read_csv(hist_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
    char_cols <- c("generated_at","model_version","player_id","player_display_name","position","team","opponent","projection_confidence")
    for (nm in intersect(char_cols, names(old))) old[[nm]] <- as.character(old[[nm]])
    for (nm in intersect(char_cols, names(snap))) snap[[nm]] <- as.character(snap[[nm]])
    readr::write_csv(dplyr::bind_rows(old, snap), hist_path)
  }

  # Preserve full safe feature snapshots for the current NFL week. This is an
  # append-only evidence stream; weekly learning later selects the last snapshot
  # that is provably before kickoff. Older snapshots are never deleted.
  feature_rows31 <- future_pred |>
    dplyr::filter(week == current_week, wk31_num(is_actual, 0) == 0)
  if (nrow(feature_rows31)) {
    feature_rows31 <- wk31_add_features(feature_rows31)
    id_cols31 <- intersect(c(
      "season","week","player_id","player_display_name","position","team","opponent",
      "gameday","gametime"
    ), names(feature_rows31))
    safe_cols31 <- intersect(MODEL31_SAFE_FEATURES, names(feature_rows31))
    feature_snap31 <- feature_rows31[, unique(c(id_cols31, safe_cols31)), drop = FALSE]
    feature_snap31$pregame_captured_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    feature_snap31$feature_model_version <- "3.1"
    feature_snap31$production_projection <- wk31_num(feature_rows31$projected_weekly_fppg)
    feature_snap31 <- feature_snap31[, c(
      "pregame_captured_at","feature_model_version",
      setdiff(names(feature_snap31), c("pregame_captured_at","feature_model_version"))
    ), drop = FALSE]

    feature_history_path31 <- paste0("output/pregame_feature_history_", CURRENT_SEASON, ".csv")
    old_feature31 <- if (file.exists(feature_history_path31)) {
      tryCatch(readr::read_csv(feature_history_path31, show_col_types = FALSE, progress = FALSE),
               error = function(e) data.frame())
    } else data.frame()
    char31 <- c("pregame_captured_at","feature_model_version","player_id","player_display_name",
                "position","team","opponent","gameday","gametime")
    for (nm in intersect(char31, names(old_feature31))) old_feature31[[nm]] <- as.character(old_feature31[[nm]])
    for (nm in intersect(char31, names(feature_snap31))) feature_snap31[[nm]] <- as.character(feature_snap31[[nm]])
    readr::write_csv(dplyr::bind_rows(old_feature31, feature_snap31), feature_history_path31)
    cat("[3.1 WEEKLY LIVE] Archived ", nrow(feature_snap31),
        " pregame feature snapshots for Week ", current_week, ".\n", sep = "")
  }

  cat("[3.1 LIVE] Guarded 3.1 projection pass complete.\n")
  print(manifest31 |> dplyr::select(position, promoted_for_2026, final_alpha_2026,
                                    incumbent_starter_MAE, challenger_starter_MAE))
}


# -------------------------------------------------------------------------
# Guarded current-season weekly learner.
#
# This layer is independent of the optional historical 3.1 challenger artifacts.
# It always starts from the exact production forecast that exists after the
# upstream pipeline (2.5 today, historical 3.1 later if those artifacts exist).
# -------------------------------------------------------------------------
live_manifest_path31 <- "output/weekly_3_1_live_manifest.csv"
live_manifest31 <- if (file.exists(live_manifest_path31)) {
  tryCatch(readr::read_csv(live_manifest_path31, show_col_types = FALSE, progress = FALSE),
           error = function(e) data.frame())
} else data.frame()
live_resid_path31 <- "output/weekly_3_1_live_residual_pool.csv"
live_resid31 <- if (file.exists(live_resid_path31)) {
  tryCatch(readr::read_csv(live_resid_path31, show_col_types = FALSE, progress = FALSE),
           error = function(e) data.frame())
} else data.frame()

live_rows31 <- list()
for (pos in POSITIONS) {
  d <- future_pred |> dplyr::filter(position == pos)
  if (!nrow(d)) next

  if (!"weekly_live_direct_fppg_31" %in% names(d)) d$weekly_live_direct_fppg_31 <- NA_real_
  if (!"weekly_live_alpha_31" %in% names(d)) d$weekly_live_alpha_31 <- 0
  if (!"weekly_live_model_used_31" %in% names(d)) d$weekly_live_model_used_31 <- FALSE

  lm <- if (nrow(live_manifest31) && "position" %in% names(live_manifest31)) {
    live_manifest31[as.character(live_manifest31$position) == pos, , drop = FALSE]
  } else data.frame()
  use_live <- nrow(lm) && "use_live_model" %in% names(lm) && isTRUE(wk25_bool(lm$use_live_model[1]))
  alpha_live <- if (nrow(lm) && "deployed_alpha" %in% names(lm)) {
    suppressWarnings(as.numeric(lm$deployed_alpha[1]))
  } else 0
  if (!is.finite(alpha_live)) alpha_live <- 0

  live_model_path <- paste0("models/weekly_3_1_live_", pos, ".json")
  live_meta_path <- paste0("models/weekly_3_1_live_", pos, "_meta.rds")
  unplayed <- wk31_num(d$is_actual, 0) == 0

  if (use_live && alpha_live > 0 && any(unplayed) &&
      file.exists(live_model_path) && file.exists(live_meta_path)) {
    meta_live <- readRDS(live_meta_path)
    obj_live <- list(model = xgboost::xgb.load(live_model_path), features = meta_live$features)
    du <- wk31_add_features(d[unplayed, , drop = FALSE])
    direct_final <- wk31_predict(obj_live, du)
    avail <- if ("availability_factor" %in% names(du)) wk31_num(du$availability_factor, 1) else rep(1, nrow(du))
    base_final <- wk31_num(du$projected_weekly_fppg)
    candidate_final <- pmax(0, base_final + alpha_live * (direct_final - base_final))
    candidate_final[avail <= 0] <- 0

    d$weekly_live_direct_fppg_31[unplayed] <- direct_final
    d$weekly_live_alpha_31[unplayed] <- alpha_live
    d$weekly_live_model_used_31[unplayed] <- TRUE
    d$projected_weekly_fppg[unplayed] <- candidate_final

    if ("projected_weekly_fppg_pre_availability" %in% names(d)) {
      pre <- candidate_final / pmax(avail, 1e-6)
      pre[avail <= 0] <- 0
      d$projected_weekly_fppg_pre_availability[unplayed] <- pre
    }

    rr <- if (nrow(live_resid31) && all(c("position","residual") %in% names(live_resid31))) {
      wk31_num(live_resid31$residual[as.character(live_resid31$position) == pos], NA_real_)
    } else numeric()
    rr <- rr[is.finite(rr)]
    if (length(rr) >= 30 && exists("empirical_distribution22", mode = "function")) {
      dist <- empirical_distribution22(d$projected_weekly_fppg[unplayed], rr, pos)
      for (nm in names(dist)) {
        if (!nm %in% names(d)) d[[nm]] <- NA_real_
        d[[nm]][unplayed] <- dist[[nm]]
      }
    }

    cat("[3.1 WEEKLY LIVE] ", pos, " deployed weekly learner applied to ",
        sum(unplayed), " rows; alpha=", alpha_live, ".\n", sep = "")
  }

  live_rows31[[length(live_rows31) + 1L]] <- d
}
future_pred <- dplyr::bind_rows(live_rows31)

# -------------------------------------------------------------------------
# Model 3.2 signal/noise optimizer.
# Applies only to positions that passed BOTH historical architecture/factor
# validation and independent live OOF gates. 3.1 remains the fallback.
# -------------------------------------------------------------------------
promotion32_path <- "output/weekly_3_2_promotion.csv"
residual32_path <- "output/weekly_3_2_residual_pool.csv"
promotion32 <- if (file.exists(promotion32_path)) {
  tryCatch(readr::read_csv(promotion32_path, show_col_types = FALSE, progress = FALSE),
           error = function(e) data.frame())
} else data.frame()
residual32 <- if (file.exists(residual32_path)) {
  tryCatch(readr::read_csv(residual32_path, show_col_types = FALSE, progress = FALSE),
           error = function(e) data.frame())
} else data.frame()

if (exists("wk32_add_factors", mode = "function") && nrow(promotion32)) {
  rows32 <- list()
  for (pos in POSITIONS) {
    d <- future_pred |> dplyr::filter(position == pos)
    if (!nrow(d)) next

    d$weekly_live_direct_fppg_32 <- NA_real_
    d$weekly_live_alpha_32 <- 0
    d$weekly_live_model_used_32 <- FALSE
    d$weekly_live_variant_32 <- ""

    pr32 <- promotion32[promotion32$position == pos, , drop = FALSE]
    promote32 <- nrow(pr32) &&
      "promoted_32" %in% names(pr32) &&
      isTRUE(wk25_bool(pr32$promoted_32[1]))

    model32_path <- paste0("models/weekly_3_2_live_", pos, ".json")
    meta32_path <- paste0("models/weekly_3_2_live_", pos, "_meta.rds")
    unplayed <- wk31_num(d$is_actual, 0) == 0

    if (promote32 && any(unplayed) &&
        file.exists(model32_path) && file.exists(meta32_path)) {
      meta32 <- readRDS(meta32_path)
      alpha32 <- suppressWarnings(as.numeric(meta32$blend_alpha))
      if (!is.finite(alpha32)) alpha32 <- as.numeric(MODEL31_WEEKLY_ALPHA[[pos]])
      obj32 <- list(
        model = xgboost::xgb.load(model32_path),
        features = meta32$features
      )

      du32 <- wk32_add_factors(d[unplayed, , drop = FALSE])
      direct32 <- wk31_predict(obj32, du32)
      base32 <- wk31_num(du32$projected_weekly_fppg)
      avail32 <- if ("availability_factor" %in% names(du32)) {
        wk31_num(du32$availability_factor, 1)
      } else rep(1, nrow(du32))
      candidate32 <- pmax(0, base32 + alpha32 * (direct32 - base32))
      candidate32[avail32 <= 0] <- 0

      d$weekly_live_direct_fppg_32[unplayed] <- direct32
      d$weekly_live_alpha_32[unplayed] <- alpha32
      d$weekly_live_model_used_32[unplayed] <- TRUE
      d$weekly_live_variant_32[unplayed] <- as.character(meta32$selected_variant)
      d$projected_weekly_fppg[unplayed] <- candidate32

      if ("projected_weekly_fppg_pre_availability" %in% names(d)) {
        pre32 <- candidate32 / pmax(avail32, 1e-6)
        pre32[avail32 <= 0] <- 0
        d$projected_weekly_fppg_pre_availability[unplayed] <- pre32
      }

      rr32 <- if (nrow(residual32) && all(c("position","residual") %in% names(residual32))) {
        wk31_num(residual32$residual[as.character(residual32$position) == pos], NA_real_)
      } else numeric()
      rr32 <- rr32[is.finite(rr32)]
      if (length(rr32) >= 30 && exists("empirical_distribution22", mode = "function")) {
        dist32 <- empirical_distribution22(d$projected_weekly_fppg[unplayed], rr32, pos)
        for (nm in names(dist32)) {
          if (!nm %in% names(d)) d[[nm]] <- NA_real_
          d[[nm]][unplayed] <- dist32[[nm]]
        }
      }

      cat("[3.2 LIVE] ", pos, " signal optimizer applied to ",
          sum(unplayed), " unplayed rows; alpha=", alpha32,
          " | variant=", as.character(meta32$selected_variant), ".\n", sep = "")
    }

    rows32[[length(rows32) + 1L]] <- d
  }
  future_pred <- dplyr::bind_rows(rows32)
}

rerank_live31 <- function(d) {
  if (!"weekly_position_rank" %in% names(d)) d$weekly_position_rank <- NA_real_
  idx <- wk31_num(d$is_actual, 0) == 0
  if (any(idx)) {
    d$weekly_position_rank[idx] <- rank(
      -wk31_num(d$projected_weekly_fppg[idx]),
      ties.method = "min", na.last = "keep"
    )
  }
  d
}
future_pred <- dplyr::bind_rows(
  lapply(split(future_pred, interaction(future_pred$week, future_pred$position, drop = TRUE)), rerank_live31)
) |>
  dplyr::arrange(week, dplyr::desc(projected_weekly_fppg))

future_pred$season_ledger_points <- ifelse(
  wk31_num(future_pred$is_actual, 0) == 1,
  wk31_num(future_pred$actual_weekly_fppg),
  wk31_num(future_pred$projected_weekly_fppg)
)
readr::write_csv(future_pred, paste0("output/weekly_", CURRENT_SEASON, "_projections.csv"))

week_now <- future_pred |>
  dplyr::filter(week == current_week, is_actual == 0) |>
  dplyr::arrange(dplyr::desc(projected_weekly_fppg))
readr::write_csv(week_now, paste0("output/week_", current_week, "_rankings.csv"))

ros_live31 <- future_pred |>
  dplyr::group_by(player_id, player_display_name, position, team) |>
  dplyr::summarise(
    actual_points_to_date = sum(dplyr::if_else(is_actual == 1, actual_weekly_fppg, 0), na.rm = TRUE),
    projected_remaining_points = sum(dplyr::if_else(is_actual == 0, projected_weekly_fppg, 0), na.rm = TRUE),
    projected_full_season_points = sum(season_ledger_points, na.rm = TRUE),
    remaining_games = sum(is_actual == 0),
    .groups = "drop"
  ) |>
  dplyr::group_by(position) |>
  dplyr::arrange(dplyr::desc(projected_full_season_points), .by_group = TRUE) |>
  dplyr::mutate(ros_position_rank = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(projected_full_season_points))
readr::write_csv(ros_live31, paste0("output/rest_of_season_", CURRENT_SEASON, ".csv"))

# Full safe pregame feature evidence for future weekly training.
feature_rows31 <- future_pred |>
  dplyr::filter(week == current_week, wk31_num(is_actual, 0) == 0)
if (nrow(feature_rows31)) {
  feature_rows31 <- wk31_add_features(feature_rows31)
  id_cols31 <- intersect(c(
    "season","week","player_id","player_display_name","position","team","opponent",
    "gameday","gametime"
  ), names(feature_rows31))
  safe_cols31 <- intersect(MODEL31_SAFE_FEATURES, names(feature_rows31))
  feature_snap31 <- feature_rows31[, unique(c(id_cols31, safe_cols31)), drop = FALSE]
  feature_snap31$pregame_captured_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  feature_snap31$feature_model_version <- "3.1-weekly-capable"
  feature_snap31$production_projection <- wk31_num(feature_rows31$projected_weekly_fppg)
  feature_snap31 <- feature_snap31[, c(
    "pregame_captured_at","feature_model_version",
    setdiff(names(feature_snap31), c("pregame_captured_at","feature_model_version"))
  ), drop = FALSE]

  feature_history_path31 <- paste0("output/pregame_feature_history_", CURRENT_SEASON, ".csv")
  old_feature31 <- if (file.exists(feature_history_path31)) {
    tryCatch(readr::read_csv(feature_history_path31, show_col_types = FALSE, progress = FALSE),
             error = function(e) data.frame())
  } else data.frame()
  char31 <- c(
    "pregame_captured_at","feature_model_version","player_id","player_display_name",
    "position","team","opponent","gameday","gametime"
  )
  for (nm in intersect(char31, names(old_feature31))) old_feature31[[nm]] <- as.character(old_feature31[[nm]])
  for (nm in intersect(char31, names(feature_snap31))) feature_snap31[[nm]] <- as.character(feature_snap31[[nm]])

  readr::write_csv(dplyr::bind_rows(old_feature31, feature_snap31), feature_history_path31)
  cat("[3.1 WEEKLY LIVE] Archived ", nrow(feature_snap31),
      " safe pregame feature snapshots for Week ", current_week, ".\n", sep = "")
}

cat("[3.1 WEEKLY LIVE] Final weekly-learning-capable projection pass complete.\n")
