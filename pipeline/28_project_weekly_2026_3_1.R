# ============================================================
# FANTASY MODEL 3.1 - GUARDED LIVE PROJECTION
# ============================================================
# Build the exact validated 2.5 production forecast first. 3.1 can alter only
# unplayed rows and only at positions whose 3.1 challenger passed honest OOF.

source("config.R")
source("pipeline/23_project_weekly_2026_2_5.R")
ensure_packages(c("dplyr", "readr", "xgboost"))
source("R/weekly_engine_31.R")

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

  cat("[3.1 LIVE] Guarded 3.1 projection pass complete.\n")
  print(manifest31 |> dplyr::select(position, promoted_for_2026, final_alpha_2026,
                                    incumbent_starter_MAE, challenger_starter_MAE))
}
