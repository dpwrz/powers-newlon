# ============================================================
# FANTASY MODEL 3.2 - SIGNAL OPTIMIZATION TOURNAMENT
# ============================================================
# Historical 2024-2025 decides the feature architecture. 2026 is then used only
# as an independent rolling OOF promotion gate against the projections users
# actually saw. Production is not touched by this script.

source("config.R")
ensure_packages(c("dplyr","readr","tidyr","xgboost","jsonlite"))
source("R/weekly_engine_31.R")
source("R/weekly_training_history_31.R")
source("R/weekly_engine_32.R")

dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create("models", recursive = TRUE, showWarnings = FALSE)

historical_path <- "data/processed/weekly_model_table_2_3.csv"
live_path <- file.path("data","processed",paste0("weekly_training_live_", CURRENT_SEASON, ".csv"))
archive_path <- file.path("output",paste0("pregame_projection_archive_", CURRENT_SEASON, ".csv"))
config_path <- "output/weekly_3_1_live_config.csv"

for (p in c(historical_path, live_path, archive_path, config_path)) {
  if (!file.exists(p)) stop("Missing Model 3.2 prerequisite: ", p)
}

historical <- readr::read_csv(historical_path, show_col_types = FALSE, progress = FALSE)
live <- readr::read_csv(live_path, show_col_types = FALSE, progress = FALSE)
archive <- readr::read_csv(archive_path, show_col_types = FALSE, progress = FALSE)
config31 <- readr::read_csv(config_path, show_col_types = FALSE, progress = FALSE)

normalize32 <- function(d) {
  if (!nrow(d)) return(d)
  d$season <- suppressWarnings(as.integer(d$season))
  d$week <- suppressWarnings(as.integer(d$week))
  d$position <- toupper(as.character(d$position))
  d$player_id <- as.character(d$player_id)
  d
}
historical <- normalize32(historical) |> dplyr::filter(season <= TRAIN_END, position %in% POSITIONS)
live <- normalize32(live) |> dplyr::filter(season == CURRENT_SEASON, position %in% POSITIONS)
completed_through <- max(live$week, na.rm = TRUE)
if (!is.finite(completed_through)) stop("No finalized current-season training rows.")

config_for32 <- function(pos) {
  cc <- config31[config31$position == pos, , drop = FALSE]
  grid <- wk31_param_grid(pos)
  if (nrow(cc) && "config_id" %in% names(cc)) {
    z <- grid[grid$config_id == as.character(cc$config_id[1]), , drop = FALSE]
    if (nrow(z)) return(z[1, , drop = FALSE])
  }
  grid[1, , drop = FALSE]
}

score_metrics32 <- function(m) {
  val <- function(cohort, metric) {
    z <- m[m$cohort == cohort, , drop = FALSE]
    if (!nrow(z)) return(NA_real_)
    as.numeric(z[[metric]][1])
  }
  vals <- c(
    0.45 * val("Starter","MAE"),
    0.30 * val("Relevant","MAE"),
    0.20 * val("All","MAE"),
    0.025 * val("Starter","RMSE"),
    0.025 * val("Relevant","RMSE")
  )
  if (any(!is.finite(vals))) return(Inf)
  sum(vals)
}

historical_years <- intersect(c(2024L, 2025L), sort(unique(historical$season)))
hist_rows <- list()
feature_rows <- list()

for (pos in POSITIONS) {
  cfg <- config_for32(pos)
  for (yy in historical_years) {
    train0 <- historical |> dplyr::filter(position == pos, season < yy)
    test0 <- historical |> dplyr::filter(position == pos, season == yy)
    if (nrow(train0) < 100 || nrow(test0) < 25) next

    train0 <- wk32_add_factors(train0)
    test0 <- wk32_add_factors(test0)

    for (variant in c("full","compact")) {
      feats <- wk32_variant_features(train0, pos, variant)
      fit <- wk31_fit(
        train0, pos, cfg, features = feats,
        seed = SEED + yy * 100L + match(pos, POSITIONS) * 10L
      )
      test0$.pred32 <- wk31_predict(fit, test0)
      mm <- wk32_cohort_metrics(test0, ".pred32", pos)
      sc <- score_metrics32(mm) + wk32_complexity_penalty(length(feats))

      for (i in seq_len(nrow(mm))) {
        hist_rows[[length(hist_rows) + 1L]] <- data.frame(
          position = pos, target_year = yy, variant = variant,
          cohort = mm$cohort[i], n = mm$n[i], MAE = mm$MAE[i], RMSE = mm$RMSE[i],
          correlation = mm$correlation[i], rank_correlation = mm$rank_correlation[i],
          decision_score = sc, n_features = length(feats),
          stringsAsFactors = FALSE
        )
      }
      feature_rows[[length(feature_rows) + 1L]] <- data.frame(
        position = pos, target_year = yy, variant = variant,
        n_features = length(feats), features = paste(feats, collapse = ";"),
        stringsAsFactors = FALSE
      )
    }
  }
}

hist_metrics <- dplyr::bind_rows(hist_rows)
if (!nrow(hist_metrics)) stop("3.2 historical tournament produced no metrics.")
readr::write_csv(hist_metrics, "output/weekly_3_2_historical_metrics.csv")
readr::write_csv(dplyr::bind_rows(feature_rows), "output/weekly_3_2_feature_sets_by_fold.csv")

fold_scores <- hist_metrics |>
  dplyr::filter(cohort == "All") |>
  dplyr::select(position, target_year, variant, decision_score, n_features) |>
  dplyr::distinct()

summary32 <- fold_scores |>
  dplyr::group_by(position, variant) |>
  dplyr::summarise(
    mean_score = mean(decision_score),
    max_score = max(decision_score),
    mean_features = mean(n_features),
    .groups = "drop"
  )

full_scores <- summary32 |>
  dplyr::filter(variant == "full") |>
  dplyr::select(position, full_mean_score = mean_score)
summary32 <- summary32 |>
  dplyr::left_join(full_scores, by = "position") |>
  dplyr::mutate(hist_improvement_pct = 100 * (full_mean_score - mean_score) / pmax(full_mean_score, 1e-9))

selection_rows <- list()
for (pos in POSITIONS) {
  s <- summary32[summary32$position == pos, , drop = FALSE]
  f <- fold_scores[fold_scores$position == pos, , drop = FALSE]
  full_by_year <- f[f$variant == "full", c("target_year","decision_score"), drop = FALSE]
  names(full_by_year)[2] <- "full_score"

  eligible <- s[s$variant != "full" & s$hist_improvement_pct >= 0.10, , drop = FALSE]
  if (nrow(eligible)) {
    stable <- vapply(seq_len(nrow(eligible)), function(i) {
      vv <- eligible$variant[i]
      z <- merge(
        f[f$variant == vv, c("target_year","decision_score"), drop = FALSE],
        full_by_year, by = "target_year", all.x = TRUE
      )
      if (!nrow(z)) return(FALSE)
      ratios <- z$decision_score / pmax(z$full_score, 1e-9)
      sum(z$decision_score < z$full_score, na.rm = TRUE) >= 1L &&
        max(ratios, na.rm = TRUE) <= 1.005
    }, logical(1))
    eligible <- eligible[stable, , drop = FALSE]
  }

  chosen <- if (nrow(eligible)) {
    eligible[order(eligible$mean_score, eligible$mean_features), , drop = FALSE][1, ]
  } else {
    s[s$variant == "full", , drop = FALSE][1, ]
  }

  selection_rows[[length(selection_rows) + 1L]] <- data.frame(
    position = pos,
    selected_variant = chosen$variant,
    historical_mean_score = chosen$mean_score,
    historical_full_score = chosen$full_mean_score,
    historical_improvement_pct = chosen$hist_improvement_pct,
    mean_features = chosen$mean_features,
    selection_basis = "2024-2025 chronological OOF only",
    stringsAsFactors = FALSE
  )
}
selection <- dplyr::bind_rows(selection_rows) |>
  dplyr::rename(architecture_improvement_pct = historical_improvement_pct)

# -------------------------------------------------------------------------
# 1B) Screen new factors one-at-a-time on the same historical OOF folds.
#     Noise architecture is already locked above. This prevents a bundle of
#     weak factors from hiding one genuinely useful interaction.
# -------------------------------------------------------------------------
factor_screen_rows <- list()
factor_combo_rows <- list()

for (pos in POSITIONS) {
  base_variant <- selection$selected_variant[selection$position == pos][1]
  cfg <- config_for32(pos)

  for (yy in historical_years) {
    train0 <- wk32_add_factors(historical |> dplyr::filter(position == pos, season < yy))
    test0 <- wk32_add_factors(historical |> dplyr::filter(position == pos, season == yy))
    if (nrow(train0) < 100 || nrow(test0) < 25) next

    base_feats <- wk32_variant_features(train0, pos, base_variant)
    for (factor in c("NONE", MODEL32_FACTOR_FEATURES)) {
      feats <- if (factor == "NONE") base_feats else unique(c(base_feats, factor))
      fit <- wk31_fit(
        train0, pos, cfg, features = feats,
        seed = SEED + 32100L + yy * 10L + match(pos, POSITIONS)
      )
      test0$.pred32_factor <- wk31_predict(fit, test0)
      mm <- wk32_cohort_metrics(test0, ".pred32_factor", pos)
      factor_screen_rows[[length(factor_screen_rows) + 1L]] <- data.frame(
        position = pos, target_year = yy, factor = factor,
        decision_score = score_metrics32(mm) + wk32_complexity_penalty(length(feats)),
        n_features = length(feats), stringsAsFactors = FALSE
      )
    }
  }
}

factor_screen <- dplyr::bind_rows(factor_screen_rows)
factor_summary <- factor_screen |>
  dplyr::group_by(position, factor) |>
  dplyr::summarise(
    mean_score = mean(decision_score),
    max_score = max(decision_score),
    .groups = "drop"
  )

base_factor_score <- factor_summary |>
  dplyr::filter(factor == "NONE") |>
  dplyr::select(position, base_factor_mean_score = mean_score)
factor_summary <- factor_summary |>
  dplyr::left_join(base_factor_score, by = "position") |>
  dplyr::mutate(
    improvement_vs_base_pct =
      100 * (base_factor_mean_score - mean_score) / pmax(base_factor_mean_score, 1e-9)
  )

selected_factor_rows <- list()
for (pos in POSITIONS) {
  fs <- factor_summary[factor_summary$position == pos & factor_summary$factor != "NONE", , drop = FALSE]
  folds <- factor_screen[factor_screen$position == pos, , drop = FALSE]
  base_fold <- folds[folds$factor == "NONE", c("target_year","decision_score"), drop = FALSE]
  names(base_fold)[2] <- "base_score"

  stable <- vapply(seq_len(nrow(fs)), function(i) {
    ff <- fs$factor[i]
    z <- merge(
      folds[folds$factor == ff, c("target_year","decision_score"), drop = FALSE],
      base_fold, by = "target_year", all.x = TRUE
    )
    if (!nrow(z)) return(FALSE)
    ratio <- z$decision_score / pmax(z$base_score, 1e-9)
    fs$improvement_vs_base_pct[i] >= 0.05 &&
      sum(z$decision_score < z$base_score, na.rm = TRUE) >= 1L &&
      max(ratio, na.rm = TRUE) <= 1.0025
  }, logical(1))

  eligible <- fs[stable, , drop = FALSE] |>
    dplyr::arrange(mean_score)
  candidates <- list(character())
  labels <- "NONE"
  if (nrow(eligible)) {
    top <- head(as.character(eligible$factor), 3L)
    for (ff in top) {
      candidates[[length(candidates) + 1L]] <- ff
      labels <- c(labels, ff)
    }
    if (length(top) >= 2L) {
      candidates[[length(candidates) + 1L]] <- top[1:2]
      labels <- c(labels, paste(top[1:2], collapse = "+"))
    }
    if (length(top) >= 3L) {
      candidates[[length(candidates) + 1L]] <- top[1:3]
      labels <- c(labels, paste(top[1:3], collapse = "+"))
    }
  }

  combo_scores <- list()
  for (ci in seq_along(candidates)) {
    factors <- candidates[[ci]]
    fold_sc <- numeric()
    feat_n <- numeric()
    for (yy in historical_years) {
      train0 <- wk32_add_factors(historical |> dplyr::filter(position == pos, season < yy))
      test0 <- wk32_add_factors(historical |> dplyr::filter(position == pos, season == yy))
      if (nrow(train0) < 100 || nrow(test0) < 25) next
      base_feats <- wk32_variant_features(train0, pos, selection$selected_variant[selection$position == pos][1])
      feats <- unique(c(base_feats, factors))
      fit <- wk31_fit(
        train0, pos, config_for32(pos), features = feats,
        seed = SEED + 32200L + yy * 10L + match(pos, POSITIONS)
      )
      test0$.pred_combo32 <- wk31_predict(fit, test0)
      mm <- wk32_cohort_metrics(test0, ".pred_combo32", pos)
      fold_sc <- c(fold_sc, score_metrics32(mm) + wk32_complexity_penalty(length(feats)))
      feat_n <- c(feat_n, length(feats))
      factor_combo_rows[[length(factor_combo_rows) + 1L]] <- data.frame(
        position = pos, target_year = yy, factor_combo = labels[ci],
        decision_score = tail(fold_sc, 1), n_features = length(feats),
        stringsAsFactors = FALSE
      )
    }
    if (length(fold_sc)) {
      combo_scores[[length(combo_scores) + 1L]] <- data.frame(
        factor_combo = labels[ci], mean_score = mean(fold_sc),
        max_score = max(fold_sc), mean_features = mean(feat_n),
        stringsAsFactors = FALSE
      )
    }
  }

  cs <- dplyr::bind_rows(combo_scores)
  base_score <- cs$mean_score[cs$factor_combo == "NONE"][1]
  if (!is.finite(base_score)) base_score <- Inf
  cs$improvement_vs_base_pct <- 100 * (base_score - cs$mean_score) / pmax(base_score, 1e-9)

  cand <- cs[cs$factor_combo != "NONE" & cs$improvement_vs_base_pct >= 0.05, , drop = FALSE]
  chosen_combo <- "NONE"
  if (nrow(cand)) {
    # Preserve fold stability: no historical OOF year may worsen >0.25%.
    stable_combo <- vapply(seq_len(nrow(cand)), function(i) {
      lab <- cand$factor_combo[i]
      z <- dplyr::bind_rows(factor_combo_rows) |>
        dplyr::filter(position == pos, factor_combo %in% c("NONE", lab)) |>
        dplyr::select(target_year, factor_combo, decision_score) |>
        tidyr::pivot_wider(names_from = factor_combo, values_from = decision_score)
      if (!all(c("NONE", lab) %in% names(z))) return(FALSE)
      max(z[[lab]] / pmax(z$NONE, 1e-9), na.rm = TRUE) <= 1.0025
    }, logical(1))
    cand <- cand[stable_combo, , drop = FALSE]
    if (nrow(cand)) chosen_combo <- cand$factor_combo[which.min(cand$mean_score)]
  }

  selected_factor_rows[[length(selected_factor_rows) + 1L]] <- data.frame(
    position = pos,
    selected_factors = if (chosen_combo == "NONE") "" else chosen_combo,
    factor_mean_score = cs$mean_score[cs$factor_combo == chosen_combo][1],
    factor_gain_vs_base_pct = cs$improvement_vs_base_pct[cs$factor_combo == chosen_combo][1],
    stringsAsFactors = FALSE
  )
}

selected_factors <- dplyr::bind_rows(selected_factor_rows)
selection <- selection |>
  dplyr::left_join(selected_factors, by = "position") |>
  dplyr::mutate(
    selected_factors = dplyr::coalesce(selected_factors, ""),
    historical_mean_score = dplyr::if_else(
      is.finite(factor_mean_score), factor_mean_score, historical_mean_score
    ),
    factor_gain_vs_base_pct = dplyr::coalesce(factor_gain_vs_base_pct, 0),
    historical_improvement_pct = 100 * (
      1 -
        (1 - dplyr::coalesce(architecture_improvement_pct, 0) / 100) *
        (1 - factor_gain_vs_base_pct / 100)
    ),
    mean_features = mean_features +
      dplyr::if_else(nzchar(selected_factors),
        lengths(strsplit(selected_factors, "\\+")), 0L)
  )

readr::write_csv(factor_screen, "output/weekly_3_2_factor_screen_folds.csv")
readr::write_csv(factor_summary, "output/weekly_3_2_factor_screen_summary.csv")
readr::write_csv(dplyr::bind_rows(factor_combo_rows), "output/weekly_3_2_factor_combo_folds.csv")
readr::write_csv(selection, "output/weekly_3_2_selection.csv")

# -------------------------------------------------------------------------
# Feature audit on the complete immutable historical set.
# -------------------------------------------------------------------------
audit_rows <- list()
final_feature_rows <- list()
for (pos in POSITIONS) {
  d <- wk32_add_factors(historical |> dplyr::filter(position == pos))
  sel <- selection$selected_variant[selection$position == pos][1]
  feats <- wk32_variant_features(d, pos, sel)
  sf <- selection$selected_factors[selection$position == pos][1]
  addf <- if (is.na(sf) || !nzchar(sf)) character() else strsplit(sf, "\\+", fixed = FALSE)[[1]]
  feats <- unique(c(feats, addf))
  audit <- wk32_feature_audit(d, pos, unique(c(MODEL31_SAFE_FEATURES, MODEL32_FACTOR_FEATURES)))
  audit$position <- pos
  audit$selected_variant <- sel
  audit$selected_for_2026_test <- audit$feature %in% feats
  audit_rows[[length(audit_rows) + 1L]] <- audit
  final_feature_rows[[length(final_feature_rows) + 1L]] <- data.frame(
    position = pos, selected_variant = sel, n_features = length(feats),
    features = paste(feats, collapse = ";"), stringsAsFactors = FALSE
  )
}
readr::write_csv(dplyr::bind_rows(audit_rows), "output/weekly_3_2_feature_audit.csv")
readr::write_csv(dplyr::bind_rows(final_feature_rows), "output/weekly_3_2_selected_features.csv")

# -------------------------------------------------------------------------
# Individual factor effects: permutation tests on historical OOF seasons only.
# -------------------------------------------------------------------------
perm_rows <- list()
for (pos in POSITIONS) {
  selected_variant <- selection$selected_variant[selection$position == pos][1]
  sf <- selection$selected_factors[selection$position == pos][1]
  selected_factor_vec <- if (is.na(sf) || !nzchar(sf)) character() else strsplit(sf, "\\+", fixed = FALSE)[[1]]
  if (!length(selected_factor_vec)) next
  cfg <- config_for32(pos)

  for (yy in historical_years) {
    train0 <- wk32_add_factors(historical |> dplyr::filter(position == pos, season < yy))
    test0 <- wk32_add_factors(historical |> dplyr::filter(position == pos, season == yy))
    if (nrow(train0) < 100 || nrow(test0) < 25) next
    feats <- unique(c(wk32_variant_features(train0, pos, selected_variant), selected_factor_vec))
    fit <- wk31_fit(train0, pos, cfg, features = feats,
                    seed = SEED + 32000L + yy + match(pos, POSITIONS) * 100L)
    base_pred <- wk31_predict(fit, test0)
    base_mae <- mean(abs(wk32_num(test0$weekly_fppg, NA_real_) - base_pred), na.rm = TRUE)

    for (factor in intersect(selected_factor_vec, feats)) {
      tp <- test0
      set.seed(SEED + yy + match(factor, MODEL32_FACTOR_FEATURES) * 100L + match(pos, POSITIONS))
      tp[[factor]] <- sample(tp[[factor]], nrow(tp), replace = FALSE)
      pp <- wk31_predict(fit, tp)
      perm_mae <- mean(abs(wk32_num(tp$weekly_fppg, NA_real_) - pp), na.rm = TRUE)
      perm_rows[[length(perm_rows) + 1L]] <- data.frame(
        position = pos, target_year = yy, factor = factor,
        baseline_MAE = base_mae, permuted_MAE = perm_mae,
        delta_MAE_if_permuted = perm_mae - base_mae,
        stringsAsFactors = FALSE
      )
    }
  }
}
perm <- dplyr::bind_rows(perm_rows)
if (nrow(perm)) {
  readr::write_csv(perm, "output/weekly_3_2_factor_permutation.csv")
  factor_summary <- perm |>
    dplyr::group_by(position, factor) |>
    dplyr::summarise(
      mean_delta_MAE_if_permuted = mean(delta_MAE_if_permuted),
      min_delta_MAE_if_permuted = min(delta_MAE_if_permuted),
      years_positive = sum(delta_MAE_if_permuted > 0),
      years_tested = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(position, dplyr::desc(mean_delta_MAE_if_permuted))
  readr::write_csv(factor_summary, "output/weekly_3_2_factor_effects.csv")
}

factor_attribution <- lapply(POSITIONS, function(pos) {
  sf <- selection$selected_factors[selection$position == pos][1]
  factors <- if (is.na(sf) || !nzchar(sf)) character() else strsplit(sf, "\\+", fixed = FALSE)[[1]]
  if (!length(factors)) {
    return(data.frame(position = pos, factor_attribution_ok = TRUE,
                      factor_attribution_failed = "", stringsAsFactors = FALSE))
  }
  if (!exists("factor_summary") || !nrow(factor_summary)) {
    return(data.frame(position = pos, factor_attribution_ok = FALSE,
                      factor_attribution_failed = paste(factors, collapse = ";"),
                      stringsAsFactors = FALSE))
  }
  z <- factor_summary[factor_summary$position == pos & factor_summary$factor %in% factors, , drop = FALSE]
  passed <- z$mean_delta_MAE_if_permuted > 0 & z$years_positive >= 1
  failed <- setdiff(factors, as.character(z$factor[passed]))
  data.frame(
    position = pos,
    factor_attribution_ok = length(failed) == 0L,
    factor_attribution_failed = paste(failed, collapse = ";"),
    stringsAsFactors = FALSE
  )
}) |> dplyr::bind_rows()

selection <- selection |>
  dplyr::left_join(factor_attribution, by = "position")
readr::write_csv(selection, "output/weekly_3_2_selection.csv")

# -------------------------------------------------------------------------
# Independent 2026 rolling OOF. Variant is already locked above.
# -------------------------------------------------------------------------
if (!"captured_at" %in% names(archive)) archive$captured_at <- ""
archive <- archive |>
  dplyr::mutate(
    week = suppressWarnings(as.integer(week)),
    player_id = as.character(player_id),
    position = toupper(as.character(position)),
    captured_num = suppressWarnings(as.numeric(as.POSIXct(captured_at, tz = "UTC"))),
    projection = suppressWarnings(as.numeric(projection))
  ) |>
  dplyr::filter(week <= completed_through, position %in% POSITIONS, is.finite(projection)) |>
  dplyr::arrange(week, player_id, captured_num) |>
  dplyr::group_by(week, player_id, position) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup() |>
  dplyr::select(week, player_id, position, production_base_31 = projection)

starter_cut <- c(QB = 16, RB = 10, WR = 10, TE = 8)
relevant_cut <- c(QB = 12, RB = 6, WR = 6, TE = 4.5)

live_oof_rows <- list()
promotion_rows <- list()
final_model_rows <- list()

for (pos in POSITIONS) {
  selected_variant <- selection$selected_variant[selection$position == pos][1]
  hist_gain <- selection$historical_improvement_pct[selection$position == pos][1]
  cfg <- config_for32(pos)
  alpha <- as.numeric(MODEL31_WEEKLY_ALPHA[[pos]])

  live_pos <- live |>
    dplyr::filter(position == pos, week <= completed_through) |>
    dplyr::left_join(archive |> dplyr::filter(position == pos),
                     by = c("week","player_id","position")) |>
    dplyr::filter(is.finite(suppressWarnings(as.numeric(production_base_31))))

  weeks <- sort(unique(live_pos$week))
  for (target_week in weeks) {
    test0 <- live_pos |> dplyr::filter(week == target_week)
    if (!nrow(test0)) next
    train0 <- dplyr::bind_rows(
      historical |> dplyr::filter(position == pos),
      live |> dplyr::filter(position == pos, week < target_week)
    )
    if (nrow(train0) < 100) next

    train0 <- wk32_add_factors(train0)
    test0 <- wk32_add_factors(test0)
    sf <- selection$selected_factors[selection$position == pos][1]
    addf <- if (is.na(sf) || !nzchar(sf)) character() else strsplit(sf, "\\+", fixed = FALSE)[[1]]
    feats <- unique(c(wk32_variant_features(train0, pos, selected_variant), addf))

    fit <- wk31_fit(
      train0, pos, cfg, features = feats,
      seed = SEED + CURRENT_SEASON * 100L + target_week * 10L + match(pos, POSITIONS) + 32000L
    )
    direct <- wk31_predict(fit, test0)
    candidate <- pmax(
      0,
      wk32_num(test0$production_base_31) +
        alpha * (direct - wk32_num(test0$production_base_31))
    )

    test0$actual_fppg <- wk32_num(test0$weekly_fppg, NA_real_)
    test0$direct_fppg_31 <- direct
    test0$candidate_fppg_31 <- candidate
    test0$blend_alpha_31 <- alpha
    test0$starter_cohort <- wk32_num(test0$production_base_31) >= starter_cut[[pos]]
    test0$relevant_cohort <- wk32_num(test0$production_base_31) >= relevant_cut[[pos]]
    test0$variant_32 <- selected_variant
    test0$n_features_32 <- length(feats)
    live_oof_rows[[length(live_oof_rows) + 1L]] <- test0 |>
      dplyr::select(
        season, week, player_id, player_display_name, position, team,
        actual_fppg, production_base_31, direct_fppg_31, candidate_fppg_31,
        blend_alpha_31, starter_cohort, relevant_cohort, feature_source,
        variant_32, n_features_32
      )
  }

  pos_oof <- dplyr::bind_rows(live_oof_rows) |> dplyr::filter(position == pos)
  gate <- fm31_weekly_live_gate(pos_oof, pos)
  base_promote <- isTRUE(gate$promoted[1])
  checks32 <- c(
    selected_new_architecture =
      selected_variant != "full" ||
      nzchar(selection$selected_factors[selection$position == pos][1]),
    historical_gain = is.finite(hist_gain) && hist_gain >= 0.10,
    live_gate = base_promote,
    live_all_gain = is.finite(gate$all_mae_gain_pct[1]) && gate$all_mae_gain_pct[1] >= 0.15,
    live_starter_gain = is.finite(gate$starter_mae_gain_pct[1]) && gate$starter_mae_gain_pct[1] > 0,
    live_confidence = is.finite(gate$bootstrap_p_improves[1]) && gate$bootstrap_p_improves[1] >= 0.60,
    factor_attribution = isTRUE(selection$factor_attribution_ok[selection$position == pos][1])
  )
  promote32 <- all(checks32)

  promotion_rows[[length(promotion_rows) + 1L]] <- data.frame(
    position = pos,
    selected_variant = selected_variant,
    historical_improvement_pct = hist_gain,
    factor_attribution_ok = selection$factor_attribution_ok[selection$position == pos][1],
    factor_attribution_failed = selection$factor_attribution_failed[selection$position == pos][1],
    live_n = gate$n[1], live_weeks = gate$weeks[1], live_starter_n = gate$starter_n[1],
    live_all_mae_gain_pct = gate$all_mae_gain_pct[1],
    live_starter_mae_gain_pct = gate$starter_mae_gain_pct[1],
    live_week_win_rate = gate$week_win_rate[1],
    live_bootstrap_p_improves = gate$bootstrap_p_improves[1],
    promoted_32 = promote32,
    failed_checks = paste(names(checks32)[!checks32], collapse = ";"),
    evaluated_through_week = completed_through,
    stringsAsFactors = FALSE
  )

  if (promote32) {
    final_train <- wk32_add_factors(dplyr::bind_rows(
      historical |> dplyr::filter(position == pos),
      live |> dplyr::filter(position == pos, week <= completed_through)
    ))
    sf_final <- selection$selected_factors[selection$position == pos][1]
    addf_final <- if (is.na(sf_final) || !nzchar(sf_final)) character() else strsplit(sf_final, "\\+", fixed = FALSE)[[1]]
    feats <- unique(c(wk32_variant_features(final_train, pos, selected_variant), addf_final))
    fit <- wk31_fit(
      final_train, pos, cfg, features = feats,
      seed = SEED + 32999L + completed_through * 100L + match(pos, POSITIONS)
    )
    model_path <- paste0("models/weekly_3_2_live_", pos, ".json")
    meta_path <- paste0("models/weekly_3_2_live_", pos, "_meta.rds")
    xgboost::xgb.save(fit$model, model_path)
    saveRDS(list(
      position = pos,
      features = feats,
      selected_variant = selected_variant,
      selected_factors = selection$selected_factors[selection$position == pos][1],
      n_features = length(feats),
      config = cfg,
      nrounds = fit$nrounds,
      n_train = fit$n_train,
      trained_through_season = CURRENT_SEASON,
      trained_through_week = completed_through,
      blend_alpha = alpha,
      historical_improvement_pct = hist_gain,
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ), meta_path)
    final_model_rows[[length(final_model_rows) + 1L]] <- data.frame(
      position = pos, model_path = model_path, meta_path = meta_path,
      stringsAsFactors = FALSE
    )
  }
}

live_oof <- dplyr::bind_rows(live_oof_rows)
promotion <- dplyr::bind_rows(promotion_rows)
readr::write_csv(live_oof, "output/weekly_3_2_live_oof.csv")
readr::write_csv(promotion, "output/weekly_3_2_promotion.csv")

if (nrow(live_oof)) {
  residual <- live_oof |>
    dplyr::mutate(
      residual = actual_fppg - candidate_fppg_31,
      abs_error = abs(residual)
    )
  readr::write_csv(residual, "output/weekly_3_2_residual_pool.csv")
}

lines <- c(
  "FANTASY MODEL 3.2 - SIGNAL OPTIMIZATION TOURNAMENT",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("Independent live evaluation through Week: ", completed_through),
  "",
  "ARCHITECTURE SELECTION (2024-2025 only)"
)
for (i in seq_len(nrow(selection))) {
  s <- selection[i, ]
  lines <- c(lines, sprintf(
    "%s | %s | factors=%s | historical score gain %.3f%% | mean features %.1f",
    s$position, s$selected_variant,
    ifelse(nzchar(s$selected_factors), s$selected_factors, "none"),
    s$historical_improvement_pct, s$mean_features
  ))
}
lines <- c(lines, "", "LIVE 2026 PROMOTION")
for (i in seq_len(nrow(promotion))) {
  p <- promotion[i, ]
  lines <- c(lines, sprintf(
    "%s | %s | all MAE gain %.3f%% | starter gain %.3f%% | week win %.1f%% | bootstrap %.3f | %s",
    p$position,
    ifelse(p$promoted_32, "PROMOTE 3.2", "KEEP 3.1"),
    p$live_all_mae_gain_pct, p$live_starter_mae_gain_pct,
    100 * p$live_week_win_rate, p$live_bootstrap_p_improves,
    ifelse(nchar(p$failed_checks), paste0("failed: ", p$failed_checks), "all gates passed")
  ))
}
writeLines(lines, "output/weekly_3_2_quality_report.txt")

cat("\n[3.2] SIGNAL OPTIMIZATION COMPLETE\n")
print(selection)
print(promotion)
