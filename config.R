# ============================================================
# Fantasy Football ML Model Configuration
# FANTASY MODEL 2.3 - HISTORICAL META-CALIBRATION + 2.0 SEASON ENGINE
# ============================================================

PROJECT_VERSION <- "2.4.3"
MODEL_LINEAGE <- "Validated 2.0 season prior + locked 2.3/2.3.2 weekly benchmarks + 2.4 signal-attribution, player-talent prior, hierarchical player-response and strict promotion gate"

CURRENT_SEASON <- 2026
TRAIN_START <- 2016
TRAIN_END <- CURRENT_SEASON - 1
HISTORY_LOOKBACK_YEARS <- 2
CONTEXT_START <- 2018

POSITIONS <- c("QB", "RB", "WR", "TE")

# Fantasy scoring: half-PPR default
SCORING <- list(
  pass_yd = 0.04,
  pass_td = 4,
  interception = -2,
  rush_yd = 0.1,
  rush_td = 6,
  reception = 0.5,
  rec_yd = 0.1,
  rec_td = 6,
  fumble_lost = -2,
  two_pt = 2
)

MIN_GAMES <- 4
DYNASTY_YEARS <- 4
SEED <- 42

# 2.0 stays mobile-safe: no XGBoost or compilation-heavy packages.
MODEL_ENGINE <- "rpart_ensemble"
RPART_ENSEMBLE_TREES <- 16
VALIDATION_ENSEMBLE_TREES <- 8
TIER_ENSEMBLE_TREES <- 12
VALIDATION_TIER_TREES <- 6
BREAKOUT_ENSEMBLE_TREES <- 12
VALIDATION_BREAKOUT_TREES <- 6
RPART_ROW_SAMPLE_FRAC <- 0.85
RPART_FEATURE_SAMPLE_FRAC <- 0.80

RPART_CONTROL <- list(
  cp = 0.0025,
  minsplit = 18,
  minbucket = 6,
  maxdepth = 9,
  xval = 0
)

# The validated direct engine uses a true multi-season walk-forward backtest.
VALIDATION_YEARS <- 4
BLEND_WEIGHT_CANDIDATES <- c(0.50, 0.65, 0.80, 0.90, 1.00)
DEFAULT_MODEL_PREDICTION_WEIGHT <- 0.80
CALIBRATION_MIN_ROWS <- 20
PROJECTION_INTERVAL_Z <- 1.281552
BREAKOUT_GAIN_FPPG <- 3
BREAKOUT_MIN_FPPG <- 8

# Replacement + elite levels for ranking diagnostics.
REPLACEMENT_RANK <- c(QB = 14, RB = 30, WR = 36, TE = 14)
ELITE_RANK <- c(QB = 6, RB = 12, WR = 12, TE = 6)
FANTASY_TIER_CUTOFFS <- list(
  QB = c(Top5 = 5, Top12 = 12),
  RB = c(Top12 = 12, Top24 = 24),
  WR = c(Top12 = 12, Top24 = 24, Top36 = 36),
  TE = c(Top6 = 6, Top12 = 12)
)
PROJECTABLE_ROSTER_STATUSES <- c("ACT", "INA", "PUP", "RES", "SUS", "EXE", "E14")

# ------------------------------------------------------------
# 1.2 FEATURE ARCHITECTURE
#
# The validated 1.0 feature set remains common to every position.
# Context is now position-specific so receiver-style route/location
# variables cannot dilute RB/QB models and vice versa.
# ------------------------------------------------------------
CORE_MODEL_FEATURES <- c(
  "prior_fppg", "two_year_fppg", "recent_weighted_fppg", "fppg_trend",
  "prior_fantasy_points", "prior_games", "two_year_games",
  "prior_targets_pg", "two_year_targets_pg", "prior_carries_pg", "two_year_carries_pg",
  "prior_pass_attempts_pg", "two_year_pass_attempts_pg",
  "prior_rec_yd_pg", "prior_rush_yd_pg", "prior_pass_yd_pg",
  "prior_catch_rate", "prior_yards_per_target", "prior_yards_per_carry", "prior_yards_per_attempt",
  "prior_target_share", "prior_air_yards_share", "prior_wopr",
  "prior_pass_td_rate", "prior_rush_td_rate", "prior_rec_td_rate",
  "career_fppg_before", "peak_fppg_before", "career_games_before",
  "experience", "is_rookie", "years_since_last_season",
  "age", "age_squared", "height", "weight",
  "draft_round", "draft_pick", "draft_capital_score",
  "team_prior_pass_yd_pg", "team_prior_rush_yd_pg",
  "team_prior_pass_attempts_pg", "team_prior_carries_pg",
  "team_prior_points_pg"
)

QB_CONTEXT_FEATURES <- c(
  "context_available",
  "team_prior_pass_rate", "team_prior_neutral_pass_rate", "team_prior_plays_pg",
  "team_prior_redzone_pass_rate", "team_prior_deep_throw_rate",
  "team_prior_shotgun_rate", "team_prior_no_huddle_rate", "team_prior_avg_air_yards",
  "player_qb_context_available", "player_qb_prior_avg_air_yards",
  "player_qb_prior_deep_throw_rate", "player_qb_prior_middle_throw_rate", "player_qb_prior_redzone_throw_rate",
  "player_qb_prior_wr_target_rate", "player_qb_prior_te_target_rate", "player_qb_prior_rb_target_rate",
  "player_qb_prior_ngs_time_to_throw", "player_qb_prior_ngs_aggressiveness", "player_qb_prior_ngs_cpoe",
  "qb_team_continuity"
)

RB_CONTEXT_FEATURES <- c(
  "context_available", "receiver_context_available", "qb_context_available",
  "team_prior_rush_rate", "team_prior_neutral_rush_rate", "team_prior_redzone_rush_rate",
  "team_prior_plays_pg", "team_prior_shotgun_rate",
  "qb_prior_rb_target_rate",
  "rec_prior_short_target_rate", "rec_prior_redzone_target_rate", "rec_prior_route_screen",
  "rb_prior_team_carry_share", "rb_prior_touch_opportunity", "rb_receiving_fit",
  "team_target_opportunity"
)

WR_CONTEXT_FEATURES <- c(
  "context_available", "receiver_context_available", "qb_context_available",
  "team_prior_pass_rate", "team_prior_neutral_pass_rate", "team_prior_plays_pg",
  "team_prior_redzone_pass_rate", "team_prior_deep_throw_rate", "team_prior_avg_air_yards",
  "team_prior_shotgun_rate", "team_prior_no_huddle_rate",
  "rec_prior_avg_depth_target", "rec_prior_deep_target_rate", "rec_prior_redzone_target_rate",
  "rec_prior_route_vertical", "rec_prior_route_in_break", "rec_prior_route_out_break",
  "rec_prior_route_screen", "rec_prior_route_hitch",
  "rec_prior_ngs_avg_cushion", "rec_prior_ngs_avg_separation",
  "alignment_available", "rec_prior_slot_rate", "rec_prior_wide_rate",
  "qb_prior_avg_air_yards", "qb_prior_deep_throw_rate", "qb_prior_redzone_throw_rate",
  "qb_prior_wr_target_rate", "qb_prior_ngs_time_to_throw", "qb_prior_ngs_cpoe",
  "qb_receiver_depth_fit", "qb_receiver_location_fit", "qb_receiver_route_fit",
  "wr_context_fit_score", "team_target_opportunity"
)

TE_CONTEXT_FEATURES <- c(
  "context_available", "receiver_context_available", "qb_context_available",
  "team_prior_pass_rate", "team_prior_neutral_pass_rate", "team_prior_plays_pg",
  "team_prior_redzone_pass_rate", "team_prior_middle_throw_rate",
  "rec_prior_short_target_rate", "rec_prior_middle_target_rate", "rec_prior_redzone_target_rate",
  "rec_prior_route_in_break", "rec_prior_route_hitch", "rec_prior_route_screen",
  "rec_prior_ngs_avg_separation", "alignment_available", "rec_prior_inline_rate",
  "qb_prior_middle_throw_rate", "qb_prior_redzone_throw_rate", "qb_prior_te_target_rate",
  "qb_prior_ngs_time_to_throw",
  "te_middle_fit", "te_redzone_fit", "te_route_fit", "te_context_fit_score",
  "team_target_opportunity"
)

POSITION_CONTEXT_FEATURES <- list(
  QB = QB_CONTEXT_FEATURES,
  RB = RB_CONTEXT_FEATURES,
  WR = WR_CONTEXT_FEATURES,
  TE = TE_CONTEXT_FEATURES
)

ALL_MODEL_FEATURES <- unique(c(
  CORE_MODEL_FEATURES, QB_CONTEXT_FEATURES, RB_CONTEXT_FEATURES, WR_CONTEXT_FEATURES, TE_CONTEXT_FEATURES
))

# Backward-compatible alias used by the feature-table preparation script.
MODEL_FEATURES <- ALL_MODEL_FEATURES

get_model_features <- function(position, available_names = NULL) {
  pos <- toupper(as.character(position)[1])
  context <- POSITION_CONTEXT_FEATURES[[pos]]
  if (is.null(context)) context <- character()
  out <- unique(c(CORE_MODEL_FEATURES, context))
  if (!is.null(available_names)) out <- intersect(out, available_names)
  out
}

COMP_FEATURES <- c(
  "age", "height", "weight", "draft_pick", "experience", "is_rookie",
  "recent_weighted_fppg", "prior_targets_pg", "prior_carries_pg",
  "prior_pass_attempts_pg", "prior_rec_yd_pg", "prior_rush_yd_pg", "prior_pass_yd_pg",
  "prior_target_share", "prior_wopr", "career_fppg_before"
)

ensure_packages <- function(packages) {
  installed <- rownames(installed.packages())
  missing <- packages[!packages %in% installed]

  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }

  still_missing <- packages[!vapply(
    packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)
  )]

  if (length(still_missing) > 0) {
    stop("Required package(s) could not be installed: ", paste(still_missing, collapse = ", "))
  }
  invisible(TRUE)
}

resolve_model_engine <- function(requested = MODEL_ENGINE) {
  if (!identical(tolower(requested), "rpart_ensemble")) {
    stop("Fantasy Model 2.0 supports MODEL_ENGINE='rpart_ensemble' only.")
  }
  if (!requireNamespace("rpart", quietly = TRUE)) {
    stop("The rpart package is required for Fantasy Model 2.0.")
  }
  "rpart_ensemble"
}

prepare_feature_frame <- function(data, features) {
  missing <- setdiff(features, names(data))
  if (length(missing) > 0) stop("Missing model feature(s): ", paste(missing, collapse = ", "))

  out <- data[, features, drop = FALSE]
  for (nm in features) {
    out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
    out[[nm]][!is.finite(out[[nm]])] <- 0
  }
  out
}

fit_fantasy_model <- function(data, features, target_col, engine = resolve_model_engine(), seed = SEED,
                              n_trees = RPART_ENSEMBLE_TREES) {
  X <- prepare_feature_frame(data, features)
  y <- suppressWarnings(as.numeric(data[[target_col]]))
  keep <- is.finite(y)
  X <- X[keep, , drop = FALSE]
  y <- y[keep]

  if (length(y) < 20) stop("Not enough valid target values for model training.")

  set.seed(seed + nrow(X) + n_trees)
  trees <- vector("list", n_trees)
  n_rows <- max(20, ceiling(nrow(X) * RPART_ROW_SAMPLE_FRAC))
  n_features <- max(5, ceiling(length(features) * RPART_FEATURE_SAMPLE_FRAC))
  n_features <- min(n_features, length(features))
  control <- do.call(rpart::rpart.control, RPART_CONTROL)

  for (i in seq_len(n_trees)) {
    row_idx <- sample(seq_len(nrow(X)), size = n_rows, replace = TRUE)
    tree_features <- sample(features, size = n_features, replace = FALSE)
    train_frame <- X[row_idx, tree_features, drop = FALSE]
    train_frame$.target <- y[row_idx]

    tree <- rpart::rpart(
      .target ~ ., data = train_frame, method = "anova", control = control
    )
    trees[[i]] <- list(model = tree, features = tree_features)
  }

  list(engine = engine, trees = trees, features = features, n_trees = n_trees)
}

predict_fantasy_model_detail <- function(model_object, new_data) {
  if (!identical(model_object$engine, "rpart_ensemble")) {
    stop("Saved model is not a Fantasy Model 2.0-compatible rpart ensemble. Delete models/ contents and rerun MOBILE_RUN.R.")
  }

  pred_list <- lapply(model_object$trees, function(tree_obj) {
    X <- prepare_feature_frame(new_data, tree_obj$features)
    as.numeric(predict(tree_obj$model, newdata = X))
  })
  pred_mat <- do.call(cbind, pred_list)

  data.frame(
    model_prediction = rowMeans(pred_mat, na.rm = TRUE),
    model_disagreement_sd = apply(pred_mat, 1, stats::sd, na.rm = TRUE)
  )
}

predict_fantasy_model <- function(model_object, new_data) {
  predict_fantasy_model_detail(model_object, new_data)$model_prediction
}

# Rookies always use 100% model output because they have no meaningful NFL baseline.
blend_projection <- function(model_prediction, data, model_weight = DEFAULT_MODEL_PREDICTION_WEIGHT) {
  baseline <- suppressWarnings(as.numeric(data$recent_weighted_fppg))
  baseline[!is.finite(baseline)] <- 0
  rookie <- suppressWarnings(as.numeric(data$is_rookie))
  rookie[!is.finite(rookie)] <- 0

  weight <- ifelse(rookie == 1, 1, model_weight)
  pmax(0, weight * model_prediction + (1 - weight) * baseline)
}

get_position_blend_weight <- function(position, path = "output/selected_blend_weights.csv") {
  if (!file.exists(path)) return(DEFAULT_MODEL_PREDICTION_WEIGHT)
  x <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || !all(c("position", "selected_model_weight") %in% names(x))) {
    return(DEFAULT_MODEL_PREDICTION_WEIGHT)
  }
  w <- x$selected_model_weight[x$position == position]
  if (length(w) == 0 || !is.finite(w[1])) DEFAULT_MODEL_PREDICTION_WEIGHT else as.numeric(w[1])
}

get_feature_importance <- function(model_object) {
  rows <- lapply(model_object$trees, function(x) {
    imp <- x$model$variable.importance
    if (is.null(imp) || length(imp) == 0) return(NULL)
    data.frame(feature = names(imp), importance = as.numeric(imp))
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(out)
  out |>
    dplyr::group_by(feature) |>
    dplyr::summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(importance_pct = 100 * importance / sum(importance, na.rm = TRUE)) |>
    dplyr::arrange(dplyr::desc(importance_pct))
}


# ------------------------------------------------------------
# Inherited fantasy-decision classification + calibration helpers
# ------------------------------------------------------------
add_v7_targets <- function(data) {
  data |>
    dplyr::group_by(season, position) |>
    dplyr::mutate(
      v7_actual_position_rank = rank(-target_fppg, ties.method = "min"),
      v7_elite_cutoff = as.numeric(ELITE_RANK[position]),
      v7_starter_cutoff = as.numeric(REPLACEMENT_RANK[position]),
      tier_target = dplyr::case_when(
        v7_actual_position_rank <= v7_elite_cutoff ~ "Elite",
        v7_actual_position_rank <= v7_starter_cutoff ~ "Starter",
        TRUE ~ "Depth"
      ),
      breakout_target = dplyr::case_when(
        is_rookie == 1 & v7_actual_position_rank <= v7_starter_cutoff ~ "Yes",
        is_rookie != 1 & recent_weighted_fppg > 0 &
          target_fppg >= BREAKOUT_MIN_FPPG &
          target_fppg >= recent_weighted_fppg + BREAKOUT_GAIN_FPPG ~ "Yes",
        TRUE ~ "No"
      )
    ) |>
    dplyr::ungroup()
}

fit_fantasy_classifier <- function(data, features, target_col, class_levels,
                                   seed = SEED, n_trees = TIER_ENSEMBLE_TREES) {
  X <- prepare_feature_frame(data, features)
  y <- factor(as.character(data[[target_col]]), levels = class_levels)
  keep <- !is.na(y)
  X <- X[keep, , drop = FALSE]
  y <- y[keep]

  observed <- class_levels[class_levels %in% unique(as.character(y))]
  if (length(observed) < 2) {
    constant <- if (length(observed) == 1) observed[1] else class_levels[1]
    return(list(
      engine = "rpart_class_ensemble", trees = list(), features = features,
      class_levels = class_levels, constant_class = constant, n_trees = 0
    ))
  }

  set.seed(seed + nrow(X) + n_trees + length(class_levels) * 1000)
  trees <- vector("list", n_trees)
  n_rows <- max(20, ceiling(nrow(X) * RPART_ROW_SAMPLE_FRAC))
  n_features <- max(5, ceiling(length(features) * RPART_FEATURE_SAMPLE_FRAC))
  n_features <- min(n_features, length(features))
  control <- do.call(rpart::rpart.control, RPART_CONTROL)

  class_rows <- lapply(observed, function(cl) which(as.character(y) == cl))
  names(class_rows) <- observed

  for (i in seq_len(n_trees)) {
    anchors <- unlist(lapply(class_rows, function(idx) sample(idx, 1)))
    extra_n <- max(0, n_rows - length(anchors))
    row_idx <- c(anchors, sample(seq_len(nrow(X)), size = extra_n, replace = TRUE))
    tree_features <- sample(features, size = n_features, replace = FALSE)
    train_frame <- X[row_idx, tree_features, drop = FALSE]
    train_frame$.target <- factor(as.character(y[row_idx]), levels = class_levels)

    tree <- rpart::rpart(
      .target ~ ., data = train_frame, method = "class", control = control
    )
    trees[[i]] <- list(model = tree, features = tree_features)
  }

  list(
    engine = "rpart_class_ensemble", trees = trees, features = features,
    class_levels = class_levels, constant_class = NA_character_, n_trees = n_trees
  )
}

predict_fantasy_classifier <- function(model_object, new_data) {
  levels <- model_object$class_levels
  n <- nrow(new_data)
  if (n == 0) return(matrix(numeric(0), nrow = 0, ncol = length(levels), dimnames = list(NULL, levels)))

  if (length(model_object$trees) == 0) {
    out <- matrix(0, nrow = n, ncol = length(levels), dimnames = list(NULL, levels))
    out[, model_object$constant_class] <- 1
    return(out)
  }

  aligned <- lapply(model_object$trees, function(tree_obj) {
    X <- prepare_feature_frame(new_data, tree_obj$features)
    pr <- predict(tree_obj$model, newdata = X, type = "prob")
    if (is.null(dim(pr))) {
      pr <- matrix(pr, nrow = n)
    }
    out <- matrix(0, nrow = n, ncol = length(levels), dimnames = list(NULL, levels))
    common <- intersect(colnames(pr), levels)
    if (length(common) > 0) out[, common] <- pr[, common, drop = FALSE]
    rs <- rowSums(out)
    rs[!is.finite(rs) | rs <= 0] <- 1
    out / rs
  })

  Reduce(`+`, aligned) / length(aligned)
}

get_classifier_importance <- function(model_object) {
  if (length(model_object$trees) == 0) return(data.frame())
  rows <- lapply(model_object$trees, function(x) {
    imp <- x$model$variable.importance
    if (is.null(imp) || length(imp) == 0) return(NULL)
    data.frame(feature = names(imp), importance = as.numeric(imp))
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(out)
  out |>
    dplyr::group_by(feature) |>
    dplyr::summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(importance_pct = 100 * importance / sum(importance, na.rm = TRUE)) |>
    dplyr::arrange(dplyr::desc(importance_pct))
}

fit_linear_calibration <- function(predicted, actual, min_rows = CALIBRATION_MIN_ROWS) {
  keep <- is.finite(predicted) & is.finite(actual)
  predicted <- predicted[keep]
  actual <- actual[keep]
  if (length(actual) < min_rows || stats::sd(predicted) < 1e-8) {
    residual_sd <- if (length(actual) >= 2) stats::sd(actual - predicted) else 0
    if (!is.finite(residual_sd)) residual_sd <- 0
    return(c(intercept = 0, slope = 1, residual_sd = residual_sd, n = length(actual)))
  }
  fit <- stats::lm(actual ~ predicted)
  co <- stats::coef(fit)
  intercept <- as.numeric(co[1])
  slope <- as.numeric(co[2])
  if (!is.finite(intercept)) intercept <- 0
  if (!is.finite(slope)) slope <- 1
  # Conservative guards keep one noisy historical period from creating extreme corrections.
  intercept <- pmin(3, pmax(-3, intercept))
  slope <- pmin(1.5, pmax(0.5, slope))
  calibrated <- pmax(0, intercept + slope * predicted)
  residual_sd <- stats::sd(actual - calibrated)
  if (!is.finite(residual_sd)) residual_sd <- 0
  c(intercept = intercept, slope = slope, residual_sd = residual_sd, n = length(actual))
}

apply_linear_calibration <- function(predicted, intercept = 0, slope = 1) {
  pmax(0, intercept + slope * predicted)
}

get_position_calibration <- function(position, path = "output/selected_calibration.csv") {
  default <- list(intercept = 0, slope = 1, residual_sd = 0)
  if (!file.exists(path)) return(default)
  x <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || !all(c("position", "intercept", "slope", "residual_sd") %in% names(x))) return(default)
  row <- x[x$position == position, , drop = FALSE]
  if (nrow(row) == 0) return(default)
  list(
    intercept = as.numeric(row$intercept[1]),
    slope = as.numeric(row$slope[1]),
    residual_sd = as.numeric(row$residual_sd[1])
  )
}


fit_probability_calibration <- function(probability, actual, min_rows = 30) {
  keep <- is.finite(probability) & is.finite(actual)
  p <- pmin(0.99, pmax(0.01, probability[keep]))
  y <- as.integer(actual[keep])
  if (length(y) < min_rows || sum(y == 1) < 5 || sum(y == 0) < 5) {
    return(c(intercept = 0, slope = 1, n = length(y)))
  }
  x <- stats::qlogis(p)
  fit <- suppressWarnings(tryCatch(
    stats::glm(y ~ x, family = stats::binomial()),
    error = function(e) NULL
  ))
  if (is.null(fit)) return(c(intercept = 0, slope = 1, n = length(y)))
  co <- stats::coef(fit)
  intercept <- ifelse(is.finite(co[1]), as.numeric(co[1]), 0)
  slope <- ifelse(is.finite(co[2]), as.numeric(co[2]), 1)
  intercept <- pmin(3, pmax(-3, intercept))
  slope <- pmin(3, pmax(0.25, slope))
  c(intercept = intercept, slope = slope, n = length(y))
}

apply_probability_calibration <- function(probability, intercept = 0, slope = 1) {
  p <- pmin(0.99, pmax(0.01, probability))
  stats::plogis(intercept + slope * stats::qlogis(p))
}

get_probability_calibration <- function(position, event,
                                        path = "output/selected_probability_calibration.csv") {
  default <- list(intercept = 0, slope = 1)
  if (!file.exists(path)) return(default)
  x <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || !all(c("position", "event", "intercept", "slope") %in% names(x))) return(default)
  row <- x[x$position == position & x$event == event, , drop = FALSE]
  if (nrow(row) == 0) return(default)
  list(intercept = as.numeric(row$intercept[1]), slope = as.numeric(row$slope[1]))
}

binary_auc <- function(actual, probability) {
  actual <- as.integer(actual)
  keep <- is.finite(actual) & is.finite(probability)
  actual <- actual[keep]
  probability <- probability[keep]
  n_pos <- sum(actual == 1)
  n_neg <- sum(actual == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  r <- rank(probability, ties.method = "average")
  (sum(r[actual == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}



# ------------------------------------------------------------
# FANTASY MODEL 2.0 ARCHITECTURE
# Opportunity is modeled directly. Efficiency is empirical-Bayes
# shrunk toward position baselines. TD rates are strongly shrunk
# and adjusted by red-zone/team context. A capped residual model
# learns what the structured stat projection still misses.
# ------------------------------------------------------------
OPPORTUNITY_FINAL_TREES <- 14
OPPORTUNITY_VALIDATION_TREES <- 7
TEAM_VOLUME_FINAL_TREES_20 <- 16
TEAM_VOLUME_VALIDATION_TREES_20 <- 8
RESIDUAL_FINAL_TREES <- 10
RESIDUAL_VALIDATION_TREES <- 5
RESIDUAL_MIN_ROWS <- 40
RESIDUAL_CLIP_FPPG <- 4.0
ARCHITECTURE_BLEND_CANDIDATES <- c(0, 0.25, 0.50, 0.75, 1.00)

# Shrinkage grids. These are tuned independently inside each
# walk-forward training window, so the held-out season never
# influences the selected hyperparameters.
SHRINK_K_CANDIDATES <- c(5, 10, 20, 40, 80, 160, 320)
SHRINK_RECENCY_WEIGHTS <- c(0.50, 0.65, 0.80)
TD_CONTEXT_STRENGTH_CANDIDATES <- c(0, 0.25, 0.50, 0.75)

# Minimum exposure for a target efficiency statistic to be used
# in hyperparameter selection / diagnostics.
MIN_TARGETS_EFFICIENCY <- 5
MIN_CARRIES_EFFICIENCY <- 3
MIN_ATTEMPTS_EFFICIENCY <- 30

# Conservative bounds prevent one noisy rate from exploding a
# multiplicative stat-line projection.
RATE_BOUNDS_20 <- list(
  QB = list(pass_ypa = c(4.0, 10.5), int_rate = c(0.005, 0.07), rush_ypc = c(1.0, 8.5), pass_td_rate = c(0.015, 0.09), rush_td_rate = c(0, 0.20)),
  RB = list(catch_rate = c(0.35, 0.95), ypt = c(3.0, 11.5), rush_ypc = c(2.5, 6.5), rec_td_rate = c(0.005, 0.12), rush_td_rate = c(0.005, 0.12)),
  WR = list(catch_rate = c(0.35, 0.90), ypt = c(4.0, 13.5), rush_ypc = c(2.5, 10.0), rec_td_rate = c(0.005, 0.12), rush_td_rate = c(0, 0.18)),
  TE = list(catch_rate = c(0.40, 0.90), ypt = c(3.5, 12.0), rec_td_rate = c(0.005, 0.14))
)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("models", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------
# FANTASY MODEL 2.1 - WEEKLY PROJECTION FOUNDATION
# ------------------------------------------------------------
WEEKLY_TRAIN_START <- 2020
WEEKLY_VALIDATION_YEARS <- 4
WEEKLY_FINAL_TREES <- 20
WEEKLY_VALIDATION_TREES <- 8
WEEKLY_MIN_TRAIN_ROWS <- 80
WEEKLY_BLEND_CANDIDATES <- c(0, 0.25, 0.50, 0.75, 1.00)
WEEKLY_DEFENSE_LOOKBACK_SHORT <- 4
WEEKLY_DEFENSE_LOOKBACK_LONG <- 8
WEEKLY_PLAYER_LOOKBACK_SHORT <- 3
WEEKLY_PLAYER_LOOKBACK_LONG <- 5
WEEKLY_PROJECTION_INTERVAL_Z <- 1.281552
WEEKLY_ACTIVE_ROSTER_STATUSES <- PROJECTABLE_ROSTER_STATUSES

# 2.1 intentionally keeps betting-market inputs limited to values present in
# the schedule file before kickoff. Game totals are used; no post-game score or
# result field is ever a model feature.
WEEKLY_USE_GAME_TOTAL <- TRUE
WEEKLY_USE_INJURIES <- TRUE
WEEKLY_USE_PBP_DEFENSE <- TRUE


# ------------------------------------------------------------
# FANTASY MODEL 2.2 - WEEKLY ACCURACY ENGINE
# ------------------------------------------------------------
# 2.2 does not replace the validated 2.0 season engine. It uses the 2.0
# projection as a long-horizon prior, then decomposes weekly forecasting into
# neutral role, opportunity/stat-line, matchup delta, and a direct weekly model.
# Final weights are selected with strict chronological guardrails.
WEEKLY_22_VALIDATION_YEARS <- 4
WEEKLY_22_FINAL_TREES <- 28
WEEKLY_22_VALIDATION_TREES <- 12
WEEKLY_22_OPPORTUNITY_FINAL_TREES <- 24
WEEKLY_22_OPPORTUNITY_VALIDATION_TREES <- 10
WEEKLY_22_MATCHUP_FINAL_TREES <- 20
WEEKLY_22_MATCHUP_VALIDATION_TREES <- 8
WEEKLY_22_MIN_TRAIN_ROWS <- 100
WEEKLY_22_MATCHUP_MIN_ROWS <- 120
WEEKLY_22_INNER_MIN_ROWS <- 80

# Honest stack candidates. A 0.25 grid across four architectures gives 35
# constrained combinations and is small enough for Posit Cloud.
WEEKLY_22_STACK_STEP <- 0.25
WEEKLY_22_MATCHUP_CAP <- c(QB = 5.0, RB = 4.0, WR = 4.0, TE = 3.5)
WEEKLY_22_SIM_DRAWS <- 2000
WEEKLY_22_BOOM_THRESHOLD <- c(QB = 20, RB = 15, WR = 15, TE = 10)
WEEKLY_22_BUST_THRESHOLD <- c(QB = 10, RB = 7, WR = 7, TE = 5)
WEEKLY_22_STARTER_CUTOFF <- c(QB = 12, RB = 24, WR = 36, TE = 12)
WEEKLY_22_RELEVANT_CUTOFF <- c(QB = 24, RB = 60, WR = 80, TE = 36)
WEEKLY_22_TOP_PROB_CUTOFF <- c(QB = 12, RB = 24, WR = 24, TE = 12)

# Empirical-Bayes style current-season rate shrinkage. Exposure controls how
# quickly the live season can override the preseason efficiency prior.
WEEKLY_22_RATE_K <- list(
  pass_attempts = 80, targets = 24, carries = 35,
  qb_rush_carries = 18, rec_targets = 24, rush_carries = 35
)

# Weekly Next Gen Stats are a live optional source. The model remains runnable
# when NGS is missing for a player because NGS has minimum-attempt thresholds.
WEEKLY_22_USE_NGS <- TRUE
WEEKLY_22_OPTIONAL_ML_ENGINES <- c("ranger", "xgboost")
WEEKLY_22_AUTO_INSTALL_OPTIONAL_ENGINES <- FALSE

# Projection-stat bounds keep component models from generating impossible
# weekly stat lines when a tree extrapolates into a sparse role state.
WEEKLY_22_COMPONENT_BOUNDS <- list(
  QB = list(pass_attempts = c(0, 65), carries = c(0, 25)),
  RB = list(carries = c(0, 40), targets = c(0, 22)),
  WR = list(targets = c(0, 25), carries = c(0, 10)),
  TE = list(targets = c(0, 22))
)
WEEKLY_22_INNER_SEASONS <- 3


# ------------------------------------------------------------
# FANTASY MODEL 2.3 - HISTORICAL OOF META-CALIBRATION
# ------------------------------------------------------------
# Every 2.3 tuning decision is learned from prior OUT-OF-SAMPLE predictions.
# The current holdout season can never tune its own calibration/stack.
WEEKLY_23_VALIDATION_YEARS <- 4
WEEKLY_23_META_STACK_STEP <- 0.25
WEEKLY_23_VERSION_STACK_STEP <- 0.25
WEEKLY_23_META_MIN_ROWS <- 80
WEEKLY_23_VERSION_MIN_ROWS <- 80
WEEKLY_23_PHASE_MIN_ROWS <- 60
WEEKLY_23_STARTER_OBJECTIVE_WEIGHT <- 0.15

# Matchup calibration learns separate favorable/difficult slopes. 2.2 showed
# correct matchup direction but materially excessive magnitude.
WEEKLY_23_MATCHUP_CAL_MIN_ROWS <- 60
WEEKLY_23_MATCHUP_SIDE_MIN_ROWS <- 30
WEEKLY_23_MATCHUP_SIDE_THRESHOLD <- 0.50
WEEKLY_23_MATCHUP_MAX_SLOPE <- 1.00

# The residual layer is intentionally small, ridge-regularized, capped and only
# enabled when a prior-season inner holdout shows an actual MAE improvement.
WEEKLY_23_RIDGE_LAMBDAS <- c(5, 10, 25, 50, 100, 250)
WEEKLY_23_RESIDUAL_MIN_ROWS <- 180
WEEKLY_23_RESIDUAL_INNER_MIN_ROWS <- 100
WEEKLY_23_RESIDUAL_MIN_GAIN <- 0.01
WEEKLY_23_RESIDUAL_CAP <- c(QB = 2.5, RB = 2.0, WR = 2.0, TE = 1.5)
WEEKLY_23_RESIDUAL_STARTER_WEIGHT <- 0.35
WEEKLY_23_RESIDUAL_RELEVANT_WEIGHT <- 0.15

# Projection snapshots support in-season audit/history in the app.
WEEKLY_23_SAVE_PROJECTION_HISTORY <- TRUE


# ------------------------------------------------------------
# FANTASY MODEL 2.3.2 - SIGNAL DISCOVERY + ERROR FEEDBACK
# ------------------------------------------------------------
# A feature must explain remaining OOF error consistently before it can enter
# the contextual correction model. Highly redundant signals are pruned.
WEEKLY_232_BASE_MIN_ROWS <- 120
WEEKLY_232_SIGNAL_MIN_ROWS <- 240
WEEKLY_232_SIGNAL_INNER_MIN_ROWS <- 140
WEEKLY_232_SIGNAL_TOP_N <- 14
WEEKLY_232_SIGNAL_MIN_SCORE <- 0.10
WEEKLY_232_REDUNDANCY_COR <- 0.92
WEEKLY_232_SIGNAL_RIDGE_LAMBDAS <- c(10, 25, 50, 100, 250, 500)
WEEKLY_232_SIGNAL_MIN_UTILITY <- 0.001
WEEKLY_232_SIGNAL_CAP <- c(QB = 2.5, RB = 2.0, WR = 2.0, TE = 1.5)
WEEKLY_232_SIGNAL_STARTER_WEIGHT <- 0.40
WEEKLY_232_SIGNAL_RELEVANT_WEIGHT <- 0.15

# PID-like controller. Gains are learned from a prior-season inner holdout and
# may choose zero. The integral term is decayed/capped to prevent windup and
# chasing touchdown noise.
WEEKLY_232_PID_MIN_ROWS <- 300
WEEKLY_232_PID_INNER_MIN_ROWS <- 180
WEEKLY_232_PID_MIN_UTILITY <- 0.001
WEEKLY_232_PID_KP <- c(0, 0.05, 0.10, 0.15, 0.20, 0.30)
WEEKLY_232_PID_KI <- c(0, 0.02, 0.05, 0.08, 0.12)
WEEKLY_232_PID_KD <- c(0, 0.03, 0.06, 0.10)
WEEKLY_232_PID_DECAY <- 0.70
WEEKLY_232_PID_INTEGRAL_CAP <- c(QB = 8, RB = 6, WR = 6, TE = 5)
WEEKLY_232_PID_DERIVATIVE_CAP <- c(QB = 10, RB = 8, WR = 8, TE = 6)
WEEKLY_232_TOTAL_CORRECTION_CAP <- c(QB = 3.0, RB = 2.25, WR = 2.25, TE = 1.75)

# 2.3.2 is a challenger until its chronological OOF metrics beat 2.3 without
# materially worsening RMSE/correlation.
WEEKLY_232_PROMOTION_MIN_MAE_GAIN <- 0.001


# ------------------------------------------------------------
# FANTASY MODEL 2.4 - SIGNAL-FIRST + PLAYER TALENT ARCHITECTURE
# ------------------------------------------------------------
# 2.4 intentionally freezes the prior weekly engines as benchmarks and asks a
# more fundamental question: which PRE-KICKOFF variables add stable, unique
# predictive value, and how does the effect vary by player/archetype?
MODEL24_COLLEGE_START <- 2013
MODEL24_NFL_SIGNAL_START <- 2016
MODEL24_AUTO_INSTALL_CFBFASTR <- TRUE
MODEL24_USE_COLLEGE <- TRUE
MODEL24_USE_COMBINE <- TRUE
MODEL24_USE_FFOPPORTUNITY <- TRUE
MODEL24_USE_PARTICIPATION <- TRUE
MODEL24_USE_FTN_CHARTING <- TRUE
MODEL24_COLLEGE_CHECKPOINT_DIR <- "data/raw/college_2_4_checkpoints"
MODEL24_DEFENSE_CHECKPOINT_DIR <- "data/raw/defense_style_2_4_checkpoints"

# Prospect / player-talent variables. Missingness flags are retained so a model
# never confuses "not measured" with a literal zero athletic result.
MODEL24_TALENT_FEATURES <- c(
  "age_at_draft_24", "early_declare_heuristic_24",
  "combine_height_in_24", "combine_weight_24", "combine_forty_24",
  "combine_vertical_24", "combine_broad_24", "combine_cone_24", "combine_shuttle_24",
  "combine_speed_score_24", "combine_bmi_24", "combine_athleticism_z_24",
  "draft_round_24", "draft_overall_24", "draft_capital_log_24",
  "college_seasons_24", "college_games_24", "college_starts_24",
  "college_career_pass_att_24", "college_career_pass_yd_24", "college_career_pass_td_24", "college_career_int_24",
  "college_career_rush_att_24", "college_career_rush_yd_24", "college_career_rush_td_24",
  "college_career_rec_24", "college_career_rec_yd_24", "college_career_rec_td_24",
  "college_final_pass_att_24", "college_final_pass_yd_24", "college_final_pass_td_24", "college_final_int_24",
  "college_final_rush_att_24", "college_final_rush_yd_24", "college_final_rush_td_24",
  "college_final_rec_24", "college_final_rec_yd_24", "college_final_rec_td_24",
  "college_final_pass_share_24", "college_final_rush_share_24", "college_final_rec_yd_share_24",
  "college_peak_pass_share_24", "college_peak_rush_share_24", "college_peak_rec_yd_share_24",
  "college_final_ypa_24", "college_final_ypc_24", "college_final_ypr_24",
  "college_breakout_age_24", "college_production_score_z_24",
  "combine_missing_24", "college_missing_24"
)

MODEL24_TALENT_RIDGE_LAMBDAS <- c(10, 25, 50, 100, 250, 500)
MODEL24_TALENT_MIN_TRAIN <- 28
MODEL24_TALENT_HALF_LIFE_GAMES <- c(QB = 12, RB = 10, WR = 14, TE = 18)
MODEL24_TALENT_MAX_WEEKLY_EFFECT <- c(QB = 3.0, RB = 2.5, WR = 2.5, TE = 2.0)

# Defense-style variables are intentionally interpretable. Participation data
# supplies pass-rusher/pressure/coverage profiles; FTN charting supplies a
# direct blitz measure from 2022 onward. All game-week values are lagged before
# they can reach a forecast.
MODEL24_DEFENSE_STYLE_FEATURES <- c(
  "opp_blitz_rate_roll4_24", "opp_rushers5_rate_roll4_24", "opp_avg_pass_rushers_roll4_24",
  "opp_pressure_rate_roll4_24", "opp_man_rate_roll4_24", "opp_zone_rate_roll4_24",
  "opp_cover0_rate_roll4_24", "opp_cover1_rate_roll4_24", "opp_cover2_rate_roll4_24",
  "opp_cover3_rate_roll4_24", "opp_cover4_rate_roll4_24", "opp_cover6_rate_roll4_24",
  "opp_avg_box_roll4_24"
)
MODEL24_RESPONSE_FEATURES <- MODEL24_DEFENSE_STYLE_FEATURES
MODEL24_RESPONSE_PLAYER_K <- 18
MODEL24_RESPONSE_ARCHETYPE_K <- 55
MODEL24_RESPONSE_MIN_PLAYER_GAMES <- 4
MODEL24_RESPONSE_MIN_ARCHETYPE_ROWS <- 60
MODEL24_RESPONSE_CAP <- c(QB = 2.5, RB = 1.75, WR = 2.0, TE = 1.75)

# Signal qualification / transparent conditional weighting.
MODEL24_SIGNAL_TOP_N <- 24
MODEL24_SIGNAL_MIN_ROWS <- 160
MODEL24_SIGNAL_INNER_MIN_ROWS <- 100
MODEL24_SIGNAL_MIN_ABS_RESID_COR <- 0.015
MODEL24_SIGNAL_MIN_DIRECTION_STABILITY <- 0.50
MODEL24_SIGNAL_REDUNDANCY_COR <- 0.94
MODEL24_SIGNAL_RIDGE_LAMBDAS <- c(5, 10, 25, 50, 100, 250, 500)
MODEL24_SIGNAL_STARTER_WEIGHT <- 0.50
MODEL24_SIGNAL_RELEVANT_WEIGHT <- 0.20
MODEL24_SIGNAL_CORRECTION_CAP <- c(QB = 4.0, RB = 3.0, WR = 3.0, TE = 2.5)
MODEL24_PERMUTATION_REPEATS <- 3

# Promotion is intentionally stricter than 2.3.2. 2.4 must reduce MAE and
# RMSE, preserve/improve starter MAE, and not sacrifice linear/rank correlation.
MODEL24_PROMOTION_MIN_MAE_GAIN <- 0.0005
MODEL24_PROMOTION_MAX_CORR_LOSS <- 0.001
MODEL24_PROMOTION_MAX_RANK_LOSS <- 0.002
MODEL24_PROMOTION_MAX_STARTER_MAE_RATIO <- 1.000

# ------------------------------------------------------------
# 3.0 DYNASTY INTELLIGENCE + ROLE DATA LAB
# ------------------------------------------------------------
APP_VERSION <- "3.0.9"
PROJECTION_ENGINE_VERSION <- "2.4.3"
DYNASTY_ENGINE_VERSION <- "1.1-model-first"
ROLE_ENGINE_VERSION <- "3.0-shadow"

SLEEPER_API_BASE <- "https://api.sleeper.app/v1"
SLEEPER_CACHE_DIR <- "data/league_cache"
SLEEPER_PLAYER_CACHE_HOURS <- 24
SLEEPER_COMPACT_PLAYERS_PATH <- "data/processed/sleeper_players_compact_3_0.csv"
FM3_APP_WEEKLY_SNAPSHOT_PATH <- "data/processed/app_current_week_snapshot_3_0.csv"
DYNASTY_SETTINGS_PATH <- "settings/dynasty_config.csv"
DYNASTY_VALUE_MAX <- 10000
DYNASTY_FUTURE_PICK_DISCOUNT <- 0.88
DYNASTY_ROOKIE_DRAFT_MAX_ROUNDS <- 6

# The AI layer is optional. The quantitative engines work without an API key.
OPENAI_MODEL_DEFAULT <- "gpt-5.6-luna"
OPENAI_API_URL <- "https://api.openai.com/v1/responses"

ROLE30_MIN_TRAIN_ROWS <- 120
ROLE30_MIN_POSITION_ROWS <- 80
ROLE30_PROMOTION_MIN_MAE_GAIN <- 0.005
ROLE30_PROMOTION_MAX_RMSE_RATIO <- 1.01
ROLE30_ENSEMBLE_TREES <- 12
ROLE30_VALIDATION_TREES <- 8
ROLE30_VALIDATION_START <- 2022
ROLE30_VALIDATION_END <- 2025
ROLE30_OUTPUT_TABLE <- "data/processed/weekly_role_table_3_0.csv"
ROLE30_LIVE_OUTPUT <- "output/weekly_role_forecasts_3_0.csv"

ROLE30_TARGETS <- list(
  QB = c("pass_attempts", "pass_attempt_share_week", "offense_pct"),
  RB = c("carries", "targets", "carry_share_week", "target_share_week", "offense_pct"),
  WR = c("targets", "target_share_week", "offense_pct"),
  TE = c("targets", "target_share_week", "offense_pct")
)
