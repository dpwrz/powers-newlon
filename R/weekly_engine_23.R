# ============================================================
# FANTASY MODEL 2.3 - HISTORICAL META-CALIBRATION ENGINE
# ============================================================
# Requires config.R, R/weekly_engine.R and R/weekly_engine_22.R first.
# 2.3 learns only from historical OUT-OF-SAMPLE forecasts. It calibrates the
# explicit matchup delta, reconstructs a 2.1 challenger, learns phase-aware
# meta-stack weights, applies a guarded ridge residual correction, and then
# uses a cross-version guardrail before production.

wk23_phase <- function(week) {
  w <- suppressWarnings(as.integer(week))
  dplyr::case_when(
    is.na(w) ~ "ALL",
    w <= 3 ~ "W1-3",
    w <= 7 ~ "W4-7",
    w <= 12 ~ "W8-12",
    TRUE ~ "W13-18"
  )
}

wk23_clip <- function(x, lo, hi) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- 0
  pmin(hi, pmax(lo, x))
}

# ------------------------------------------------------------
# Legacy Model 2.1 challenger
# ------------------------------------------------------------
score_legacy21_blend23 <- function(d, grid = WEEKLY_BLEND_CANDIDATES) {
  if (nrow(d) == 0) return(data.frame())
  rows <- lapply(grid, function(w) {
    p <- w * wk_num(d$legacy21_model_fppg) + (1 - w) * wk_num(d$baseline_weekly_fppg)
    starter <- if ("starter_cohort" %in% names(d)) d$starter_cohort %in% TRUE else rep(FALSE, nrow(d))
    starter_mae <- if (sum(starter, na.rm = TRUE) >= 10) mean(abs(p[starter] - d$actual_fppg[starter]), na.rm = TRUE) else NA_real_
    mae <- mean(abs(p - d$actual_fppg), na.rm = TRUE)
    objective <- if (is.finite(starter_mae)) (1 - WEEKLY_23_STARTER_OBJECTIVE_WEIGHT) * mae + WEEKLY_23_STARTER_OBJECTIVE_WEIGHT * starter_mae else mae
    data.frame(weekly_model_weight = w, prior_weight = 1 - w, MAE = mae, starter_MAE = starter_mae, objective = objective,
               RMSE = sqrt(mean((p - d$actual_fppg)^2, na.rm = TRUE)), correlation = safe_cor21(p, d$actual_fppg), stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows) |> dplyr::arrange(objective, MAE, RMSE, dplyr::desc(correlation))
}

select_legacy21_weight23 <- function(history, phase = "ALL") {
  h <- history
  if (!identical(phase, "ALL") && "week_phase_23" %in% names(h)) {
    hp <- h[h$week_phase_23 == phase, , drop = FALSE]
    if (nrow(hp) >= WEEKLY_23_PHASE_MIN_ROWS) h <- hp
  }
  if (nrow(h) < WEEKLY_23_META_MIN_ROWS) return(data.frame(weekly_model_weight = 0, prior_weight = 1))
  score_legacy21_blend23(h) |> dplyr::slice(1) |> dplyr::select(weekly_model_weight, prior_weight)
}

apply_legacy21_weight23 <- function(d, w) {
  if (nrow(w) == 0) return(wk_num(d$baseline_weekly_fppg))
  ww <- wk_num(w$weekly_model_weight[1]); if (!is.finite(ww)) ww <- 0
  pmax(0, ww * wk_num(d$legacy21_model_fppg) + (1 - ww) * wk_num(d$baseline_weekly_fppg))
}

# ------------------------------------------------------------
# Matchup-delta calibration
# ------------------------------------------------------------
# 2.2 correctly found matchup direction but materially overstated magnitude.
# Learn separate favorable/difficult no-intercept shrinkage slopes from prior
# OOF rows only. Zero matchup stays zero, and slopes are constrained to [0, 1].
fit_matchup_calibration23 <- function(history, pos) {
  d <- history[history$position == pos, , drop = FALSE]
  if (!all(c("matchup_delta_22_raw", "structured_neutral_fppg", "actual_fppg") %in% names(d))) {
    return(data.frame(position = pos, positive_slope = 0, negative_slope = 0, overall_slope = 0,
                      n = 0, n_positive = 0, n_negative = 0))
  }
  x <- wk_num(d$matchup_delta_22_raw)
  y <- wk_num(d$actual_fppg) - wk_num(d$structured_neutral_fppg)
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]; y <- y[keep]
  cap <- as.numeric(WEEKLY_22_MATCHUP_CAP[[pos]]); if (!is.finite(cap)) cap <- 4
  x <- wk23_clip(x, -cap, cap); y <- wk23_clip(y, -2 * cap, 2 * cap)
  slope0 <- function(xx, yy, min_n) {
    k <- is.finite(xx) & is.finite(yy)
    xx <- xx[k]; yy <- yy[k]
    if (length(xx) < min_n || sum(xx^2) < 1e-8) return(NA_real_)
    s <- sum(xx * yy) / sum(xx^2)
    pmin(WEEKLY_23_MATCHUP_MAX_SLOPE, pmax(0, s))
  }
  overall <- slope0(x, y, WEEKLY_23_MATCHUP_CAL_MIN_ROWS)
  if (!is.finite(overall)) overall <- 0
  pos_idx <- x >= WEEKLY_23_MATCHUP_SIDE_THRESHOLD
  neg_idx <- x <= -WEEKLY_23_MATCHUP_SIDE_THRESHOLD
  sp <- slope0(x[pos_idx], y[pos_idx], WEEKLY_23_MATCHUP_SIDE_MIN_ROWS)
  sn <- slope0(x[neg_idx], y[neg_idx], WEEKLY_23_MATCHUP_SIDE_MIN_ROWS)
  if (!is.finite(sp)) sp <- overall
  if (!is.finite(sn)) sn <- overall
  data.frame(position = pos, positive_slope = sp, negative_slope = sn, overall_slope = overall,
             n = length(x), n_positive = sum(pos_idx), n_negative = sum(neg_idx), stringsAsFactors = FALSE)
}

apply_matchup_calibration23 <- function(raw_delta, calibration) {
  x <- wk_num(raw_delta)
  if (nrow(calibration) == 0) return(rep(0, length(x)))
  sp <- wk_num(calibration$positive_slope[1]); sn <- wk_num(calibration$negative_slope[1]); so <- wk_num(calibration$overall_slope[1])
  if (!is.finite(sp)) sp <- 0; if (!is.finite(sn)) sn <- 0; if (!is.finite(so)) so <- 0
  slope <- ifelse(x > 0, sp, ifelse(x < 0, sn, so))
  x * slope
}

# ------------------------------------------------------------
# Phase-aware five-way meta stack
# ------------------------------------------------------------
stack_grid23 <- function(step = WEEKLY_23_META_STACK_STEP) {
  vals <- seq(0, 1, by = step); rows <- list()
  for (a in vals) for (b in vals) for (c in vals) for (d in vals) {
    e <- 1 - a - b - c - d
    if (e < -1e-9 || e > 1 + 1e-9) next
    rows[[length(rows) + 1]] <- data.frame(
      w_legacy21 = a, w_prior = b, w_neutral = c, w_structured_calibrated = d, w_direct = max(0, e),
      stringsAsFactors = FALSE
    )
  }
  unique(dplyr::bind_rows(rows))
}

apply_meta_stack23 <- function(d, w) {
  if (nrow(w) == 0) return(wk_num(d$baseline_weekly_fppg))
  wk_num(w$w_legacy21[1]) * wk_num(d$legacy21_honest_fppg) +
    wk_num(w$w_prior[1]) * wk_num(d$baseline_weekly_fppg) +
    wk_num(w$w_neutral[1]) * wk_num(d$neutral_direct_fppg) +
    wk_num(w$w_structured_calibrated[1]) * wk_num(d$structured_matchup_calibrated_fppg) +
    wk_num(w$w_direct[1]) * wk_num(d$direct_full_fppg)
}

score_meta_stack23 <- function(d, grid = stack_grid23()) {
  if (nrow(d) == 0) return(data.frame())
  starter <- if ("starter_cohort" %in% names(d)) d$starter_cohort %in% TRUE else rep(FALSE, nrow(d))
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, , drop = FALSE]
    p <- apply_meta_stack23(d, g)
    mae <- mean(abs(p - d$actual_fppg), na.rm = TRUE)
    starter_mae <- if (sum(starter, na.rm = TRUE) >= 10) mean(abs(p[starter] - d$actual_fppg[starter]), na.rm = TRUE) else NA_real_
    objective <- if (is.finite(starter_mae)) (1 - WEEKLY_23_STARTER_OBJECTIVE_WEIGHT) * mae + WEEKLY_23_STARTER_OBJECTIVE_WEIGHT * starter_mae else mae
    data.frame(g, MAE = mae, starter_MAE = starter_mae, objective = objective,
               RMSE = sqrt(mean((p - d$actual_fppg)^2, na.rm = TRUE)), correlation = safe_cor21(p, d$actual_fppg),
               rank_correlation = safe_cor21(p, d$actual_fppg, "spearman"), bias = mean(p - d$actual_fppg, na.rm = TRUE))
  })
  dplyr::bind_rows(rows) |> dplyr::arrange(objective, MAE, RMSE, dplyr::desc(correlation))
}

safe_meta_stack23 <- function() data.frame(w_legacy21 = 0, w_prior = 1, w_neutral = 0, w_structured_calibrated = 0, w_direct = 0)

select_meta_weights23 <- function(history, phase = "ALL") {
  h <- history
  if (!identical(phase, "ALL") && "week_phase_23" %in% names(h)) {
    hp <- h[h$week_phase_23 == phase, , drop = FALSE]
    if (nrow(hp) >= WEEKLY_23_PHASE_MIN_ROWS) h <- hp
  }
  if (nrow(h) < WEEKLY_23_META_MIN_ROWS) return(safe_meta_stack23())
  score_meta_stack23(h) |> dplyr::slice(1) |>
    dplyr::select(w_legacy21, w_prior, w_neutral, w_structured_calibrated, w_direct)
}

# ------------------------------------------------------------
# Conservative ridge residual calibration
# ------------------------------------------------------------
W23_RESIDUAL_FEATURES <- c(
  "meta_precal_fppg", "model_disagreement_23", "calibrated_matchup_delta_23",
  "role_fppg_trend", "implied_team_total", "team_spread_line", "games_played_prior",
  "season_week", "preseason_prior_fppg", "roll3_fppg"
)

ridge_prepare23 <- function(d, features, means = NULL, sds = NULL) {
  X <- sapply(features, function(nm) {
    if (!nm %in% names(d)) return(rep(0, nrow(d)))
    x <- wk_num(d[[nm]]); x[!is.finite(x)] <- 0; x
  })
  if (is.null(dim(X))) X <- matrix(X, ncol = length(features))
  colnames(X) <- features
  if (is.null(means)) means <- colMeans(X)
  if (is.null(sds)) {
    sds <- apply(X, 2, stats::sd)
    sds[!is.finite(sds) | sds < 1e-8] <- 1
  }
  Xs <- sweep(sweep(X, 2, means, "-"), 2, sds, "/")
  list(X = cbind(`(Intercept)` = 1, Xs), means = means, sds = sds)
}

fit_ridge23 <- function(d, features, target, lambda, weights = NULL) {
  prep <- ridge_prepare23(d, features)
  X <- prep$X; y <- wk_num(d[[target]])
  keep <- is.finite(y) & apply(is.finite(X), 1, all)
  X <- X[keep, , drop = FALSE]; y <- y[keep]
  if (nrow(X) < 30) return(NULL)
  if (is.null(weights)) weights <- rep(1, nrow(d))
  weights <- wk_num(weights)[keep]; weights[!is.finite(weights) | weights <= 0] <- 1
  sw <- sqrt(weights); Xw <- X * sw; yw <- y * sw
  pen <- diag(ncol(X)); pen[1, 1] <- 0
  A <- crossprod(Xw) + lambda * pen
  b <- crossprod(Xw, yw)
  beta <- tryCatch(as.numeric(solve(A, b)), error = function(e) tryCatch(as.numeric(qr.solve(A, b)), error = function(e2) rep(0, ncol(X))))
  list(features = features, means = prep$means, sds = prep$sds, beta = beta, lambda = lambda)
}

predict_ridge23 <- function(model, d) {
  if (is.null(model)) return(rep(0, nrow(d)))
  prep <- ridge_prepare23(d, model$features, model$means, model$sds)
  p <- as.numeric(prep$X %*% model$beta); p[!is.finite(p)] <- 0; p
}

fit_residual_calibrator23 <- function(history, pos) {
  d <- history[history$position == pos, , drop = FALSE]
  if (nrow(d) < WEEKLY_23_RESIDUAL_MIN_ROWS || length(unique(d$season)) < 2) {
    return(list(enabled = FALSE, position = pos, model = NULL, lambda = NA_real_, validation_gain = 0))
  }
  d$residual_target_23 <- wk_num(d$actual_fppg) - wk_num(d$meta_precal_fppg)
  features <- intersect(W23_RESIDUAL_FEATURES, names(d))
  if (length(features) < 5) return(list(enabled = FALSE, position = pos, model = NULL, lambda = NA_real_, validation_gain = 0))
  years <- sort(unique(as.integer(d$season))); val_year <- max(years)
  tr <- d[d$season < val_year, , drop = FALSE]; va <- d[d$season == val_year, , drop = FALSE]
  if (nrow(tr) < WEEKLY_23_RESIDUAL_INNER_MIN_ROWS || nrow(va) < 30) {
    return(list(enabled = FALSE, position = pos, model = NULL, lambda = NA_real_, validation_gain = 0))
  }
  wtr <- rep(1, nrow(tr))
  if ("starter_cohort" %in% names(tr)) wtr[tr$starter_cohort %in% TRUE] <- 1 + WEEKLY_23_RESIDUAL_STARTER_WEIGHT
  if ("relevant_cohort" %in% names(tr)) wtr[tr$relevant_cohort %in% TRUE] <- pmax(wtr[tr$relevant_cohort %in% TRUE], 1 + WEEKLY_23_RESIDUAL_RELEVANT_WEIGHT)
  cap <- as.numeric(WEEKLY_23_RESIDUAL_CAP[[pos]]); if (!is.finite(cap)) cap <- 2
  scored <- lapply(WEEKLY_23_RIDGE_LAMBDAS, function(lambda) {
    fit <- fit_ridge23(tr, features, "residual_target_23", lambda, wtr)
    corr <- wk23_clip(predict_ridge23(fit, va), -cap, cap)
    base <- wk_num(va$meta_precal_fppg); final <- pmax(0, base + corr)
    data.frame(lambda = lambda, base_MAE = mean(abs(base - va$actual_fppg), na.rm = TRUE),
               corrected_MAE = mean(abs(final - va$actual_fppg), na.rm = TRUE), stringsAsFactors = FALSE)
  }) |> dplyr::bind_rows() |> dplyr::mutate(gain = base_MAE - corrected_MAE) |> dplyr::arrange(corrected_MAE)
  best <- scored[1, , drop = FALSE]
  enabled <- is.finite(best$gain) && best$gain >= WEEKLY_23_RESIDUAL_MIN_GAIN
  if (!enabled) return(list(enabled = FALSE, position = pos, model = NULL, lambda = best$lambda, validation_gain = best$gain, validation = scored))
  wall <- rep(1, nrow(d)); if ("starter_cohort" %in% names(d)) wall[d$starter_cohort %in% TRUE] <- 1 + WEEKLY_23_RESIDUAL_STARTER_WEIGHT
  if ("relevant_cohort" %in% names(d)) wall[d$relevant_cohort %in% TRUE] <- pmax(wall[d$relevant_cohort %in% TRUE], 1 + WEEKLY_23_RESIDUAL_RELEVANT_WEIGHT)
  final_model <- fit_ridge23(d, features, "residual_target_23", best$lambda, wall)
  list(enabled = TRUE, position = pos, model = final_model, lambda = best$lambda, validation_gain = best$gain, validation = scored)
}

predict_residual_calibrator23 <- function(calibrator, d, pos) {
  if (is.null(calibrator) || !isTRUE(calibrator$enabled) || is.null(calibrator$model)) return(rep(0, nrow(d)))
  cap <- as.numeric(WEEKLY_23_RESIDUAL_CAP[[pos]]); if (!is.finite(cap)) cap <- 2
  wk23_clip(predict_ridge23(calibrator$model, d), -cap, cap)
}

# ------------------------------------------------------------
# Cross-version guardrail
# ------------------------------------------------------------
version_grid23 <- function(step = WEEKLY_23_VERSION_STACK_STEP) {
  vals <- seq(0, 1, by = step); rows <- list()
  for (a in vals) for (b in vals) {
    c <- 1 - a - b
    if (c < -1e-9 || c > 1 + 1e-9) next
    rows[[length(rows) + 1]] <- data.frame(w_legacy21_version = a, w_22_version = b, w_23_version = max(0, c), stringsAsFactors = FALSE)
  }
  unique(dplyr::bind_rows(rows))
}

apply_version_guard23 <- function(d, w) {
  if (nrow(w) == 0) return(wk_num(d$candidate23_fppg))
  wk_num(w$w_legacy21_version[1]) * wk_num(d$legacy21_honest_fppg) +
    wk_num(w$w_22_version[1]) * wk_num(d$honest22_fppg) +
    wk_num(w$w_23_version[1]) * wk_num(d$candidate23_fppg)
}

score_version_guard23 <- function(d, grid = version_grid23()) {
  if (nrow(d) == 0) return(data.frame())
  starter <- if ("starter_cohort" %in% names(d)) d$starter_cohort %in% TRUE else rep(FALSE, nrow(d))
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, , drop = FALSE]; p <- apply_version_guard23(d, g)
    mae <- mean(abs(p - d$actual_fppg), na.rm = TRUE)
    starter_mae <- if (sum(starter, na.rm = TRUE) >= 10) mean(abs(p[starter] - d$actual_fppg[starter]), na.rm = TRUE) else NA_real_
    objective <- if (is.finite(starter_mae)) (1 - WEEKLY_23_STARTER_OBJECTIVE_WEIGHT) * mae + WEEKLY_23_STARTER_OBJECTIVE_WEIGHT * starter_mae else mae
    data.frame(g, MAE = mae, starter_MAE = starter_mae, objective = objective,
               RMSE = sqrt(mean((p - d$actual_fppg)^2, na.rm = TRUE)), correlation = safe_cor21(p, d$actual_fppg),
               rank_correlation = safe_cor21(p, d$actual_fppg, "spearman"), bias = mean(p - d$actual_fppg, na.rm = TRUE))
  })
  dplyr::bind_rows(rows) |> dplyr::arrange(objective, MAE, RMSE, dplyr::desc(correlation))
}

safe_version_guard23 <- function() data.frame(w_legacy21_version = 0, w_22_version = 0, w_23_version = 1)

select_version_weights23 <- function(history, phase = "ALL") {
  h <- history
  if (!identical(phase, "ALL") && "week_phase_23" %in% names(h)) {
    hp <- h[h$week_phase_23 == phase, , drop = FALSE]
    if (nrow(hp) >= WEEKLY_23_PHASE_MIN_ROWS) h <- hp
  }
  if (nrow(h) < WEEKLY_23_VERSION_MIN_ROWS) return(safe_version_guard23())
  score_version_guard23(h) |> dplyr::slice(1) |>
    dplyr::select(w_legacy21_version, w_22_version, w_23_version)
}

# ------------------------------------------------------------
# Model disagreement / confidence
# ------------------------------------------------------------
model_disagreement23 <- function(d) {
  cols <- intersect(c("legacy21_honest_fppg", "baseline_weekly_fppg", "neutral_direct_fppg", "structured_matchup_calibrated_fppg", "direct_full_fppg"), names(d))
  if (length(cols) < 2) return(rep(0, nrow(d)))
  m <- as.matrix(data.frame(lapply(d[, cols, drop = FALSE], wk_num), check.names = FALSE))
  apply(m, 1, function(x) { x <- x[is.finite(x)]; if (length(x) < 2) 0 else stats::sd(x) })
}

build_confidence_calibration23 <- function(oof) {
  rows <- list()
  for (pos in unique(oof$position)) {
    d <- oof[oof$position == pos, , drop = FALSE]
    x <- wk_num(d$model_disagreement_23); x[!is.finite(x)] <- 0
    q <- as.numeric(stats::quantile(x, c(.33, .67), na.rm = TRUE, names = FALSE, type = 7))
    if (length(q) < 2 || any(!is.finite(q))) q <- c(0.5, 1.5)
    bucket <- ifelse(x <= q[1], "High", ifelse(x <= q[2], "Medium", "Low"))
    err <- abs(wk_num(d$honest_final_fppg) - wk_num(d$actual_fppg))
    tmp <- data.frame(position = pos, confidence = bucket, abs_error = err)
    sm <- tmp |> dplyr::group_by(position, confidence) |> dplyr::summarise(n = dplyr::n(), expected_abs_error = mean(abs_error, na.rm = TRUE), .groups = "drop")
    sm$q33_disagreement <- q[1]; sm$q67_disagreement <- q[2]
    rows[[length(rows) + 1]] <- sm
  }
  dplyr::bind_rows(rows)
}

assign_confidence23 <- function(disagreement, pos, calibration) {
  cpos <- calibration[calibration$position == pos, , drop = FALSE]
  if (nrow(cpos) == 0) return(data.frame(projection_confidence = rep("Medium", length(disagreement)), expected_abs_error = rep(NA_real_, length(disagreement))))
  q1 <- wk_num(cpos$q33_disagreement[1]); q2 <- wk_num(cpos$q67_disagreement[1])
  x <- wk_num(disagreement); x[!is.finite(x)] <- 0
  lab <- ifelse(x <= q1, "High", ifelse(x <= q2, "Medium", "Low"))
  err_map <- setNames(wk_num(cpos$expected_abs_error), as.character(cpos$confidence))
  data.frame(projection_confidence = lab, expected_abs_error = unname(err_map[lab]), stringsAsFactors = FALSE)
}

cat("[2.3] Historical meta-calibration engine loaded: OOF matchup shrinkage + phase-aware meta stack + guarded residual calibration + cross-version guardrail.\n")
