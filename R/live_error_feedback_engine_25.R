# ============================================================
# FANTASY MODEL 2.5 - FINAL-FORECAST ERROR FEEDBACK
# ============================================================
# Uses ONLY completed player-weeks from the frozen final 2.5 pregame archive.
# Positive error = actual > projection (model under-projected).
# The live layer never looks at the target/current player-week outcome.

FEEDBACK25_DECAY <- 0.65
FEEDBACK25_MAX_PLAYER_ERRORS <- 5L
FEEDBACK25_POSITION_WEEKS <- 4L
FEEDBACK25_HORIZON_DECAY <- 0.78
FEEDBACK25_CAP <- c(QB = 2.00, RB = 1.50, WR = 1.50, TE = 1.25)
FEEDBACK25_GAIN_GRID <- c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
FEEDBACK25_POSITION_GAIN_GRID <- c(0, 0.05, 0.10, 0.15)

fb25_num <- function(x, default = NA_real_) {
  z <- suppressWarnings(as.numeric(x))
  z[!is.finite(z)] <- default
  z
}
fb25_chr <- function(x) {
  z <- as.character(x)
  z[is.na(z)] <- ""
  z
}
fb25_bool <- function(x) {
  if (is.logical(x)) return(replace(x, is.na(x), FALSE))
  toupper(trimws(as.character(x))) %in% c("TRUE", "T", "1", "YES", "Y")
}

fb25_weighted_mean <- function(x, decay = FEEDBACK25_DECAY) {
  x <- fb25_num(x, NA_real_); x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  # x is oldest -> newest; newest gets weight 1.
  n <- length(x)
  w <- decay ^ rev(seq_len(n) - 1L)
  sum(x * w) / sum(w)
}

fb25_player_state_before <- function(errors, player_id, season, week) {
  if (!nrow(errors)) return(list(n = 0L, last = NA_real_, ewma = NA_real_, roll3 = NA_real_, abs3 = NA_real_, trend = NA_real_))
  d <- errors[
    fb25_chr(errors$player_id) == fb25_chr(player_id) &
      (as.integer(errors$season) < as.integer(season) |
         (as.integer(errors$season) == as.integer(season) & as.integer(errors$week) < as.integer(week))),
    , drop = FALSE
  ]
  if (!nrow(d)) return(list(n = 0L, last = NA_real_, ewma = NA_real_, roll3 = NA_real_, abs3 = NA_real_, trend = NA_real_))
  d <- d[order(as.integer(d$season), as.integer(d$week)), , drop = FALSE]
  e <- fb25_num(d$error, NA_real_); e <- e[is.finite(e)]
  if (!length(e)) return(list(n = 0L, last = NA_real_, ewma = NA_real_, roll3 = NA_real_, abs3 = NA_real_, trend = NA_real_))
  tail_e <- tail(e, FEEDBACK25_MAX_PLAYER_ERRORS)
  r3 <- tail(e, 3)
  list(
    n = length(e),
    last = tail(e, 1),
    ewma = fb25_weighted_mean(tail_e),
    roll3 = mean(r3),
    abs3 = mean(abs(r3)),
    trend = if (length(e) >= 2) tail(e, 1) - tail(e, 2)[1] else NA_real_
  )
}

fb25_position_state_before <- function(errors, position, season, week) {
  if (!nrow(errors)) return(list(n = 0L, bias = NA_real_, abs_error = NA_real_))
  d <- errors[
    fb25_chr(errors$position) == fb25_chr(position) &
      (as.integer(errors$season) < as.integer(season) |
         (as.integer(errors$season) == as.integer(season) & as.integer(errors$week) < as.integer(week))),
    , drop = FALSE
  ]
  if (!nrow(d)) return(list(n = 0L, bias = NA_real_, abs_error = NA_real_))
  d <- d[order(as.integer(d$season), as.integer(d$week)), , drop = FALSE]
  # Only recent completed weeks matter for live calibration.
  key <- paste(as.integer(d$season), as.integer(d$week), sep = "-")
  keys <- tail(unique(key), FEEDBACK25_POSITION_WEEKS)
  d <- d[key %in% keys, , drop = FALSE]
  e <- fb25_num(d$error, NA_real_); e <- e[is.finite(e)]
  if (!length(e)) return(list(n = 0L, bias = NA_real_, abs_error = NA_real_))
  # Winsorize position bias so one explosive outlier cannot move a whole position.
  if (length(e) >= 10) {
    q <- stats::quantile(e, c(0.05, 0.95), na.rm = TRUE, names = FALSE)
    e <- pmax(q[1], pmin(q[2], e))
  }
  list(n = length(e), bias = mean(e), abs_error = mean(abs(e)))
}

fb25_build_features <- function(target, errors) {
  if (!nrow(target)) return(target)
  out <- target
  n <- nrow(out)
  vals <- lapply(seq_len(n), function(i) {
    ps <- fb25_player_state_before(errors, out$player_id[i], out$season[i], out$week[i])
    zs <- fb25_position_state_before(errors, out$position[i], out$season[i], out$week[i])
    c(
      player_error_n = ps$n,
      player_last_error = ps$last,
      player_error_ewma = ps$ewma,
      player_error_roll3 = ps$roll3,
      player_abs_error_roll3 = ps$abs3,
      player_error_trend = ps$trend,
      position_error_n = zs$n,
      position_error_bias = zs$bias,
      position_abs_error = zs$abs_error
    )
  })
  mat <- do.call(rbind, vals)
  for (nm in colnames(mat)) out[[nm]] <- as.numeric(mat[, nm])
  out
}

fb25_shrunk_player_bias <- function(n, ewma, position_bias) {
  n <- fb25_num(n, 0); ewma <- fb25_num(ewma, 0); position_bias <- fb25_num(position_bias, 0)
  # Four pseudo-observations pull a player's short error history toward the
  # broader position calibration rather than chasing one miss.
  w <- n / (n + 4)
  w * ewma + (1 - w) * position_bias
}

fb25_apply_correction <- function(d, player_gain, position_gain, current_week = NULL) {
  if (!nrow(d)) return(d)
  pos <- fb25_chr(d$position)
  base <- fb25_num(d$feedback_base_projection_25, 0)
  player_bias <- fb25_shrunk_player_bias(d$player_error_n, d$player_error_ewma, d$position_error_bias)
  position_bias <- fb25_num(d$position_error_bias, 0)
  raw <- player_gain * player_bias + position_gain * position_bias
  cap <- unname(FEEDBACK25_CAP[pos]); cap[!is.finite(cap)] <- 1.5
  raw <- pmax(-cap, pmin(cap, raw))

  if (is.null(current_week)) {
    horizon <- rep(1, nrow(d))
  } else {
    horizon <- pmax(1, as.integer(fb25_num(d$week, current_week)) - as.integer(current_week) + 1L)
  }
  decay <- FEEDBACK25_HORIZON_DECAY ^ (horizon - 1L)
  corr <- raw * decay

  d$feedback_player_bias_25 <- player_bias
  d$feedback_position_bias_25 <- position_bias
  d$feedback_raw_correction_25 <- raw
  d$feedback_horizon_decay_25 <- decay
  d$feedback_correction_25 <- corr
  d$feedback_post_projection_25 <- pmax(0, base + corr)
  d
}

fb25_decision_score <- function(d, pred_col) {
  if (!nrow(d)) return(Inf)
  base <- d$feedback_base_projection_25
  cand <- d[[pred_col]]
  actual <- d$actual_fppg
  keep <- is.finite(base) & is.finite(cand) & is.finite(actual)
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) return(Inf)
  mae_ratio <- function(mask) {
    if (!any(mask)) return(1)
    b <- mean(abs(d$actual_fppg[mask] - d$feedback_base_projection_25[mask]))
    c <- mean(abs(d$actual_fppg[mask] - d[[pred_col]][mask]))
    if (!is.finite(b) || b <= 0 || !is.finite(c)) 1 else c / b
  }
  rmse_ratio <- function(mask) {
    if (!any(mask)) return(1)
    b <- sqrt(mean((d$actual_fppg[mask] - d$feedback_base_projection_25[mask])^2))
    c <- sqrt(mean((d$actual_fppg[mask] - d[[pred_col]][mask])^2))
    if (!is.finite(b) || b <= 0 || !is.finite(c)) 1 else c / b
  }
  starter <- if ("starter_cohort" %in% names(d)) fb25_bool(d$starter_cohort) else rep(FALSE, nrow(d))
  relevant <- if ("relevant_cohort" %in% names(d)) fb25_bool(d$relevant_cohort) else rep(TRUE, nrow(d))
  all <- rep(TRUE, nrow(d))
  0.45 * mae_ratio(starter) +
    0.30 * mae_ratio(relevant) +
    0.15 * mae_ratio(all) +
    0.05 * rmse_ratio(starter) +
    0.05 * rmse_ratio(relevant)
}

fb25_select_gains <- function(prior) {
  if (!nrow(prior)) return(c(player_gain = 0, position_gain = 0))
  best <- c(player_gain = 0, position_gain = 0)
  best_score <- Inf
  for (gp in FEEDBACK25_GAIN_GRID) {
    for (gz in FEEDBACK25_POSITION_GAIN_GRID) {
      z <- fb25_apply_correction(prior, gp, gz, current_week = NULL)
      s <- fb25_decision_score(z, "feedback_post_projection_25")
      # Prefer smaller corrections when scores are essentially tied.
      penalty <- 1e-4 * (gp + gz)
      if (is.finite(s) && s + penalty < best_score) {
        best_score <- s + penalty
        best <- c(player_gain = gp, position_gain = gz)
      }
    }
  }
  best
}
