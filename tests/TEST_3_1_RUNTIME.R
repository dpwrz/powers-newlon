# Lightweight structural/runtime checks for Model 3.1.
source("config.R")
source("R/live_refresh_engine_31.R")
source("R/weekly_engine_31.R")

dummy <- data.frame(
  position = c("WR","RB"),
  games_played_prior = c(2,5),
  preseason_prior_fppg = c(10,9),
  roll3_fppg = c(14,11),
  roll5_fppg = c(12,10),
  role_fppg_trend = c(2,0),
  snap_trend = c(15,2),
  target_trend = c(3,0),
  carry_trend = c(0,2),
  stringsAsFactors = FALSE
)
z <- wk31_add_features(dummy)
stopifnot(all(MODEL31_DERIVED_FEATURES %in% names(z)))
stopifnot(all(is.finite(z$adaptive_prior_weight_31)))
stopifnot(all(z$adaptive_prior_weight_31 >= 0.10 & z$adaptive_prior_weight_31 <= 0.95))

cur <- list(
  stats = "a", schedule = "b", future_context = "c", public = "d",
  completed_games = 2L, latest_actual_week = 2L, stats_rows = 10L, schedule_rows = 20L
)
prev <- list(
  generated_at = "2026-09-17 UTC",
  stats_fingerprint = "old",
  future_context_fingerprint = "c",
  public_fingerprint = "old",
  completed_games = 2L
)
chg <- live31_classify_change(cur, prev, force = FALSE)
stopifnot(identical(chg$reason, "in_progress_stats_only"))
stopifnot(!isTRUE(chg$rebuild))

prev$completed_games <- 1L
chg2 <- live31_classify_change(cur, prev, force = FALSE)
stopifnot(identical(chg2$reason, "completed_game"))
stopifnot(isTRUE(chg2$rebuild))

cat("Model 3.1 runtime checks passed.\n")
