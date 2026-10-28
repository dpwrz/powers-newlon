# ============================================================
# FANTASY MODEL 3.2 - SIGNAL OPTIMIZATION SAFETY TESTS
# ============================================================
source("config.R")
source("R/weekly_engine_31.R")
source("R/weekly_engine_32.R")

d <- data.frame(
  position = rep("WR", 120),
  season = rep(2021:2025, length.out = 120),
  weekly_fppg = seq(1, 24, length.out = 120),
  snap_trend = rep(c(-0.15, 0, 0.15), length.out = 120),
  target_trend = rep(c(-2, 0, 2), length.out = 120),
  carry_trend = 0,
  role_fppg_trend = rep(c(-3, 0, 3), length.out = 120),
  roll3_pass_attempts = 34,
  roll5_pass_attempts = 32,
  roll3_target_share = seq(.10, .35, length.out = 120),
  roll3_carry_share = 0,
  roll3_pass_attempt_share = 0,
  roll3_offense_pct = seq(.55, .95, length.out = 120),
  roll3_offense_snaps = seq(35, 65, length.out = 120),
  preseason_prior_fppg = seq(7, 17, length.out = 120),
  roll3_fppg = seq(6, 19, length.out = 120),
  roll5_fppg = seq(6.2, 18.5, length.out = 120),
  season_to_date_fppg = seq(6.5, 18, length.out = 120),
  roll3_fppg_sd = 3,
  roll3_targets = seq(3, 10, length.out = 120),
  roll5_targets = seq(3.2, 9.7, length.out = 120),
  roll3_carries = 0,
  roll5_carries = 0,
  implied_team_total = 24,
  expected_team_plays = 65,
  expected_team_pass_rate = .58,
  team_skill_out_count = rep(0:2, length.out = 120),
  team_skill_questionable_count = rep(0:3, length.out = 120),
  injury_risk = 0,
  practice_risk = 0,
  season_week = rep(1:18, length.out = 120),
  favorite_points = 0,
  underdog_points = 3,
  def_redzone_pass_td_rate_roll4 = .12,
  def_redzone_rush_td_rate_roll4 = .08,
  def_sack_rate_roll4 = .07,
  def_qb_hit_rate_roll4 = .10,
  roll3_ngs_time_to_throw = 2.8,
  roll3_ypt = 8,
  roll3_catch_rate = .66,
  wr_volume_matchup = 0,
  wr_deep_matchup = 0,
  wr_redzone_matchup = 0,
  stringsAsFactors = FALSE
)

x <- wk32_add_factors(d)
stopifnot(all(MODEL32_FACTOR_FEATURES %in% names(x)))
stopifnot(abs(x$snap_change_scaled_32[3] - 1) < 1e-9)
stopifnot(all(is.finite(as.matrix(x[, MODEL32_FACTOR_FEATURES]))))
stopifnot(all(x$role_stability_32 > 0 & x$role_stability_32 <= 1))

# Noise selector must reject a constant feature while retaining anchors.
d$constant_noise <- 1
audit <- wk32_feature_audit(d, "WR", c("constant_noise","roll3_fppg","roll3_target_share"))
const <- audit[audit$feature == "constant_noise", , drop = FALSE]
stopifnot(nrow(const) == 1L)
stopifnot(!is.finite(const$sd[1]) || const$sd[1] < 1e-8)

compact <- wk32_select_compact_features(
  d, "WR",
  features = unique(c(MODEL31_SAFE_FEATURES, "constant_noise")),
  min_features = 5L, max_features = 20L
)
stopifnot(!"constant_noise" %in% compact)
stopifnot("roll3_fppg" %in% compact)
stopifnot("roll3_target_share" %in% compact)

cat("Model 3.2 signal optimization safety checks passed.\n")
