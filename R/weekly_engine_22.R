# ============================================================
# FANTASY MODEL 2.2 - WEEKLY ACCURACY ENGINE
# ============================================================
# Requires config.R and R/weekly_engine.R to be sourced first.
# 2.2 separates neutral role, expected opportunity/stat line, matchup delta,
# full direct weekly prediction, and the validated season prior. All historical
# validation is chronological and current-week outcomes are never features.

wk22_clip <- function(x, lo, hi) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- lo
  pmin(hi, pmax(lo, x))
}

wk22_safe_mean <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
  if (length(x) == 0) default else mean(x)
}

lag_roll_rate22 <- function(num, den, k, default = NA_real_) {
  num <- wk_num(num); den <- wk_num(den); n <- length(num)
  out <- rep(default, n)
  if (n == 0) return(out)
  for (i in seq_len(n)) {
    hi <- i - 1L
    if (hi < 1L) next
    lo <- max(1L, hi - as.integer(k) + 1L)
    nn <- sum(num[lo:hi], na.rm = TRUE); dd <- sum(den[lo:hi], na.rm = TRUE)
    if (is.finite(dd) && dd > 0) out[i] <- nn / dd
  }
  out
}

lag_cum_sum22 <- function(x) {
  x <- wk_num(x); n <- length(x); out <- rep(0, n)
  if (n <= 1) return(out)
  for (i in 2:n) out[i] <- sum(x[seq_len(i - 1L)], na.rm = TRUE)
  out
}

lag_cum_rate22 <- function(num, den, default = NA_real_) {
  n <- length(num); out <- rep(default, n)
  if (n <= 1) return(out)
  for (i in 2:n) {
    nn <- sum(wk_num(num[seq_len(i - 1L)]), na.rm = TRUE)
    dd <- sum(wk_num(den[seq_len(i - 1L)]), na.rm = TRUE)
    if (is.finite(dd) && dd > 0) out[i] <- nn / dd
  }
  out
}

# -----------------------------
# Feature-family definitions
# -----------------------------
W22_ROLE_COMMON <- c(
  "preseason_prior_fppg", "games_played_prior", "prior_game_fppg",
  "roll3_fppg", "roll5_fppg", "season_to_date_fppg", "roll3_fppg_sd",
  "roll3_offense_pct", "roll5_offense_pct", "roll3_offense_snaps", "snap_trend",
  "opportunity_per_snap", "role_fppg_trend", "target_trend", "carry_trend",
  "roll3_targets", "roll5_targets", "roll3_carries", "roll5_carries",
  "roll3_pass_attempts", "roll5_pass_attempts", "roll3_target_share",
  "roll3_carry_share", "roll3_pass_attempt_share", "roll3_team_pass_attempts",
  "roll3_team_carries", "expected_team_plays", "expected_team_pass_rate",
  "prior_catch_rate", "prior_yards_per_target", "prior_yards_per_carry",
  "prior_yards_per_attempt", "prior_pass_td_rate", "prior_rush_td_rate",
  "prior_rec_td_rate", "prior_interception_rate",
  "season_to_date_catch_rate", "season_to_date_ypt", "season_to_date_rush_ypc",
  "season_to_date_pass_ypa", "season_to_date_pass_td_rate",
  "season_to_date_interception_rate", "season_to_date_rush_td_rate",
  "season_to_date_rec_td_rate", "season_targets_prior", "season_carries_prior",
  "season_pass_attempts_prior"
)

W22_GAME_COMMON <- c(
  "season_week", "is_home", "rest_days", "total_line", "team_spread_line",
  "implied_team_total", "favorite_points", "underdog_points",
  "favorite_rush_interaction", "underdog_target_interaction",
  "injury_risk", "practice_risk", "team_skill_out_count",
  "team_skill_questionable_count"
)

W22_MATCHUP_COMMON <- c(
  "opp_pos_residual_roll4", "opp_pos_residual_roll8", "opp_pos_fppg_allowed_roll4",
  "def_pass_epa_allowed_roll4", "def_pass_epa_allowed_roll8",
  "def_rush_epa_allowed_roll4", "def_rush_epa_allowed_roll8",
  "def_pass_success_allowed_roll4", "def_rush_success_allowed_roll4",
  "def_explosive_pass_rate_roll4", "def_sack_rate_roll4", "def_qb_hit_rate_roll4",
  "def_redzone_pass_td_rate_roll4", "def_redzone_rush_td_rate_roll4",
  "qb_pass_matchup", "qb_pressure_matchup", "rb_rush_matchup", "rb_receiving_matchup",
  "wr_volume_matchup", "wr_deep_matchup", "wr_redzone_matchup",
  "te_middle_matchup", "te_redzone_matchup", "matchup_role_interaction"
)

W22_NGS_COMMON <- c(
  "ngs_available", "roll3_ngs_cpoe", "roll3_ngs_air_yards",
  "roll3_ngs_time_to_throw", "roll3_ngs_aggressiveness",
  "roll3_ngs_separation", "roll3_ngs_cushion", "roll3_ngs_yac_oe",
  "roll3_ngs_air_yard_share", "roll3_ngs_rush_yoe_pa",
  "roll3_ngs_box_rate", "roll3_ngs_time_to_los"
)

# Keep player-role/efficiency descriptors separate from opponent-defense fields.
# This is the central 2.2 decomposition: the neutral and opportunity learners
# cannot quietly consume defensive features and drown out the explicit matchup
# delta model.
W22_POSITION_ROLE_EXTRA <- list(
  QB = c("roll3_pass_ypa", "roll3_pass_td_rate", "roll3_interception_rate", "roll3_rush_ypc", "roll3_rush_td_rate"),
  RB = c("roll3_catch_rate", "roll3_ypt", "roll3_rush_ypc", "roll3_rec_td_rate", "roll3_rush_td_rate"),
  WR = c("preseason_deep_target_rate", "preseason_redzone_target_rate", "roll3_catch_rate", "roll3_ypt", "roll3_rec_td_rate"),
  TE = c("preseason_middle_target_rate", "preseason_redzone_target_rate", "roll3_catch_rate", "roll3_ypt", "roll3_rec_td_rate")
)
W22_POSITION_MATCHUP_EXTRA <- list(
  QB = c("def_deep_pass_rate_roll4", "def_deep_epa_allowed_roll4"),
  RB = character(),
  WR = c("def_deep_pass_rate_roll4", "def_deep_epa_allowed_roll4", "def_wr_residual_roll4"),
  TE = c("def_middle_pass_rate_roll4", "def_middle_epa_allowed_roll4", "def_te_residual_roll4")
)

get_features22 <- function(position, family = c("full", "neutral", "opportunity", "matchup"), available_names = NULL) {
  pos <- toupper(as.character(position)[1]); family <- match.arg(family)
  role_extra <- W22_POSITION_ROLE_EXTRA[[pos]]; if (is.null(role_extra)) role_extra <- character()
  matchup_extra <- W22_POSITION_MATCHUP_EXTRA[[pos]]; if (is.null(matchup_extra)) matchup_extra <- character()
  out <- switch(
    family,
    neutral = unique(c(W22_ROLE_COMMON, W22_NGS_COMMON, role_extra)),
    opportunity = unique(c(W22_ROLE_COMMON, W22_GAME_COMMON, W22_NGS_COMMON, role_extra)),
    matchup = unique(c("structured_neutral_fppg", "baseline_weekly_fppg", W22_GAME_COMMON, W22_MATCHUP_COMMON, W22_NGS_COMMON, role_extra, matchup_extra)),
    full = unique(c(W22_ROLE_COMMON, W22_GAME_COMMON, W22_MATCHUP_COMMON, W22_NGS_COMMON, role_extra, matchup_extra))
  )
  if (!is.null(available_names)) out <- intersect(out, available_names)
  unique(out)
}

# -----------------------------
# NGS helpers
# -----------------------------
normalize_ngs22 <- function(raw, stat_type) {
  if (is.null(raw) || nrow(raw) == 0) return(data.frame())
  getv <- function(cands, default = NA_real_) wk_num(first_existing_col21(raw, cands, default))
  out <- data.frame(
    season = as.integer(getv("season")),
    week = as.integer(getv("week")),
    player_id = wk_chr(first_existing_col21(raw, c("player_gsis_id", "gsis_id"), NA_character_)),
    stringsAsFactors = FALSE
  )
  if (stat_type == "passing") {
    out$ngs_cpoe <- getv(c("completion_percentage_above_expectation", "completion_percentage_above_expectation_avg"))
    out$ngs_air_yards <- getv(c("avg_intended_air_yards", "avg_air_yards", "avg_air_distance"))
    out$ngs_time_to_throw <- getv(c("avg_time_to_throw", "time_to_throw"))
    out$ngs_aggressiveness <- getv(c("aggressiveness"))
  } else if (stat_type == "receiving") {
    out$ngs_separation <- getv(c("avg_separation"))
    out$ngs_cushion <- getv(c("avg_cushion"))
    out$ngs_yac_oe <- getv(c("avg_yac_above_expectation"))
    out$ngs_air_yard_share <- getv(c("percent_share_of_intended_air_yards"))
  } else if (stat_type == "rushing") {
    out$ngs_rush_yoe_pa <- getv(c("rush_yards_over_expected_per_att"))
    out$ngs_box_rate <- getv(c("percent_attempts_gte_eight_defenders"))
    out$ngs_time_to_los <- getv(c("avg_time_to_los"))
  }
  out <- out[is.finite(out$season) & is.finite(out$week) & out$week >= 1 & out$week <= 18 & !is.na(out$player_id) & nzchar(out$player_id), , drop = FALSE]
  out <- out[!duplicated(out[c("season", "week", "player_id")]), , drop = FALSE]
  out
}

add_ngs_rolls22 <- function(d) {
  if (nrow(d) == 0) return(d)
  d <- d[order(d$week), , drop = FALSE]
  ngs_cols <- c("ngs_cpoe", "ngs_air_yards", "ngs_time_to_throw", "ngs_aggressiveness", "ngs_separation", "ngs_cushion", "ngs_yac_oe", "ngs_air_yard_share", "ngs_rush_yoe_pa", "ngs_box_rate", "ngs_time_to_los")
  for (nm in ngs_cols) {
    if (!nm %in% names(d)) d[[nm]] <- NA_real_
    d[[paste0("roll3_", nm)]] <- lag_roll_mean21(d[[nm]], 3)
  }
  ngs_mat <- as.matrix(as.data.frame(lapply(d[ngs_cols], wk_num)))
  present <- rowSums(is.finite(ngs_mat)) > 0
  d$ngs_available_week <- as.numeric(present)
  d$ngs_available <- lag_roll_mean21(d$ngs_available_week, 3)
  d
}

# -----------------------------
# Shrunk rate/stat-line helpers
# -----------------------------
shrink_rate22 <- function(prior, current, exposure, k, fallback, bounds) {
  prior <- wk_num(prior); current <- wk_num(current); exposure <- pmax(0, wk_num(exposure))
  prior[!is.finite(prior) | prior <= 0] <- fallback
  current[!is.finite(current)] <- prior[!is.finite(current)]
  w <- exposure / (exposure + k)
  wk22_clip((1 - w) * prior + w * current, bounds[1], bounds[2])
}

component_bounds22 <- function(pos, component) {
  x <- WEEKLY_22_COMPONENT_BOUNDS[[pos]][[component]]
  if (is.null(x)) c(0, Inf) else x
}

opportunity_specs22 <- function(pos) {
  switch(pos,
    QB = list(pass_attempts = "pass_attempts", carries = "carries"),
    RB = list(carries = "carries", targets = "targets"),
    WR = list(targets = "targets", carries = "carries"),
    TE = list(targets = "targets"),
    list()
  )
}

fit_opportunity_models22 <- function(train, pos, validation = FALSE, seed = SEED) {
  specs <- opportunity_specs22(pos); out <- list()
  features <- get_features22(pos, "opportunity", names(train))
  nt <- if (validation) WEEKLY_22_OPPORTUNITY_VALIDATION_TREES else WEEKLY_22_OPPORTUNITY_FINAL_TREES
  for (j in seq_along(specs)) {
    target <- unname(specs[[j]])
    if (!target %in% names(train)) next
    y <- wk_num(train[[target]])
    if (sum(is.finite(y)) < WEEKLY_22_MIN_TRAIN_ROWS || stats::sd(y[is.finite(y)]) < 1e-8) next
    out[[names(specs)[j]]] <- fit_fantasy_model(train, features, target, seed = seed + j * 101, n_trees = nt)
  }
  list(position = pos, models = out, features = features)
}

predict_opportunity22 <- function(fit, new_data) {
  pos <- fit$position; specs <- opportunity_specs22(pos); out <- data.frame(row_id_22 = seq_len(nrow(new_data)))
  for (nm in names(specs)) {
    model <- fit$models[[nm]]
    if (is.null(model)) {
      fallback_feature <- switch(nm,
        pass_attempts = "roll3_pass_attempts", targets = "roll3_targets",
        carries = "roll3_carries", unname(specs[[nm]]))
      pred <- if (fallback_feature %in% names(new_data)) wk_num(new_data[[fallback_feature]]) else rep(0, nrow(new_data))
    } else pred <- predict_fantasy_model(model, new_data)
    b <- component_bounds22(pos, nm)
    out[[paste0("projected_", nm)]] <- wk22_clip(pred, b[1], b[2])
  }
  out$row_id_22 <- NULL
  out
}

structured_statline22 <- function(data, opp_pred, pos) {
  d <- data; for (nm in names(opp_pred)) d[[nm]] <- opp_pred[[nm]]
  n <- nrow(d)
  z <- function(nm, default = 0) if (nm %in% names(d)) wk_num(d[[nm]]) else rep(default, n)
  # Seed the result with exactly n rows.  Assigning an n-length prediction
  # vector into a zero-row data.frame raises "replacement has n rows, data has 0".
  # This row id is removed before returning.
  result <- data.frame(row_id_22 = seq_len(n), stringsAsFactors = FALSE)

  if (pos == "QB") {
    att <- z("projected_pass_attempts"); car <- z("projected_carries")
    ypa <- shrink_rate22(z("prior_yards_per_attempt", 7.0), z("season_to_date_pass_ypa", 7.0), z("season_pass_attempts_prior"), WEEKLY_22_RATE_K$pass_attempts, 7.0, c(4.5, 10.0))
    ptd <- shrink_rate22(z("prior_pass_td_rate", .045), z("season_to_date_pass_td_rate", .045), z("season_pass_attempts_prior"), WEEKLY_22_RATE_K$pass_attempts, .045, c(.015, .09))
    intr <- shrink_rate22(z("prior_interception_rate", .025), z("season_to_date_interception_rate", .025), z("season_pass_attempts_prior"), WEEKLY_22_RATE_K$pass_attempts, .025, c(.005, .07))
    rypc <- shrink_rate22(z("prior_yards_per_carry", 4.5), z("season_to_date_rush_ypc", 4.5), z("season_carries_prior"), WEEKLY_22_RATE_K$qb_rush_carries, 4.5, c(1, 9))
    rtd <- shrink_rate22(z("prior_rush_td_rate", .035), z("season_to_date_rush_td_rate", .035), z("season_carries_prior"), WEEKLY_22_RATE_K$qb_rush_carries, .035, c(0, .20))
    result$projected_pass_attempts <- att
    result$projected_pass_yards <- att * ypa
    result$projected_pass_tds <- att * ptd
    result$projected_interceptions <- att * intr
    result$projected_carries <- car
    result$projected_rush_yards <- car * rypc
    result$projected_rush_tds <- car * rtd
    result$projected_targets <- rep(0, n); result$projected_receptions <- rep(0, n); result$projected_rec_yards <- rep(0, n); result$projected_rec_tds <- rep(0, n)
  } else {
    tar <- z("projected_targets"); car <- z("projected_carries")
    catch <- shrink_rate22(z("prior_catch_rate", .65), z("season_to_date_catch_rate", .65), z("season_targets_prior"), WEEKLY_22_RATE_K$rec_targets, .65, c(.30, .95))
    ypt <- shrink_rate22(z("prior_yards_per_target", 7.0), z("season_to_date_ypt", 7.0), z("season_targets_prior"), WEEKLY_22_RATE_K$rec_targets, 7.0, c(2.5, 14))
    rtd <- shrink_rate22(z("prior_rec_td_rate", .045), z("season_to_date_rec_td_rate", .045), z("season_targets_prior"), WEEKLY_22_RATE_K$rec_targets, .045, c(.002, .16))
    result$projected_targets <- tar
    result$projected_receptions <- tar * catch
    result$projected_rec_yards <- tar * ypt
    result$projected_rec_tds <- tar * rtd
    result$projected_pass_attempts <- rep(0, n); result$projected_pass_yards <- rep(0, n); result$projected_pass_tds <- rep(0, n); result$projected_interceptions <- rep(0, n)
    if (pos %in% c("RB", "WR")) {
      ypc <- shrink_rate22(z("prior_yards_per_carry", 4.3), z("season_to_date_rush_ypc", 4.3), z("season_carries_prior"), WEEKLY_22_RATE_K$rush_carries, 4.3, c(2, 10))
      rutd <- shrink_rate22(z("prior_rush_td_rate", .035), z("season_to_date_rush_td_rate", .035), z("season_carries_prior"), WEEKLY_22_RATE_K$rush_carries, .035, c(0, .18))
      result$projected_carries <- car
      result$projected_rush_yards <- car * ypc
      result$projected_rush_tds <- car * rutd
    } else {
      result$projected_carries <- rep(0, n); result$projected_rush_yards <- rep(0, n); result$projected_rush_tds <- rep(0, n)
    }
  }
  result$structured_neutral_fppg <- pmax(0,
    result$projected_pass_yards * SCORING$pass_yd + result$projected_pass_tds * SCORING$pass_td + result$projected_interceptions * SCORING$interception +
    result$projected_rush_yards * SCORING$rush_yd + result$projected_rush_tds * SCORING$rush_td + result$projected_receptions * SCORING$reception +
    result$projected_rec_yards * SCORING$rec_yd + result$projected_rec_tds * SCORING$rec_td
  )
  result$row_id_22 <- NULL
  result
}

fit_structured22 <- function(train, pos, validation = FALSE, seed = SEED) {
  fit_opportunity_models22(train, pos, validation = validation, seed = seed)
}

predict_structured22 <- function(fit, new_data) {
  opp <- predict_opportunity22(fit, new_data)
  structured_statline22(new_data, opp, fit$position)
}

# Cross-fitted residual rows used by the matchup model. This prevents the matchup
# learner from seeing optimistic in-sample residuals from the structured model.
build_matchup_training22 <- function(train, pos, seed = SEED) {
  yrs <- sort(unique(as.integer(train$season)))
  if (length(yrs) < 2) return(data.frame())
  inner <- utils::tail(yrs[-1], WEEKLY_22_INNER_SEASONS)
  rows <- list()
  for (yr in inner) {
    tr <- train[train$season < yr, , drop = FALSE]
    te <- train[train$season == yr, , drop = FALSE]
    if (nrow(tr) < WEEKLY_22_INNER_MIN_ROWS || nrow(te) < 15) next
    fit <- fit_structured22(tr, pos, validation = TRUE, seed = seed + yr)
    st <- predict_structured22(fit, te)
    x <- te
    x$structured_neutral_fppg <- st$structured_neutral_fppg
    x$baseline_weekly_fppg <- weekly_baseline21(x$preseason_prior_fppg, x$roll3_fppg, x$games_played_prior)
    x$matchup_target_residual <- wk_num(x$weekly_fppg) - x$structured_neutral_fppg
    rows[[length(rows) + 1]] <- x
  }
  dplyr::bind_rows(rows)
}

fit_matchup_model22 <- function(train, pos, seed = SEED, validation = FALSE) {
  mtrain <- build_matchup_training22(train, pos, seed = seed)
  if (nrow(mtrain) < WEEKLY_22_MATCHUP_MIN_ROWS) return(NULL)
  feats <- get_features22(pos, "matchup", names(mtrain))
  if (length(feats) < 8) return(NULL)
  nt <- if (validation) WEEKLY_22_MATCHUP_VALIDATION_TREES else WEEKLY_22_MATCHUP_FINAL_TREES
  fit_fantasy_model(mtrain, feats, "matchup_target_residual", seed = seed + 9000, n_trees = nt)
}

predict_matchup_delta22 <- function(model, new_data, pos) {
  if (is.null(model)) return(rep(0, nrow(new_data)))
  cap <- as.numeric(WEEKLY_22_MATCHUP_CAP[[pos]]); if (!is.finite(cap)) cap <- 4
  pred <- suppressWarnings(as.numeric(predict_fantasy_model(model, new_data)))
  pred[!is.finite(pred)] <- 0
  pmin(cap, pmax(-cap, pred))
}

# -----------------------------
# Constrained architecture stack
# -----------------------------
stack_grid22 <- function(step = WEEKLY_22_STACK_STEP) {
  vals <- seq(0, 1, by = step); rows <- list()
  for (a in vals) for (b in vals) for (c in vals) {
    d <- 1 - a - b - c
    if (d < -1e-9 || d > 1 + 1e-9) next
    d <- max(0, d)
    rows[[length(rows) + 1]] <- data.frame(
      w_prior = a, w_neutral = b, w_structured_matchup = c, w_direct = d,
      stringsAsFactors = FALSE
    )
  }
  unique(dplyr::bind_rows(rows))
}

score_stack22 <- function(d, grid = stack_grid22()) {
  if (nrow(d) == 0) return(data.frame())
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, ]
    p <- g$w_prior * d$baseline_weekly_fppg + g$w_neutral * d$neutral_direct_fppg +
      g$w_structured_matchup * d$structured_matchup_fppg + g$w_direct * d$direct_full_fppg
    data.frame(
      g,
      MAE = mean(abs(p - d$actual_fppg), na.rm = TRUE),
      RMSE = sqrt(mean((p - d$actual_fppg)^2, na.rm = TRUE)),
      correlation = safe_cor21(p, d$actual_fppg),
      rank_correlation = safe_cor21(p, d$actual_fppg, method = "spearman"),
      bias = mean(p - d$actual_fppg, na.rm = TRUE)
    )
  })
  dplyr::bind_rows(rows) |> dplyr::arrange(MAE, RMSE, dplyr::desc(correlation))
}

apply_stack22 <- function(d, w) {
  if (nrow(w) == 0) return(d$baseline_weekly_fppg)
  wk_num(w$w_prior[1]) * d$baseline_weekly_fppg + wk_num(w$w_neutral[1]) * d$neutral_direct_fppg +
    wk_num(w$w_structured_matchup[1]) * d$structured_matchup_fppg + wk_num(w$w_direct[1]) * d$direct_full_fppg
}

safe_start_stack22 <- function() data.frame(w_prior = 1, w_neutral = 0, w_structured_matchup = 0, w_direct = 0)

# -----------------------------
# Validation cohorts + probability distribution
# -----------------------------
add_pregame_cohorts22 <- function(d) {
  d |> dplyr::group_by(season, week, position) |>
    dplyr::arrange(dplyr::desc(baseline_weekly_fppg), .by_group = TRUE) |>
    dplyr::mutate(
      baseline_rank = dplyr::row_number(),
      starter_cohort = baseline_rank <= as.numeric(WEEKLY_22_STARTER_CUTOFF[position]),
      relevant_cohort = baseline_rank <= as.numeric(WEEKLY_22_RELEVANT_CUTOFF[position])
    ) |> dplyr::ungroup()
}

empirical_distribution22 <- function(pred, residuals, pos, draws = WEEKLY_22_SIM_DRAWS) {
  residuals <- wk_num(residuals); residuals <- residuals[is.finite(residuals)]
  if (length(residuals) < 30) residuals <- stats::rnorm(max(200, draws), 0, ifelse(pos == "QB", 7, 5))
  n <- length(pred)
  out <- data.frame(
    weekly_median = numeric(n), weekly_floor = numeric(n), weekly_ceiling = numeric(n),
    boom_probability = numeric(n), bust_probability = numeric(n), stringsAsFactors = FALSE
  )
  for (i in seq_len(n)) {
    set.seed(SEED + i + nchar(pos) * 10000)
    sim <- pmax(0, wk_num(pred[i]) + sample(residuals, size = draws, replace = TRUE))
    out$weekly_median[i] <- stats::median(sim)
    out$weekly_floor[i] <- as.numeric(stats::quantile(sim, .20, names = FALSE, type = 7))
    out$weekly_ceiling[i] <- as.numeric(stats::quantile(sim, .80, names = FALSE, type = 7))
    out$boom_probability[i] <- mean(sim >= as.numeric(WEEKLY_22_BOOM_THRESHOLD[[pos]]))
    out$bust_probability[i] <- mean(sim <= as.numeric(WEEKLY_22_BUST_THRESHOLD[[pos]]))
  }
  out
}

matchup_grade22 <- function(delta) {
  dplyr::case_when(
    delta >= 2.5 ~ "A+", delta >= 1.5 ~ "A", delta >= 0.75 ~ "B+",
    delta > -0.75 ~ "Neutral", delta > -1.5 ~ "C", delta > -2.5 ~ "D", TRUE ~ "F"
  )
}

cat("[2.2] Weekly accuracy engine loaded: neutral role + opportunity/stat line + matchup delta + direct model + honest stack.\n")
