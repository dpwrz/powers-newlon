# ============================================================
# FANTASY MODEL 3.0 - ROLE / REGIME / REDISTRIBUTION CHALLENGER
# ============================================================
# This engine is intentionally shadow-mode. It produces validated role
# forecasts and features without overwriting the locked 2.4.3 projection.

source("config.R")
ensure_packages(c("dplyr", "readr", "tibble", "purrr", "rpart"))

fm30_num <- function(x, default = 0) {
  z <- suppressWarnings(as.numeric(x)); z[!is.finite(z)] <- default; z
}
fm30_clip <- function(x, lo = 0, hi = Inf) pmin(hi, pmax(lo, fm30_num(x)))
fm30_key <- function(season, week) 100L * as.integer(season) + as.integer(week)

fm30_role_features <- function(position, available_names = NULL) {
  common <- c(
    "games_played_prior", "preseason_prior_fppg", "weekly_is_rookie", "weekly_age", "weekly_experience",
    "roll3_fppg", "roll5_fppg", "season_to_date_fppg", "roll3_fppg_sd",
    "roll3_targets", "roll5_targets", "roll3_carries", "roll5_carries", "roll3_pass_attempts", "roll5_pass_attempts",
    "roll3_target_share", "roll3_carry_share", "roll3_pass_attempt_share",
    "roll3_offense_pct", "roll5_offense_pct", "roll3_offense_snaps",
    "snap_trend", "role_fppg_trend", "target_trend", "carry_trend", "opportunity_per_snap",
    "season_targets_prior", "season_carries_prior", "season_pass_attempts_prior",
    "roll3_team_pass_attempts", "roll3_team_carries", "expected_team_plays", "expected_team_pass_rate",
    "implied_team_total", "team_spread_line", "rest_days", "is_home",
    "injury_risk", "practice_risk", "team_skill_out_count", "team_skill_questionable_count",
    "unavailable_target_share_30", "unavailable_carry_share_30", "unavailable_pass_share_30", "unavailable_snap_share_30",
    "target_redistribution_bump_30", "carry_redistribution_bump_30", "snap_redistribution_bump_30",
    "snap_accel_30", "target_accel_30", "carry_accel_30", "pass_accel_30",
    "role_shift_magnitude_30", "role_regime_probability_30", "role_regime_direction_30",
    "phase_preseason_weight_30", "phase_recent_weight_30", "phase_stable_weight_30"
  )
  pos <- toupper(as.character(position)[1])
  extra <- switch(pos,
    QB = c("prior_yards_per_attempt", "roll3_pass_ypa", "season_to_date_pass_ypa", "roll3_ngs_cpoe", "roll3_ngs_time_to_throw"),
    RB = c("prior_yards_per_carry", "roll3_rush_ypc", "season_to_date_rush_ypc", "roll3_catch_rate", "roll3_ypt", "rb_rush_matchup", "rb_receiving_matchup"),
    WR = c("prior_catch_rate", "prior_yards_per_target", "roll3_catch_rate", "roll3_ypt", "roll3_ngs_air_yard_share", "wr_volume_matchup", "wr_deep_matchup"),
    TE = c("prior_catch_rate", "prior_yards_per_target", "roll3_catch_rate", "roll3_ypt", "te_middle_matchup", "te_redzone_matchup"),
    character()
  )
  out <- unique(c(common, extra))
  if (!is.null(available_names)) out <- intersect(out, available_names)
  out
}

fm30_build_injury_pressure <- function(weekly_history, injury_path = "data/raw/injuries_weekly_model.csv") {
  if (!file.exists(injury_path)) return(tibble::tibble())
  inj <- readr::read_csv(injury_path, show_col_types = FALSE) |>
    dplyr::filter(position %in% c("QB", "RB", "WR", "TE")) |>
    dplyr::mutate(
      player_id = as.character(gsis_id),
      injury_key = fm30_key(season, week),
      report_status = trimws(as.character(report_status)),
      status_weight = dplyr::case_when(
        tolower(report_status) == "out" ~ 1.00,
        tolower(report_status) == "doubtful" ~ 0.80,
        tolower(report_status) == "questionable" ~ 0.25,
        TRUE ~ 0
      ),
      injury_row = dplyr::row_number()
    ) |>
    dplyr::filter(status_weight > 0, nzchar(player_id)) |>
    dplyr::select(injury_row, season, week, team, player_id, report_status, status_weight, injury_key)
  if (!nrow(inj)) return(tibble::tibble())

  hist <- weekly_history |>
    dplyr::transmute(
      player_id = as.character(player_id), hist_key = fm30_key(season, week),
      last_target_share = fm30_num(target_share_week),
      last_carry_share = fm30_num(carry_share_week),
      last_pass_share = fm30_num(pass_attempt_share_week),
      last_snap_share = fm30_num(offense_pct)
    )

  # Historical rows are small enough that a player-key join followed by a
  # chronological filter is reliable and keeps the feature strictly pre-kickoff.
  prior <- inj |>
    # This is intentionally many-to-many: each injury report row is matched to
    # that player's historical weekly rows, then reduced to the latest strictly
    # prior observation below.
    dplyr::left_join(hist, by = "player_id", relationship = "many-to-many") |>
    dplyr::filter(is.na(hist_key) | hist_key < injury_key) |>
    dplyr::group_by(injury_row) |>
    dplyr::arrange(dplyr::desc(hist_key), .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      last_target_share = dplyr::coalesce(last_target_share, 0),
      last_carry_share = dplyr::coalesce(last_carry_share, 0),
      last_pass_share = dplyr::coalesce(last_pass_share, 0),
      last_snap_share = dplyr::coalesce(last_snap_share, 0)
    )

  prior |>
    dplyr::group_by(season, week, team) |>
    dplyr::summarise(
      unavailable_target_share_30 = pmin(0.85, sum(status_weight * last_target_share, na.rm = TRUE)),
      unavailable_carry_share_30 = pmin(0.95, sum(status_weight * last_carry_share, na.rm = TRUE)),
      unavailable_pass_share_30 = pmin(1.00, sum(status_weight * last_pass_share, na.rm = TRUE)),
      unavailable_snap_share_30 = pmin(2.50, sum(status_weight * last_snap_share, na.rm = TRUE)),
      out_skill_count_30 = sum(status_weight >= 0.75, na.rm = TRUE),
      questionable_skill_count_30 = sum(status_weight > 0 & status_weight < 0.75, na.rm = TRUE),
      .groups = "drop"
    )
}

fm30_add_phase_regime_features <- function(d) {
  out <- d
  n <- nrow(out)
  get <- function(nm) if (nm %in% names(out)) fm30_num(out[[nm]]) else rep(0, n)
  games <- pmax(0, get("games_played_prior"))
  out$snap_accel_30 <- get("roll3_offense_pct") - get("roll5_offense_pct")
  out$target_accel_30 <- get("roll3_targets") - get("roll5_targets")
  out$carry_accel_30 <- get("roll3_carries") - get("roll5_carries")
  out$pass_accel_30 <- get("roll3_pass_attempts") - get("roll5_pass_attempts")
  out$role_shift_magnitude_30 <- pmin(4, 3.5 * abs(out$snap_accel_30) + 0.18 * abs(out$target_accel_30) +
    0.12 * abs(out$carry_accel_30) + 0.06 * abs(out$pass_accel_30) + 0.8 * abs(get("role_fppg_trend")))
  direction_raw <- 2.8 * out$snap_accel_30 + 0.12 * get("target_trend") + 0.08 * get("carry_trend") + 0.03 * out$pass_accel_30
  out$role_regime_direction_30 <- pmax(-1, pmin(1, direction_raw))
  absence <- get("unavailable_target_share_30") + get("unavailable_carry_share_30") + 0.25 * get("unavailable_snap_share_30")
  out$role_regime_probability_30 <- stats::plogis(-1.65 + 1.15 * out$role_shift_magnitude_30 + 1.10 * pmin(1.5, absence) +
    0.35 * get("team_skill_out_count") + 0.12 * get("team_skill_questionable_count"))
  out$phase_preseason_weight_30 <- exp(-games / 3.8)
  out$phase_recent_weight_30 <- 1 - exp(-games / 3.2)
  out$phase_stable_weight_30 <- pmax(0, 1 - out$phase_preseason_weight_30 - 0.35 * out$role_regime_probability_30)
  out
}


# Add static current-season player context required by role models. The locked
# weekly 2.4.3 output intentionally omits age/experience/rookie columns, while
# the historical role training table contains them. Reconstruct those fields
# from the current-season season-projection feature store before live scoring.
fm30_add_live_static_context <- function(d, season = CURRENT_SEASON) {
  out <- d
  n <- nrow(out)
  if (!"player_id" %in% names(out) || n == 0) return(out)

  wanted <- c("weekly_age", "weekly_experience", "weekly_is_rookie")
  for (nm in wanted) if (!nm %in% names(out)) out[[nm]] <- NA_real_

  candidate_paths <- c(
    paste0("data/processed/projection_table_", season, ".csv"),
    paste0("output/", season, "_projections.csv"),
    paste0("output/final_", season, "_rankings.csv")
  )
  candidate_paths <- candidate_paths[file.exists(candidate_paths)]

  context <- NULL
  for (path in candidate_paths) {
    x <- tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
    if (is.null(x) || !"player_id" %in% names(x)) next
    age_col <- if ("weekly_age" %in% names(x)) "weekly_age" else if ("age" %in% names(x)) "age" else NA_character_
    exp_col <- if ("weekly_experience" %in% names(x)) "weekly_experience" else if ("experience" %in% names(x)) "experience" else NA_character_
    rook_col <- if ("weekly_is_rookie" %in% names(x)) "weekly_is_rookie" else if ("is_rookie" %in% names(x)) "is_rookie" else NA_character_
    if (all(is.na(c(age_col, exp_col, rook_col)))) next

    ctx <- tibble::tibble(player_id = as.character(x$player_id))
    ctx$weekly_age <- if (!is.na(age_col)) suppressWarnings(as.numeric(x[[age_col]])) else NA_real_
    ctx$weekly_experience <- if (!is.na(exp_col)) suppressWarnings(as.numeric(x[[exp_col]])) else NA_real_
    ctx$weekly_is_rookie <- if (!is.na(rook_col)) suppressWarnings(as.numeric(x[[rook_col]])) else NA_real_
    ctx <- ctx[!duplicated(ctx$player_id) & nzchar(ctx$player_id), , drop = FALSE]
    context <- if (is.null(context)) ctx else dplyr::bind_rows(context, ctx) |>
      dplyr::group_by(.data$player_id) |>
      dplyr::summarise(
        weekly_age = dplyr::first(.data$weekly_age[is.finite(.data$weekly_age)], default = NA_real_),
        weekly_experience = dplyr::first(.data$weekly_experience[is.finite(.data$weekly_experience)], default = NA_real_),
        weekly_is_rookie = dplyr::first(.data$weekly_is_rookie[is.finite(.data$weekly_is_rookie)], default = NA_real_),
        .groups = "drop"
      )
  }

  if (!is.null(context) && nrow(context)) {
    m <- match(as.character(out$player_id), as.character(context$player_id))
    for (nm in wanted) {
      current <- suppressWarnings(as.numeric(out[[nm]]))
      replacement <- suppressWarnings(as.numeric(context[[nm]][m]))
      use <- !is.finite(current) & is.finite(replacement)
      current[use] <- replacement[use]
      out[[nm]] <- current
    }
  }

  # Last-resort, position-aware imputation keeps live scoring operational for a
  # newly added player missing from the season feature store. This is preferable
  # to silently passing an impossible age/experience of zero into the trees.
  pos <- if ("position" %in% names(out)) toupper(as.character(out$position)) else rep("", n)
  defaults_age <- c(QB = 27, RB = 25, WR = 26, TE = 27)
  defaults_exp <- c(QB = 4, RB = 3, WR = 3, TE = 4)
  for (i in seq_len(n)) {
    pp <- pos[i]
    if (!is.finite(out$weekly_age[i])) out$weekly_age[i] <- if (pp %in% names(defaults_age)) defaults_age[[pp]] else 26
    if (!is.finite(out$weekly_experience[i])) out$weekly_experience[i] <- if (pp %in% names(defaults_exp)) defaults_exp[[pp]] else 3
    if (!is.finite(out$weekly_is_rookie[i])) out$weekly_is_rookie[i] <- 0
  }

  matched <- if (is.null(context)) 0L else sum(as.character(out$player_id) %in% as.character(context$player_id))
  cat("[3.0 ROLE] Live static context: matched ", matched, "/", n,
      " players; age/experience/rookie features ready.\n", sep = "")
  out
}

fm30_add_redistribution_features <- function(d) {
  out <- d
  for (nm in c("unavailable_target_share_30", "unavailable_carry_share_30", "unavailable_pass_share_30", "unavailable_snap_share_30")) {
    if (!nm %in% names(out)) out[[nm]] <- 0
    out[[nm]] <- fm30_num(out[[nm]])
  }
  out <- out |>
    dplyr::mutate(
      .target_claim = dplyr::if_else(position %in% c("RB","WR","TE"), pmax(0.015, fm30_num(roll3_target_share)), 0),
      .carry_claim = dplyr::if_else(position %in% c("QB","RB"), pmax(0.015, fm30_num(roll3_carry_share)), 0),
      .snap_claim = pmax(0.05, fm30_num(roll3_offense_pct))
    ) |>
    dplyr::group_by(season, week, team) |>
    dplyr::mutate(
      .target_den = sum(.target_claim, na.rm = TRUE),
      .carry_den = sum(.carry_claim, na.rm = TRUE),
      .snap_den = sum(.snap_claim, na.rm = TRUE),
      target_redistribution_bump_30 = 0.78 * unavailable_target_share_30 * dplyr::if_else(.target_den > 0, .target_claim / .target_den, 0),
      carry_redistribution_bump_30 = 0.82 * unavailable_carry_share_30 * dplyr::if_else(.carry_den > 0, .carry_claim / .carry_den, 0),
      snap_redistribution_bump_30 = 0.40 * unavailable_snap_share_30 * dplyr::if_else(.snap_den > 0, .snap_claim / .snap_den, 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::starts_with("."))
  out
}

fm30_enrich_weekly_table <- function(weekly_history, injury_pressure = NULL) {
  out <- weekly_history
  if (!is.null(injury_pressure) && nrow(injury_pressure)) {
    out <- out |> dplyr::left_join(injury_pressure, by = c("season", "week", "team"))
  }
  out <- fm30_add_redistribution_features(out)
  fm30_add_phase_regime_features(out)
}

fm30_baseline_for_target <- function(d, position, target) {
  n <- nrow(d); get <- function(nm) if (nm %in% names(d)) fm30_num(d[[nm]]) else rep(0, n)
  pred <- switch(target,
    "pass_attempts" = dplyr::coalesce(get("roll3_pass_attempts"), get("roll5_pass_attempts")),
    "pass_attempt_share_week" = get("roll3_pass_attempt_share"),
    "carries" = get("roll3_carries") + 0.5 * get("carry_redistribution_bump_30") * pmax(1, get("roll3_team_carries")),
    "targets" = get("roll3_targets") + 0.5 * get("target_redistribution_bump_30") * pmax(1, get("roll3_team_pass_attempts")),
    "carry_share_week" = get("roll3_carry_share") + 0.5 * get("carry_redistribution_bump_30"),
    "target_share_week" = get("roll3_target_share") + 0.5 * get("target_redistribution_bump_30"),
    "offense_pct" = get("roll3_offense_pct") + 0.5 * get("snap_redistribution_bump_30"),
    rep(0, n)
  )
  if (target %in% c("pass_attempt_share_week", "carry_share_week", "target_share_week", "offense_pct")) pred <- fm30_clip(pred, 0, 1)
  if (target %in% c("pass_attempts", "carries", "targets")) pred <- fm30_clip(pred, 0, Inf)
  pred
}

fm30_metrics <- function(actual, pred) {
  a <- fm30_num(actual, NA_real_); p <- fm30_num(pred, NA_real_)
  k <- is.finite(a) & is.finite(p)
  if (sum(k) < 10) return(c(MAE = Inf, RMSE = Inf, Cor = NA_real_))
  c(MAE = mean(abs(a[k] - p[k])), RMSE = sqrt(mean((a[k] - p[k])^2)),
    Cor = if (stats::sd(a[k]) > 0 && stats::sd(p[k]) > 0) stats::cor(a[k], p[k]) else NA_real_)
}

fm30_fit_role_model <- function(d, position, target, n_trees = ROLE30_ENSEMBLE_TREES) {
  position_name <- toupper(as.character(position)[1])
  target_name <- as.character(target)[1]
  feats <- fm30_role_features(position_name, names(d))
  if (!"position" %in% names(d) || !target_name %in% names(d)) return(NULL)
  target_values <- suppressWarnings(as.numeric(d[[target_name]]))
  keep <- as.character(d$position) == position_name & is.finite(target_values)
  pos <- d[keep, , drop = FALSE]
  if (nrow(pos) < ROLE30_MIN_POSITION_ROWS || length(feats) < 8) return(NULL)
  fit_fantasy_model(pos, feats, target_name, n_trees = n_trees)
}

fm30_predict_role_model <- function(model, d, target) {
  if (is.null(model)) return(rep(NA_real_, nrow(d)))
  p <- predict_fantasy_model(model, d)
  if (target %in% c("pass_attempt_share_week", "carry_share_week", "target_share_week", "offense_pct")) p <- fm30_clip(p, 0, 1)
  if (target %in% c("pass_attempts", "carries", "targets")) p <- fm30_clip(p, 0, Inf)
  p
}

fm30_promotion <- function(base_metrics, model_metrics) {
  is.finite(base_metrics[["MAE"]]) && is.finite(model_metrics[["MAE"]]) &&
    model_metrics[["MAE"]] <= base_metrics[["MAE"]] * (1 - ROLE30_PROMOTION_MIN_MAE_GAIN) &&
    model_metrics[["RMSE"]] <= base_metrics[["RMSE"]] * ROLE30_PROMOTION_MAX_RMSE_RATIO
}
