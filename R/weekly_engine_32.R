# ============================================================
# FANTASY MODEL 3.2 - SIGNAL / NOISE OPTIMIZATION ENGINE
# ============================================================
# Experimental challenger. 3.1 remains production unless 3.2 earns promotion.
# Every derived factor is built only from fields available before kickoff.

if (!exists("MODEL31_SAFE_FEATURES")) source("R/weekly_engine_31.R")

MODEL32_FACTOR_FEATURES <- c(
  "snap_change_scaled_32",
  "pass_attempt_trend_32",
  "role_shift_corrected_32",
  "role_stability_32",
  "opportunity_momentum_32",
  "primary_role_share_32",
  "teammate_absence_role_32",
  "questionable_role_pressure_32",
  "pace_scoring_env_32",
  "pass_script_role_32",
  "rush_script_role_32",
  "recent_vs_season_32",
  "consistency_adjusted_form_32",
  "redzone_role_env_32",
  "pressure_exposure_32"
)
MODEL32_VARIANTS <- c("full", "full_plus", "compact", "compact_plus")

wk32_num <- function(x, default = 0) {
  z <- suppressWarnings(as.numeric(x))
  z[!is.finite(z)] <- default
  z
}

wk32_add_factors <- function(d) {
  if (!nrow(d)) return(d)
  get <- function(nm, default = 0) {
    if (nm %in% names(d)) wk32_num(d[[nm]], default) else rep(default, nrow(d))
  }
  pos <- if ("position" %in% names(d)) toupper(as.character(d$position)) else rep("", nrow(d))

  snap_delta <- get("snap_trend")
  target_delta <- get("target_trend")
  carry_delta <- get("carry_trend")
  pass_delta <- get("roll3_pass_attempts") - get("roll5_pass_attempts")
  role_fppg_delta <- get("role_fppg_trend")

  # offense_pct is normalized to 0-1 upstream. 3.1's legacy snap term divided
  # this by 30, effectively muting it. This factor restores the intended scale.
  d$snap_change_scaled_32 <- pmax(-4, pmin(4, snap_delta / 0.15))
  d$pass_attempt_trend_32 <- pmax(-4, pmin(4, pass_delta / 8))

  shift <- abs(role_fppg_delta) / 4 +
    abs(snap_delta) / 0.15 +
    abs(target_delta) / 3 +
    abs(carry_delta) / 4 +
    abs(pass_delta) / 10
  d$role_shift_corrected_32 <- tanh(shift)
  d$role_stability_32 <- exp(-pmin(6, shift))

  momentum_raw <- ifelse(
    pos == "QB", 0.75 * pass_delta / 8 + 0.25 * carry_delta / 2,
    ifelse(pos == "RB", 0.65 * carry_delta / 4 + 0.35 * target_delta / 2,
      target_delta / 2)
  )
  d$opportunity_momentum_32 <- tanh(momentum_raw + 0.35 * d$snap_change_scaled_32)

  target_share <- pmax(0, get("roll3_target_share"))
  carry_share <- pmax(0, get("roll3_carry_share"))
  pass_share <- pmax(0, get("roll3_pass_attempt_share"))
  primary_share <- ifelse(
    pos == "QB", pass_share,
    ifelse(pos == "RB", pmax(carry_share, 0.70 * target_share), target_share)
  )
  d$primary_role_share_32 <- pmin(1.5, primary_share)

  out_count <- pmax(0, get("team_skill_out_count"))
  q_count <- pmax(0, get("team_skill_questionable_count"))
  d$teammate_absence_role_32 <- pmin(4, out_count * d$primary_role_share_32)
  d$questionable_role_pressure_32 <- pmin(4, 0.35 * q_count * d$primary_role_share_32)

  plays <- pmax(0, get("expected_team_plays"))
  implied <- pmax(0, get("implied_team_total"))
  pass_rate <- pmin(1, pmax(0, get("expected_team_pass_rate", .56)))
  favorite <- pmax(0, get("favorite_points"))
  underdog <- pmax(0, get("underdog_points"))

  d$pace_scoring_env_32 <- pmin(4, (plays / 65) * (implied / 24))
  d$pass_script_role_32 <- ifelse(
    pos %in% c("QB","WR","TE"),
    pmin(4, pass_rate * (1 + underdog / 10) * (0.5 + d$primary_role_share_32)),
    pmin(4, pass_rate * (1 + underdog / 10) * (0.25 + target_share))
  )
  d$rush_script_role_32 <- ifelse(
    pos == "RB",
    pmin(4, (1 - pass_rate) * (1 + favorite / 10) * (0.5 + carry_share)),
    pmin(4, (1 - pass_rate) * (1 + favorite / 10) * (0.2 + carry_share))
  )

  recent_gap <- get("roll3_fppg") - get("season_to_date_fppg")
  volatility <- pmax(0, get("roll3_fppg_sd"))
  d$recent_vs_season_32 <- pmax(-20, pmin(20, recent_gap))
  d$consistency_adjusted_form_32 <- pmax(-8, pmin(8, recent_gap / (1 + volatility)))

  rz_pass <- pmax(0, get("def_redzone_pass_td_rate_roll4"))
  rz_rush <- pmax(0, get("def_redzone_rush_td_rate_roll4"))
  d$redzone_role_env_32 <- ifelse(
    pos == "RB",
    pmin(4, (implied / 24) * (0.5 + carry_share) * (0.5 + 8 * rz_rush)),
    pmin(4, (implied / 24) * (0.5 + d$primary_role_share_32) * (0.5 + 8 * rz_pass))
  )

  pressure <- pmax(0, get("def_sack_rate_roll4")) + 0.5 * pmax(0, get("def_qb_hit_rate_roll4"))
  ttt <- pmax(0, get("roll3_ngs_time_to_throw"))
  d$pressure_exposure_32 <- ifelse(
    pos == "QB",
    pmin(5, pressure * (0.5 + get("roll3_pass_attempts") / 35) * (0.5 + ttt / 3)),
    0
  )
  d
}

wk32_anchor_features <- function(pos) {
  common <- c(
    "preseason_prior_fppg","roll3_fppg","season_to_date_fppg",
    "roll3_offense_pct","roll3_offense_snaps","implied_team_total",
    "expected_team_plays","expected_team_pass_rate",
    "team_skill_out_count","team_skill_questionable_count",
    "injury_risk","practice_risk","season_week"
  )
  extra <- switch(toupper(pos),
    QB = c("roll3_pass_attempts","roll3_pass_attempt_share","roll3_carries",
           "roll3_pass_ypa","roll3_pass_td_rate","qb_pass_matchup","qb_pressure_matchup"),
    RB = c("roll3_carries","roll3_carry_share","roll3_targets","roll3_target_share",
           "roll3_rush_ypc","rb_rush_matchup","rb_receiving_matchup"),
    WR = c("roll3_targets","roll3_target_share","roll3_ypt","roll3_catch_rate",
           "wr_volume_matchup","wr_deep_matchup","wr_redzone_matchup"),
    TE = c("roll3_targets","roll3_target_share","roll3_ypt","roll3_catch_rate",
           "te_middle_matchup","te_redzone_matchup"),
    character()
  )
  unique(c(common, extra))
}

wk32_feature_audit <- function(d, pos, features = MODEL31_SAFE_FEATURES,
                               target_col = "weekly_fppg") {
  d <- d[toupper(as.character(d$position)) == toupper(pos), , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  y <- wk32_num(d[[target_col]], NA_real_)
  years <- sort(unique(as.integer(d$season)))
  rows <- lapply(features, function(nm) {
    x <- if (nm %in% names(d)) suppressWarnings(as.numeric(d[[nm]])) else rep(NA_real_, nrow(d))
    finite <- is.finite(x) & is.finite(y)
    coverage <- mean(is.finite(x))
    sdx <- if (sum(finite) >= 5) stats::sd(x[finite]) else NA_real_
    distinct <- length(unique(x[is.finite(x)]))
    overall <- if (sum(finite) >= 30 && is.finite(sdx) && sdx > 1e-10) {
      suppressWarnings(stats::cor(x[finite], y[finite], method = "spearman"))
    } else NA_real_

    yr_cor <- vapply(years, function(yy) {
      ii <- finite & as.integer(d$season) == yy
      if (sum(ii) < 25 || stats::sd(x[ii]) <= 1e-10 || stats::sd(y[ii]) <= 1e-10) return(NA_real_)
      suppressWarnings(stats::cor(x[ii], y[ii], method = "spearman"))
    }, numeric(1))
    ok <- is.finite(yr_cor)
    med_abs <- if (any(ok)) stats::median(abs(yr_cor[ok])) else 0
    sign_consistency <- if (any(ok)) max(mean(yr_cor[ok] >= 0), mean(yr_cor[ok] <= 0)) else 0
    score <- 0.55 * abs(ifelse(is.finite(overall), overall, 0)) +
      0.30 * med_abs + 0.15 * sign_consistency
    data.frame(
      feature = nm, coverage = coverage, sd = sdx, distinct = distinct,
      spearman = overall, median_abs_year_spearman = med_abs,
      sign_consistency = sign_consistency, signal_score = score,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

wk32_select_compact_features <- function(d, pos, features = MODEL31_SAFE_FEATURES,
                                         min_features = 30L, max_features = 55L) {
  audit <- wk32_feature_audit(d, pos, features)
  if (!nrow(audit)) return(features)
  anchors <- intersect(wk32_anchor_features(pos), features)

  usable <- audit |>
    dplyr::filter(
      coverage >= 0.70,
      is.finite(sd), sd > 1e-8,
      distinct >= 5
    ) |>
    dplyr::arrange(dplyr::desc(signal_score))

  chosen <- unique(c(anchors, head(usable$feature, max_features)))
  chosen <- chosen[chosen %in% features]
  if (length(chosen) < min_features) {
    chosen <- unique(c(chosen, head(usable$feature, min_features)))
  }

  # Redundancy pruning happens on training data only. Anchors win ties.
  xdf <- as.data.frame(lapply(chosen, function(nm) {
    if (nm %in% names(d)) wk32_num(d[[nm]]) else rep(0, nrow(d))
  }))
  names(xdf) <- chosen
  cm <- suppressWarnings(stats::cor(xdf, use = "pairwise.complete.obs"))
  score_map <- stats::setNames(audit$signal_score, audit$feature)
  keep <- character()
  ordered <- unique(c(anchors, chosen[order(-score_map[chosen], na.last = TRUE)]))
  for (nm in ordered) {
    if (!nm %in% chosen) next
    if (!length(keep)) {
      keep <- c(keep, nm)
      next
    }
    high <- any(abs(cm[nm, keep, drop = TRUE]) >= 0.97, na.rm = TRUE)
    if (!high || nm %in% anchors) keep <- c(keep, nm)
  }
  keep <- unique(keep)
  if (length(keep) < min_features) {
    fill <- setdiff(chosen, keep)
    keep <- c(keep, head(fill, min_features - length(keep)))
  }
  keep
}

wk32_variant_features <- function(train, pos, variant) {
  variant <- match.arg(variant, MODEL32_VARIANTS)
  train <- wk32_add_factors(train)
  base <- MODEL31_SAFE_FEATURES
  if (variant %in% c("compact","compact_plus")) {
    base <- wk32_select_compact_features(train, pos, base)
  }
  if (variant %in% c("full_plus","compact_plus")) {
    base <- unique(c(base, MODEL32_FACTOR_FEATURES))
  }
  base
}

wk32_cohort_flags <- function(d, pos) {
  expected <- wk31_expected_fppg(d)
  cut <- list(
    QB = c(relevant = 12, starter = 16),
    RB = c(relevant = 6, starter = 10),
    WR = c(relevant = 6, starter = 10),
    TE = c(relevant = 4.5, starter = 8)
  )[[toupper(pos)]]
  list(
    relevant = expected >= cut[["relevant"]],
    starter = expected >= cut[["starter"]]
  )
}

wk32_metrics <- function(actual, pred) {
  wk31_core_metrics(actual, pred)
}

wk32_cohort_metrics <- function(d, pred_col, pos, actual_col = "weekly_fppg") {
  flags <- wk32_cohort_flags(d, pos)
  masks <- list(All = rep(TRUE, nrow(d)), Relevant = flags$relevant, Starter = flags$starter)
  dplyr::bind_rows(lapply(names(masks), function(lbl) {
    z <- d[masks[[lbl]], , drop = FALSE]
    cbind(data.frame(cohort = lbl, stringsAsFactors = FALSE),
          wk32_metrics(z[[actual_col]], z[[pred_col]]))
  }))
}

wk32_complexity_penalty <- function(n_features) {
  # Tiny tie-break only; never outweigh a real MAE gain.
  0.00015 * pmax(0, n_features - 30)
}
