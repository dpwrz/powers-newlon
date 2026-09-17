# ============================================================
# FANTASY MODEL 3.0 - DYNASTY VALUE ENGINE
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "readr", "tibble"))
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

fm3_rescale01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ok <- is.finite(x)
  if (!any(ok)) return(rep(0, length(x)))
  lo <- min(x[ok]); hi <- max(x[ok])
  if (!is.finite(hi - lo) || hi <= lo) return(ifelse(ok, 0.5, 0))
  out <- (x - lo) / (hi - lo)
  out[!ok] <- 0
  pmin(1, pmax(0, out))
}

fm3_format_premium <- function(position, settings) {
  pos <- toupper(as.character(position))
  sf <- suppressWarnings(as.numeric(settings$superflex_starters %||% 0))
  qbs <- suppressWarnings(as.numeric(settings$qb_starters %||% 1))
  tep <- suppressWarnings(as.numeric(settings$te_premium %||% 0))
  teams <- suppressWarnings(as.numeric(settings$teams %||% 12))
  if (!is.finite(sf)) sf <- 0
  if (!is.finite(qbs)) qbs <- 1
  if (!is.finite(tep)) tep <- 0
  if (!is.finite(teams)) teams <- 12
  qb_mult <- 1 + pmin(0.45, 0.20 * sf + 0.015 * pmax(0, teams - 10) + 0.06 * pmax(0, qbs - 1))
  te_mult <- 1 + pmin(0.25, 0.12 * tep)
  dplyr::case_when(pos == "QB" ~ qb_mult, pos == "TE" ~ te_mult, TRUE ~ 1)
}

fm3_percentile_rank <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(0, length(x)); ok <- is.finite(x)
  n <- sum(ok)
  if (!n) return(out)
  if (n == 1) { out[ok] <- 0.5; return(out) }
  out[ok] <- (rank(x[ok], ties.method = "average") - 1) / (n - 1)
  out
}

fm3_dynasty_replacement_ranks <- function(settings) {
  teams <- suppressWarnings(as.numeric(settings$teams %||% 12)); if (!is.finite(teams)) teams <- 12
  qb <- suppressWarnings(as.numeric(settings$qb_starters %||% 1)); if (!is.finite(qb)) qb <- 1
  rb <- suppressWarnings(as.numeric(settings$rb_starters %||% 2)); if (!is.finite(rb)) rb <- 2
  wr <- suppressWarnings(as.numeric(settings$wr_starters %||% 2)); if (!is.finite(wr)) wr <- 2
  te <- suppressWarnings(as.numeric(settings$te_starters %||% 1)); if (!is.finite(te)) te <- 1
  flex <- suppressWarnings(as.numeric(settings$flex_starters %||% 1)); if (!is.finite(flex)) flex <- 1
  sf <- suppressWarnings(as.numeric(settings$superflex_starters %||% 0)); if (!is.finite(sf)) sf <- 0
  c(
    QB = max(1, round(teams * (qb + 0.75 * sf) + 2)),
    RB = max(1, round(teams * (rb + 0.30 * flex + 0.08 * sf) + 3)),
    WR = max(1, round(teams * (wr + 0.55 * flex + 0.08 * sf) + 5)),
    TE = max(1, round(teams * (te + 0.15 * flex + 0.04 * sf) + 2))
  )
}

fm3_dynasty_tail_retention <- function(position, age) {
  pos <- toupper(as.character(position)); age <- suppressWarnings(as.numeric(age))
  dplyr::case_when(
    pos == "QB" & age <= 32 ~ 0.98,
    pos == "QB" & age <= 35 ~ 0.94,
    pos == "QB" ~ 0.86,
    pos == "RB" & age <= 25 ~ 0.92,
    pos == "RB" & age <= 27 ~ 0.85,
    pos == "RB" ~ 0.75,
    pos == "WR" & age <= 28 ~ 0.96,
    pos == "WR" & age <= 30 ~ 0.90,
    pos == "WR" ~ 0.82,
    pos == "TE" & age <= 29 ~ 0.96,
    pos == "TE" & age <= 31 ~ 0.90,
    pos == "TE" ~ 0.83,
    TRUE ~ 0.85
  )
}

fm3_dynasty_abs_survival <- function(position, age) {
  pos <- toupper(as.character(position)); age <- suppressWarnings(as.numeric(age))
  threshold <- dplyr::case_when(pos == "QB" ~ 32, pos == "RB" ~ 25, pos == "WR" ~ 28, pos == "TE" ~ 29, TRUE ~ 27)
  retention <- dplyr::case_when(pos == "QB" ~ 0.91, pos == "RB" ~ 0.80, pos == "WR" ~ 0.87, pos == "TE" ~ 0.88, TRUE ~ 0.85)
  retention ^ pmax(0, age - threshold)
}

fm3_build_dynasty_values <- function(settings, sleeper_players = NULL) {
  d <- fm3_model_player_frame()
  n <- nrow(d)

  num_col <- function(name, default = NA_real_) {
    if (!name %in% names(d)) return(rep(as.numeric(default), n))
    x <- suppressWarnings(as.numeric(d[[name]]))
    if (length(x) != n) x <- rep(as.numeric(default), n)
    x[!is.finite(x)] <- as.numeric(default)
    x
  }
  chr_col <- function(name, default = "") {
    if (!name %in% names(d)) return(rep(as.character(default), n))
    x <- as.character(d[[name]])
    if (length(x) != n) x <- rep(as.character(default), n)
    x[is.na(x) | !nzchar(x)] <- as.character(default)
    x
  }

  pos <- toupper(chr_col("position", ""))
  y1_raw <- num_col("year1_fppg", NA_real_)
  season_raw <- num_col("projected_fppg", NA_real_)
  y1 <- ifelse(is.finite(y1_raw), y1_raw, season_raw)
  y1[!is.finite(y1)] <- 0
  y3 <- num_col("year3_fppg", NA_real_)
  y3[!is.finite(y3)] <- y1[!is.finite(y3)]
  y3 <- pmax(0, y3)
  y2 <- pmax(0, (2 * y1 + y3) / 3)

  age <- num_col("age", NA_real_)
  fallback_age <- dplyr::case_when(pos == "QB" ~ 27, pos == "RB" ~ 25, pos == "WR" ~ 26, pos == "TE" ~ 27, TRUE ~ 27)
  age[!is.finite(age)] <- fallback_age[!is.finite(age)]

  replacement_ranks <- fm3_dynasty_replacement_ranks(settings)
  replacement <- rep(0, n)
  for (pp in c("QB","RB","WR","TE")) {
    idx <- which(pos == pp & is.finite(y1))
    if (!length(idx)) next
    vals <- sort(y1[idx], decreasing = TRUE)
    rr <- min(length(vals), as.integer(replacement_ranks[[pp]]))
    replacement[idx] <- if (rr > 0) vals[[rr]] else 0
  }

  horizon <- 8L
  proj <- matrix(0, nrow = n, ncol = horizon)
  proj[,1] <- pmax(0, y1); proj[,2] <- y2; proj[,3] <- y3
  if (horizon >= 4) for (yy in 4:horizon) {
    entering_age <- age + yy - 1
    proj[,yy] <- pmax(0, proj[,yy-1] * fm3_dynasty_tail_retention(pos, entering_age))
  }

  current_survival <- pmax(1e-6, fm3_dynasty_abs_survival(pos, age))
  survival <- matrix(1, nrow = n, ncol = horizon)
  for (yy in seq_len(horizon)) {
    survival[,yy] <- pmin(1, fm3_dynasty_abs_survival(pos, age + yy - 1) / current_survival)
  }
  discount <- 0.90 ^ (seq_len(horizon) - 1)
  weighted_games <- sweep(survival, 2, 17 * discount, `*`)
  surplus <- pmax(0, proj - replacement)
  career_surplus_points <- rowSums(surplus * weighted_games, na.rm = TRUE)
  career_projected_points <- rowSums(proj * weighted_games, na.rm = TRUE)
  # A small absolute-production term keeps useful veterans/depth differentiated
  # even when they sit near replacement. Surplus remains the dominant signal.
  career_signal <- career_surplus_points + 0.14 * career_projected_points

  career_pct <- fm3_percentile_rank(career_signal)
  prod_pct <- rep(0, n)
  for (pp in c("QB","RB","WR","TE")) {
    idx <- which(pos == pp)
    if (length(idx)) prod_pct[idx] <- fm3_percentile_rank(y1[idx])
  }
  elite_mult <- 1 +
    0.65 * pmax(0, (career_pct - 0.80) / 0.20) ^ 2 +
    0.25 * pmax(0, (prod_pct - 0.85) / 0.15) ^ 2

  format_mult <- fm3_format_premium(pos, settings)
  conf <- tolower(chr_col("confidence", "medium"))
  uncertainty <- dplyr::case_when(
    grepl("high", conf) ~ 1.02,
    grepl("low", conf) ~ 0.96,
    TRUE ~ 1
  )
  breakout <- pmax(0, pmin(1, num_col("breakout_probability", 0)))
  elite <- pmax(0, pmin(1, num_col("elite_probability", 0)))
  upside_mult <- 1 + 0.05 * breakout + 0.08 * elite

  score <- pmax(0, career_signal * elite_mult * format_mult * uncertainty * upside_mult)
  score_max <- if (length(score) && any(is.finite(score))) max(score, na.rm = TRUE) else 0
  scaled <- if (is.finite(score_max) && score_max > 0) DYNASTY_VALUE_MAX * (score / score_max) ^ 1.18 else rep(0, n)

  d[["year1_fppg"]] <- y1
  d[["year3_fppg"]] <- y3
  d[["age"]] <- age
  d[["dynasty_replacement_fppg"]] <- replacement
  d[["dynasty_year1_surplus"]] <- pmax(0, y1 - replacement)
  d[["dynasty_career_surplus_points"]] <- career_surplus_points
  d[["dynasty_career_projected_points"]] <- career_projected_points
  d[["dynasty_elite_multiplier"]] <- elite_mult
  d[["dynasty_format_multiplier"]] <- format_mult
  d[["dynasty_age_survival_year4"]] <- survival[,4]
  d[["dynasty_value_method"]] <- "career_surplus_v2"
  d[["model_dynasty_value"]] <- pmin(DYNASTY_VALUE_MAX, scaled)
  d <- d |>
    dplyr::arrange(dplyr::desc(model_dynasty_value)) |>
    dplyr::mutate(model_dynasty_rank = dplyr::row_number())

  # Sleeper supplies identity only; it does not set the player value.
  d[["sleeper_id"]] <- rep(NA_character_, nrow(d))
  if (!is.null(sleeper_players) && is.data.frame(sleeper_players) && nrow(sleeper_players)) {
    sleeper_players <- fm3_identity_sleeper_schema(sleeper_players)
    identity <- fm3_build_identity_map(sleeper_players, d)
    if (nrow(identity)) {
      id_join <- identity[, intersect(c("sleeper_id", "gsis_id"), names(identity)), drop = FALSE]
      if (all(c("sleeper_id", "gsis_id") %in% names(id_join))) {
        id_join[["sleeper_id"]] <- as.character(id_join[["sleeper_id"]])
        id_join[["gsis_id"]] <- as.character(id_join[["gsis_id"]])
        id_join <- id_join[nzchar(id_join[["gsis_id"]]), , drop = FALSE]
        id_join <- id_join[!duplicated(id_join[["gsis_id"]]), , drop = FALSE]
        d <- dplyr::left_join(d |> dplyr::select(-dplyr::any_of("sleeper_id")), id_join, by = "gsis_id")
      }
    }
  }

  d[["market_rank_proxy"]] <- rep(NA_real_, nrow(d))
  d[["market_value_proxy"]] <- rep(NA_real_, nrow(d))
  d[["model_market_gap"]] <- rep(NA_real_, nrow(d))
  d[["model_market_gap_pct"]] <- rep(NA_real_, nrow(d))
  d[["value_signal"]] <- rep("MODEL", nrow(d))

  market_path <- "data/external/dynasty_market_values.csv"
  if (file.exists(market_path) && "sleeper_id" %in% names(d)) {
    ext <- tryCatch(readr::read_csv(market_path, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(ext) && nrow(ext) && all(c("sleeper_id", "market_value") %in% names(ext))) {
      ext <- ext |>
        dplyr::transmute(sleeper_id = as.character(sleeper_id), external_market_value = suppressWarnings(as.numeric(market_value)),
               external_market_rank = if ("market_rank" %in% names(ext)) suppressWarnings(as.numeric(market_rank)) else NA_real_,
               market_source = if ("source" %in% names(ext)) as.character(source) else "external") |>
        dplyr::filter(nzchar(sleeper_id), is.finite(external_market_value)) |>
        dplyr::distinct(sleeper_id, .keep_all = TRUE)
      d <- d |> dplyr::left_join(ext, by = "sleeper_id") |>
        dplyr::mutate(
market_value_proxy = dplyr::if_else(is.finite(external_market_value), external_market_value, market_value_proxy),
market_rank_proxy = dplyr::if_else(is.finite(external_market_rank), external_market_rank, market_rank_proxy),
model_market_gap = dplyr::if_else(is.finite(market_value_proxy), model_dynasty_value - market_value_proxy, NA_real_),
model_market_gap_pct = dplyr::if_else(is.finite(market_value_proxy) & market_value_proxy > 0, model_market_gap / market_value_proxy, NA_real_),
value_signal = dplyr::case_when(
  is.finite(model_market_gap_pct) & model_market_gap_pct >= 0.12 ~ "BUY",
  is.finite(model_market_gap_pct) & model_market_gap_pct <= -0.12 ~ "SELL",
  is.finite(market_value_proxy) ~ "HOLD",
  TRUE ~ "MODEL"
)
        )
    }
  }
  d
}
fm3_rookie_pick_curve <- function(dynasty_values, teams = 12, rounds = 4, current_season = CURRENT_SEASON) {
  d <- dynasty_values
  if ("is_rookie" %in% names(d) && any(d$is_rookie %in% 1, na.rm = TRUE)) {
    rookie <- d |> dplyr::filter(is_rookie %in% 1)
  } else {
    rookie <- d |> dplyr::filter(!is.na(age), age <= 23, position %in% c("QB","RB","WR","TE"))
  }
  rookie <- rookie |> dplyr::arrange(dplyr::desc(model_dynasty_value))
  n_needed <- as.integer(teams * rounds)

  if (!nrow(rookie)) {
    vals <- DYNASTY_VALUE_MAX * 0.36 * exp(-0.080 * (seq_len(n_needed) - 1))
  } else {
    rv <- pmax(0, suppressWarnings(as.numeric(rookie$model_dynasty_value)))
    rv[!is.finite(rv)] <- 0
    if (!length(rv)) rv <- 0
    value_at <- function(i) {
      if (i <= length(rv)) return(rv[[i]])
      tail_start <- if (length(rv)) tail(rv, 1) else DYNASTY_VALUE_MAX * 0.18
      tail_start * 0.82 ^ (i - length(rv))
    }
    vals <- vapply(seq_len(n_needed), function(i) {
      rd <- ceiling(i / teams)
      slot <- value_at(i)
      near <- mean(vapply(i:(i + 3), value_at, numeric(1)))
      downside <- value_at(i + 8)
      expected_player <- 0.50 * slot + 0.35 * near + 0.15 * downside
      uncertainty <- c(`1` = 0.68, `2` = 0.58, `3` = 0.50, `4` = 0.44)[as.character(rd)]
      if (is.na(uncertainty)) uncertainty <- 0.40
      cap <- if (rd == 1) 0.62 * DYNASTY_VALUE_MAX else if (rd == 2) 0.35 * DYNASTY_VALUE_MAX else 0.22 * DYNASTY_VALUE_MAX
      min(cap, expected_player * uncertainty)
    }, numeric(1))
  }

  tibble::tibble(
    season = current_season,
    overall_pick = seq_len(n_needed),
    round = ceiling(overall_pick / teams),
    pick_in_round = ((overall_pick - 1) %% teams) + 1,
    pick_label = paste0(round, ".", sprintf("%02d", pick_in_round)),
    pick_value = pmax(350, vals),
    valuation_source = "rookie_expected_value_with_selection_uncertainty"
  )
}
fm3_future_pick_value <- function(pick_curve, season, round, expected_slot, current_season = CURRENT_SEASON) {
  years_out <- max(0, as.numeric(season) - current_season)
  round <- max(1, as.integer(round)); slot <- max(1, as.integer(base::round(expected_slot)))
  teams <- max(pick_curve$pick_in_round, na.rm = TRUE)
  slot <- min(teams, slot)
  idx <- which(pick_curve$round == round & pick_curve$pick_in_round == slot)
  base <- if (length(idx)) pick_curve$pick_value[idx[1]] else max(500, 3200 * 0.55 ^ (round - 1))
  base * DYNASTY_FUTURE_PICK_DISCOUNT ^ years_out
}

fm3_write_dynasty_value_outputs <- function(settings, sleeper_players = NULL) {
  d <- fm3_build_dynasty_values(settings, sleeper_players)
  teams <- suppressWarnings(as.integer(settings$teams %||% 12)); if (!is.finite(teams)) teams <- 12
  curve <- fm3_rookie_pick_curve(d, teams = teams)
  readr::write_csv(d, "output/dynasty_values_3_0.csv")
  readr::write_csv(curve, "output/dynasty_pick_curve_3_0.csv")
  invisible(list(values = d, pick_curve = curve))
}
