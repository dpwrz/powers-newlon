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

fm3_build_dynasty_values <- function(settings, sleeper_players = NULL) {
  d <- fm3_model_player_frame()
  n <- nrow(d)

  # Always return vectors of exactly n rows.  This protects the Shiny analysis
  # layer from zero-length `$column` results when a portable/older artifact is
  # missing an optional field.
  num_col <- function(name, default = 0) {
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

  raw <- num_col("dynasty_value", 0)
  raw_max <- if (length(raw) && any(is.finite(raw))) max(raw, na.rm = TRUE) else 0
  base <- if (is.finite(raw_max) && raw_max > 0) DYNASTY_VALUE_MAX * (raw / raw_max) ^ 0.84 else rep(0, n)
  premium <- fm3_format_premium(chr_col("position", ""), settings)
  conf <- tolower(chr_col("confidence", "medium"))
  uncertainty <- dplyr::case_when(
    conf == "high" ~ 1.02,
    conf == "low" ~ 0.97,
    TRUE ~ 1
  )
  breakout <- num_col("breakout_probability", 0)
  elite <- num_col("elite_probability", 0)
  upside_mult <- 1 + pmin(0.08, 0.035 * breakout + 0.035 * elite)
  d$model_dynasty_value <- pmin(DYNASTY_VALUE_MAX, pmax(0, base * premium * uncertainty * upside_mult))
  d <- d |>
    dplyr::arrange(dplyr::desc(model_dynasty_value)) |>
    dplyr::mutate(model_dynasty_rank = dplyr::row_number())

  # 3.0.8 MODEL-FIRST IDENTITY ATTACHMENT
  # Sleeper supplies only roster/player identity. It does not contribute
  # projections, rankings, search-rank values, or calibrated player value.
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

  # No pseudo-market value is inferred from Sleeper. If the optional external
  # market file is absent, decisions are explicitly model-only.
  d[["market_rank_proxy"]] <- rep(NA_real_, nrow(d))
  d[["market_value_proxy"]] <- rep(NA_real_, nrow(d))
  d[["model_market_gap"]] <- rep(NA_real_, nrow(d))
  d[["model_market_gap_pct"]] <- rep(NA_real_, nrow(d))
  d[["value_signal"]] <- rep("MODEL", nrow(d))

  # Optional user-supplied market dataset. This intentionally does not scrape a
  # third-party calculator; place a licensed/exported dataset at the path below.
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
  n_needed <- teams * rounds
  if (!nrow(rookie)) {
    vals <- DYNASTY_VALUE_MAX * exp(-0.065 * (seq_len(n_needed) - 1))
  } else {
    vals <- rookie$model_dynasty_value
    if (length(vals) < n_needed) {
      tail_start <- if (length(vals)) tail(vals, 1) else 4500
      vals <- c(vals, tail_start * 0.89 ^ seq_len(n_needed - length(vals)))
    }
    vals <- vals[seq_len(n_needed)]
  }
  tibble::tibble(
    season = current_season,
    overall_pick = seq_len(n_needed),
    round = ceiling(overall_pick / teams),
    pick_in_round = ((overall_pick - 1) %% teams) + 1,
    pick_label = paste0(round, ".", sprintf("%02d", pick_in_round)),
    pick_value = pmax(500, vals)
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
