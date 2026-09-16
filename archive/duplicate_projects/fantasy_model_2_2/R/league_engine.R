# ============================================================
# FANTASY MODEL 2.0 - SAVED LEAGUES + STRUCTURED-STAT RANKING ENGINE
# ============================================================

BASE_LEAGUE_SETTINGS <- list(
  league_name = "My League",
  teams = 12,
  scoring_preset = "Half PPR",
  reception_points = 0.5,
  pass_td_points = 4,
  te_premium = 0,
  qb_starters = 1,
  rb_starters = 2,
  wr_starters = 2,
  te_starters = 1,
  flex_starters = 1,
  superflex_starters = 0
)

LEAGUE_PROFILE_PATH <- "settings/leagues.csv"

settings_to_row <- function(settings) {
  out <- as.data.frame(settings, stringsAsFactors = FALSE)
  out$league_name <- as.character(out$league_name)
  out
}

normalize_settings <- function(x) {
  out <- BASE_LEAGUE_SETTINGS
  if (is.null(x) || nrow(x) == 0) return(out)
  for (nm in names(out)) {
    if (!nm %in% names(x)) next
    value <- x[[nm]][1]
    if (is.numeric(out[[nm]])) {
      value <- suppressWarnings(as.numeric(value))
      if (is.finite(value)) out[[nm]] <- value
    } else if (!is.na(value) && nzchar(as.character(value))) {
      out[[nm]] <- as.character(value)
    }
  }
  out
}

read_league_profiles <- function(path = LEAGUE_PROFILE_PATH) {
  if (file.exists(path)) {
    x <- tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(x) && nrow(x) > 0 && "league_name" %in% names(x)) return(x)
  }

  # Migrate the original 1.0 single-profile file if it exists.
  legacy <- "settings/league_settings.csv"
  if (file.exists(legacy)) {
    x <- tryCatch(readr::read_csv(legacy, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(x) && all(c("setting", "value") %in% names(x))) {
      vals <- stats::setNames(as.character(x$value), x$setting)
      migrated <- BASE_LEAGUE_SETTINGS
      for (nm in names(migrated)) {
        if (!nm %in% names(vals)) next
        if (is.numeric(migrated[[nm]])) {
          v <- suppressWarnings(as.numeric(vals[[nm]])); if (is.finite(v)) migrated[[nm]] <- v
        } else migrated[[nm]] <- vals[[nm]]
      }
      out <- settings_to_row(migrated)
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      readr::write_csv(out, path)
      return(out)
    }
  }

  out <- settings_to_row(BASE_LEAGUE_SETTINGS)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(out, path)
  out
}

read_league_profile <- function(name = NULL, path = LEAGUE_PROFILE_PATH) {
  profiles <- read_league_profiles(path)
  if (is.null(name) || !nzchar(name) || !name %in% profiles$league_name) {
    return(normalize_settings(profiles[1, , drop = FALSE]))
  }
  normalize_settings(profiles[profiles$league_name == name, , drop = FALSE])
}

write_league_profile <- function(settings, path = LEAGUE_PROFILE_PATH) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  profiles <- read_league_profiles(path)
  row <- settings_to_row(settings)
  name <- as.character(settings$league_name)
  profiles <- profiles[profiles$league_name != name, , drop = FALSE]
  all_cols <- union(names(profiles), names(row))
  for (nm in setdiff(all_cols, names(profiles))) profiles[[nm]] <- NA
  for (nm in setdiff(all_cols, names(row))) row[[nm]] <- NA
  profiles <- dplyr::bind_rows(profiles[, all_cols, drop = FALSE], row[, all_cols, drop = FALSE])
  readr::write_csv(profiles, path)
  invisible(path)
}

delete_league_profile <- function(name, path = LEAGUE_PROFILE_PATH) {
  profiles <- read_league_profiles(path)
  if (nrow(profiles) <= 1) return(FALSE)
  profiles <- profiles[profiles$league_name != name, , drop = FALSE]
  readr::write_csv(profiles, path)
  TRUE
}

# Backward-compatible helpers used by older scripts.
read_league_settings <- function(path = "settings/league_settings.csv") {
  read_league_profile(NULL, LEAGUE_PROFILE_PATH)
}
write_league_settings <- function(settings, path = "settings/league_settings.csv") {
  write_league_profile(settings, LEAGUE_PROFILE_PATH)
}

apply_scoring_preset <- function(preset) {
  switch(
    preset,
    "Standard" = 0,
    "PPR" = 1,
    "Half PPR" = 0.5,
    0.5
  )
}

league_replacement_ranks <- function(settings) {
  n <- max(4, as.numeric(settings$teams))
  # Coefficients are anchored so a standard 12-team 1QB/2RB/2WR/1TE/1FLEX
  # league reproduces the validated baseline replacement ranks: QB14/RB30/WR36/TE14.
  qb <- round(n * (settings$qb_starters + 0.85 * settings$superflex_starters) + 2)
  rb <- round(n * (settings$rb_starters + 0.25 * settings$flex_starters) + 3)
  wr <- round(n * (settings$wr_starters + 0.50 * settings$flex_starters) + 6)
  te <- round(n * (settings$te_starters + 0.083333 * settings$flex_starters) + 1)
  c(QB = max(1, qb), RB = max(1, rb), WR = max(1, wr), TE = max(1, te))
}

add_scoring_proxies <- function(df) {
  needed <- c(
    "prior_pass_yd_pg", "prior_pass_attempts_pg", "prior_pass_td_rate",
    "prior_rush_yd_pg", "prior_carries_pg", "prior_rush_td_rate",
    "prior_rec_yd_pg", "prior_targets_pg", "prior_catch_rate", "prior_rec_td_rate",
    "prior_games", "is_rookie"
  )
  for (nm in needed) if (!nm %in% names(df)) df[[nm]] <- 0

  # 2.0 prefers the structured stat projections when available. If a player
  # falls back to the validated direct guardrail, prior-season scoring shape remains
  # the safe fallback for custom league translation.
  for (nm in c("projected_pass_yards_pg", "projected_pass_tds_pg", "projected_rush_yards_pg",
               "projected_rush_tds_pg", "projected_receptions_pg", "projected_rec_yards_pg",
               "projected_rec_tds_pg")) if (!nm %in% names(df)) df[[nm]] <- NA_real_
  if (!"component_weight" %in% names(df)) df$component_weight <- 0

  df <- df |>
    dplyr::mutate(
      .component_shape_weight = pmin(1, pmax(0, dplyr::coalesce(component_weight, 0))),
      scoring_pass_yd_pg = pmax(0, dplyr::if_else(is.finite(projected_pass_yards_pg), .component_shape_weight * projected_pass_yards_pg + (1 - .component_shape_weight) * prior_pass_yd_pg, prior_pass_yd_pg)),
      scoring_pass_td_pg = pmax(0, dplyr::if_else(is.finite(projected_pass_tds_pg), .component_shape_weight * projected_pass_tds_pg + (1 - .component_shape_weight) * (prior_pass_attempts_pg * prior_pass_td_rate), prior_pass_attempts_pg * prior_pass_td_rate)),
      scoring_rush_yd_pg = pmax(0, dplyr::if_else(is.finite(projected_rush_yards_pg), .component_shape_weight * projected_rush_yards_pg + (1 - .component_shape_weight) * prior_rush_yd_pg, prior_rush_yd_pg)),
      scoring_rush_td_pg = pmax(0, dplyr::if_else(is.finite(projected_rush_tds_pg), .component_shape_weight * projected_rush_tds_pg + (1 - .component_shape_weight) * (prior_carries_pg * prior_rush_td_rate), prior_carries_pg * prior_rush_td_rate)),
      scoring_receptions_pg = pmax(0, dplyr::if_else(is.finite(projected_receptions_pg), .component_shape_weight * projected_receptions_pg + (1 - .component_shape_weight) * (prior_targets_pg * prior_catch_rate), prior_targets_pg * prior_catch_rate)),
      scoring_rec_yd_pg = pmax(0, dplyr::if_else(is.finite(projected_rec_yards_pg), .component_shape_weight * projected_rec_yards_pg + (1 - .component_shape_weight) * prior_rec_yd_pg, prior_rec_yd_pg)),
      scoring_rec_td_pg = pmax(0, dplyr::if_else(is.finite(projected_rec_tds_pg), .component_shape_weight * projected_rec_tds_pg + (1 - .component_shape_weight) * (prior_targets_pg * prior_rec_td_rate), prior_targets_pg * prior_rec_td_rate))
    )

  # Rookies and players without a usable prior season receive a scoring-shape
  # proxy from similarly projected veterans at the same position. This changes
  # league-scoring translation only; it does not alter the 1.0 ML projection.
  proxy_cols <- c(
    "scoring_pass_yd_pg", "scoring_pass_td_pg", "scoring_rush_yd_pg",
    "scoring_rush_td_pg", "scoring_receptions_pg", "scoring_rec_yd_pg",
    "scoring_rec_td_pg"
  )
  needs_proxy <- which(df$is_rookie == 1 | df$prior_games <= 0)
  if (length(needs_proxy) > 0) {
    for (i in needs_proxy) {
      pool <- which(
        df$position == df$position[i] &
          df$is_rookie == 0 & df$prior_games > 0 & seq_len(nrow(df)) != i
      )
      if (length(pool) == 0) next
      dist <- abs(df$projected_fppg[pool] - df$projected_fppg[i])
      pool <- pool[order(dist)][seq_len(min(12, length(pool)))]
      for (nm in proxy_cols) {
        val <- mean(df[[nm]][pool], na.rm = TRUE)
        if (is.finite(val)) df[[nm]][i] <- val
      }
    }
  }
  df
}

apply_league_scoring <- function(df, settings) {
  df <- add_scoring_proxies(df)
  reception_points <- as.numeric(settings$reception_points)
  pass_td_points <- as.numeric(settings$pass_td_points)
  te_premium <- as.numeric(settings$te_premium)

  # The core direct model was validated in half-PPR, 4-point passing-TD scoring.
  # Translate only components we can estimate robustly from the feature table.
  reception_delta <- reception_points - 0.5
  pass_td_delta <- pass_td_points - 4

  df |>
    dplyr::mutate(
      league_scoring_delta =
        scoring_receptions_pg * reception_delta +
        scoring_pass_td_pg * pass_td_delta +
        dplyr::if_else(position == "TE", scoring_receptions_pg * te_premium, 0),
      league_projected_fppg = pmax(0, projected_fppg + league_scoring_delta),
      league_floor_fppg = pmax(0, projection_floor_fppg + league_scoring_delta),
      league_ceiling_fppg = pmax(0, projection_ceiling_fppg + league_scoring_delta),
      league_projected_points = league_projected_fppg * projected_games,
      league_full_17_points = league_projected_fppg * 17
    )
}

rank_for_league <- function(df, settings) {
  d <- apply_league_scoring(df, settings)
  repl_ranks <- league_replacement_ranks(settings)
  replacement <- lapply(names(repl_ranks), function(pos) {
    x <- d |>
      dplyr::filter(position == pos) |>
      dplyr::arrange(dplyr::desc(league_projected_fppg))
    r <- min(nrow(x), repl_ranks[[pos]])
    val <- if (r > 0) x$league_projected_fppg[r] else 0
    data.frame(position = pos, league_replacement_fppg = val)
  }) |>
    dplyr::bind_rows()

  d |>
    dplyr::left_join(replacement, by = "position") |>
    dplyr::mutate(
      league_vorp_fppg = league_projected_fppg - league_replacement_fppg,
      league_value = league_vorp_fppg * projected_games
    ) |>
    dplyr::group_by(position) |>
    dplyr::arrange(dplyr::desc(league_projected_fppg), dplyr::desc(elite_probability), .by_group = TRUE) |>
    dplyr::mutate(league_position_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(league_value), dplyr::desc(league_projected_fppg), dplyr::desc(upside_index)) |>
    dplyr::mutate(league_overall_rank = dplyr::row_number())
}

adjust_dynasty_for_league <- function(dynasty, redraft_league) {
  ratio <- redraft_league |>
    dplyr::transmute(
      player_id,
      scoring_ratio = dplyr::if_else(
        projected_fppg > 0,
        pmax(0.5, pmin(1.75, league_projected_fppg / projected_fppg)),
        1
      )
    ) |>
    dplyr::distinct(player_id, .keep_all = TRUE)

  dynasty |>
    dplyr::left_join(ratio, by = "player_id") |>
    dplyr::mutate(
      scoring_ratio = dplyr::coalesce(scoring_ratio, 1),
      league_dynasty_value = dynasty_value * scoring_ratio
    ) |>
    dplyr::arrange(dplyr::desc(league_dynasty_value)) |>
    dplyr::mutate(league_dynasty_rank = dplyr::row_number())
}
