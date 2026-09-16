# ============================================================
# STEP 2B - FANTASY MODEL 2.0 OPPORTUNITY / SHRINKAGE DATA STORE
# ============================================================
cat("[2.0 HOTFIX 3] Opportunity builder loaded: raw-stat RB carry-share reconstruction + deduplicated competition joins.\n")
cat("[2.0 DATA] Building opportunity-first feature store...\n")
source("config.R")
source("R/opportunity_engine.R")
ensure_packages(c("dplyr", "readr", "tidyr", "janitor", "rpart"))

model_path <- "data/processed/model_table.csv"
projection_path <- paste0("data/processed/projection_table_", CURRENT_SEASON, ".csv")
if (!file.exists(model_path) || !file.exists(projection_path)) {
  stop("Validated 1.2 feature tables are missing. Run the 1.2 feature build first.")
}

read20 <- function(path) suppressWarnings(readr::read_csv(path, show_col_types = FALSE, progress = FALSE, lazy = FALSE))
read_selected20 <- function(path, wanted) {
  if (!file.exists(path)) return(data.frame())
  header <- suppressWarnings(readr::read_csv(path, n_max = 0, show_col_types = FALSE, progress = FALSE))
  keep <- intersect(wanted, names(header))
  if (length(keep) == 0) return(data.frame())
  suppressWarnings(readr::read_csv(path, col_select = tidyselect::all_of(keep), show_col_types = FALSE, progress = FALSE, lazy = FALSE))
}
canonical_team20 <- function(df, target = "team", aliases = c("team", "recent_team", "team_abbr", "posteam", "latest_team")) {
  if (!target %in% names(df)) {
    hit <- aliases[aliases %in% names(df)]
    df[[target]] <- if (length(hit) > 0) df[[hit[1]]] else rep(NA_character_, nrow(df))
  }
  x <- trimws(toupper(as.character(df[[target]])))
  x[x %in% c("", "NA", "N/A", "NULL", "0")] <- NA_character_
  df[[target]] <- x
  df
}
canonical_id20 <- function(df, target = "player_id", aliases = c("player_id", "gsis_id")) {
  if (!target %in% names(df)) {
    hit <- aliases[aliases %in% names(df)]
    df[[target]] <- if (length(hit) > 0) df[[hit[1]]] else rep(NA_character_, nrow(df))
  }
  df[[target]] <- as.character(df[[target]])
  df
}
sum_top_n20 <- function(x, n = 2) {
  x <- sort(num20(x)[is.finite(num20(x))], decreasing = TRUE)
  if (length(x) == 0) 0 else sum(utils::head(x, n))
}

model <- read20(model_path) |> janitor::clean_names() |> canonical_team20() |> canonical_id20()
projection <- read20(projection_path) |> janitor::clean_names() |>
  canonical_team20("current_team", c("current_team", "team", "recent_team", "team_abbr", "latest_team")) |>
  canonical_id20()

team_raw <- read_selected20(
  "data/raw/team_stats.csv",
  c("season", "team", "team_abbr", "recent_team", "attempts", "carries", "passing_yards", "rushing_yards", "passing_tds", "rushing_tds")
) |> janitor::clean_names() |> canonical_team20()
player_raw <- read_selected20(
  "data/raw/player_stats.csv",
  c("season", "team", "recent_team", "team_abbr", "posteam", "player_id", "gsis_id", "position", "targets", "carries")
) |> janitor::clean_names() |> canonical_team20() |> canonical_id20()
team_context <- if (file.exists("data/raw/team_context.csv")) read20("data/raw/team_context.csv") |> janitor::clean_names() |> canonical_team20() else data.frame()

# Required model-table actuals. These should exist in the validated 1.2 store,
# but explicit defaults make older caches fail safely rather than during joins.
actual_cols <- c(
  "season", "games", "attempts", "carries", "targets", "receptions",
  "passing_yards", "passing_tds", "interceptions",
  "rushing_yards", "rushing_tds", "receiving_yards", "receiving_tds",
  "target_share", "target_fppg"
)
for (nm in actual_cols) model <- ensure20(model, nm, 0)
model$season <- as.integer(model$season)

# ------------------------------------------------------------
# Team volume model: predict the size of the pass/rush opportunity pie.
# ------------------------------------------------------------
for (nm in c("season", "attempts", "carries", "passing_yards", "rushing_yards", "passing_tds", "rushing_tds")) team_raw <- ensure20(team_raw, nm, 0)
team_actual <- team_raw |>
  dplyr::mutate(
    season = as.integer(season), team = as.character(team),
    games = dplyr::if_else(season >= 2021, 17, 16),
    team_pass_attempts_pg = safe_div20(attempts, games),
    team_carries_pg = safe_div20(carries, games),
    team_pass_yd_pg = safe_div20(passing_yards, games),
    team_rush_yd_pg = safe_div20(rushing_yards, games),
    team_points_proxy_pg = safe_div20(
      num20(passing_yards) * SCORING$pass_yd + num20(passing_tds) * SCORING$pass_td +
        num20(rushing_yards) * SCORING$rush_yd + num20(rushing_tds) * SCORING$rush_td,
      games
    ),
    team_attempts_total = num20(attempts),
    team_carries_total = num20(carries)
  ) |>
  dplyr::filter(!is.na(team), team != "") |>
  dplyr::group_by(season, team) |>
  dplyr::summarise(
    dplyr::across(c(team_pass_attempts_pg, team_carries_pg, team_pass_yd_pg, team_rush_yd_pg,
                    team_points_proxy_pg, team_attempts_total, team_carries_total), ~ max(.x, na.rm = TRUE)),
    .groups = "drop"
  )

team_model <- team_actual |>
  dplyr::arrange(team, season) |>
  dplyr::group_by(team) |>
  dplyr::mutate(
    prior_season = dplyr::lag(season),
    team_prior_pass_attempts_pg = dplyr::lag(team_pass_attempts_pg),
    team_prior_carries_pg = dplyr::lag(team_carries_pg),
    team_prior_pass_yd_pg = dplyr::lag(team_pass_yd_pg),
    team_prior_rush_yd_pg = dplyr::lag(team_rush_yd_pg),
    team_prior_points_pg = dplyr::lag(team_points_proxy_pg),
    target_team_pass_attempts_pg = team_pass_attempts_pg,
    target_team_carries_pg = team_carries_pg
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(prior_season), prior_season == season - 1)

team_volume_features20 <- c(
  "team_prior_pass_attempts_pg", "team_prior_carries_pg", "team_prior_pass_yd_pg",
  "team_prior_rush_yd_pg", "team_prior_points_pg", "team_prior_pass_rate",
  "team_prior_neutral_pass_rate", "team_prior_plays_pg", "team_prior_redzone_pass_rate",
  "team_prior_redzone_rush_rate", "team_prior_deep_throw_rate", "team_prior_shotgun_rate",
  "team_prior_no_huddle_rate", "team_prior_avg_air_yards"
)

if (nrow(team_context) > 0 && all(c("season", "team") %in% names(team_context))) {
  for (nm in c("team_pass_rate", "team_neutral_pass_rate", "team_plays_pg", "team_redzone_pass_rate",
               "team_deep_throw_rate", "team_shotgun_rate", "team_no_huddle_rate", "team_avg_air_yards")) {
    team_context <- ensure20(team_context, nm, 0)
  }
  tc <- team_context |>
    dplyr::mutate(season = as.integer(season) + 1, team = as.character(team)) |>
    dplyr::transmute(
      season, team,
      team_prior_pass_rate = num20(team_pass_rate),
      team_prior_neutral_pass_rate = num20(team_neutral_pass_rate),
      team_prior_plays_pg = num20(team_plays_pg),
      team_prior_redzone_pass_rate = num20(team_redzone_pass_rate),
      team_prior_redzone_rush_rate = pmax(0, 1 - num20(team_redzone_pass_rate)),
      team_prior_deep_throw_rate = num20(team_deep_throw_rate),
      team_prior_shotgun_rate = num20(team_shotgun_rate),
      team_prior_no_huddle_rate = num20(team_no_huddle_rate),
      team_prior_avg_air_yards = num20(team_avg_air_yards)
    ) |>
    dplyr::distinct(season, team, .keep_all = TRUE)
  team_model <- team_model |> dplyr::left_join(tc, by = c("season", "team"))
}
for (nm in team_volume_features20) team_model <- ensure20(team_model, nm, 0)
team_model <- team_model |> dplyr::mutate(dplyr::across(dplyr::all_of(team_volume_features20), ~ tidyr::replace_na(num20(.x), 0)))
readr::write_csv(team_model, "data/processed/team_opportunity_model_table.csv")

team_oof_rows <- list()
for (yr in sort(unique(team_model$season))) {
  tr <- team_model |> dplyr::filter(season < yr)
  te <- team_model |> dplyr::filter(season == yr)
  if (nrow(te) == 0) next
  feats <- intersect(team_volume_features20, names(team_model))
  if (nrow(tr) >= 64) {
    pass_fit <- fit_fantasy_model(tr, feats, "target_team_pass_attempts_pg", seed = SEED + yr + 51000, n_trees = TEAM_VOLUME_VALIDATION_TREES_20)
    carry_fit <- fit_fantasy_model(tr, feats, "target_team_carries_pg", seed = SEED + yr + 52000, n_trees = TEAM_VOLUME_VALIDATION_TREES_20)
    pass_pred <- clip20(predict_fantasy_model(pass_fit, te), 20, 48)
    carry_pred <- clip20(predict_fantasy_model(carry_fit, te), 18, 38)
  } else {
    pass_pred <- clip20(te$team_prior_pass_attempts_pg, 20, 48)
    carry_pred <- clip20(te$team_prior_carries_pg, 18, 38)
  }
  team_oof_rows[[length(team_oof_rows) + 1]] <- data.frame(
    season = te$season, team = te$team,
    team_projected_pass_attempts_pg = pass_pred,
    team_projected_carries_pg = carry_pred
  )
}
team_oof <- dplyr::bind_rows(team_oof_rows)
readr::write_csv(team_oof, "data/processed/team_volume_oof_2_0.csv")

latest_team <- team_actual |>
  dplyr::filter(season == TRAIN_END) |>
  dplyr::transmute(
    season = CURRENT_SEASON, team,
    team_prior_pass_attempts_pg = team_pass_attempts_pg,
    team_prior_carries_pg = team_carries_pg,
    team_prior_pass_yd_pg = team_pass_yd_pg,
    team_prior_rush_yd_pg = team_rush_yd_pg,
    team_prior_points_pg = team_points_proxy_pg
  )
if (nrow(team_context) > 0) {
  tc_current <- team_context |>
    dplyr::filter(as.integer(season) == TRAIN_END) |>
    dplyr::transmute(
      team = as.character(team),
      team_prior_pass_rate = num20(team_pass_rate),
      team_prior_neutral_pass_rate = num20(team_neutral_pass_rate),
      team_prior_plays_pg = num20(team_plays_pg),
      team_prior_redzone_pass_rate = num20(team_redzone_pass_rate),
      team_prior_redzone_rush_rate = pmax(0, 1 - num20(team_redzone_pass_rate)),
      team_prior_deep_throw_rate = num20(team_deep_throw_rate),
      team_prior_shotgun_rate = num20(team_shotgun_rate),
      team_prior_no_huddle_rate = num20(team_no_huddle_rate),
      team_prior_avg_air_yards = num20(team_avg_air_yards)
    ) |>
    dplyr::distinct(team, .keep_all = TRUE)
  latest_team <- latest_team |> dplyr::left_join(tc_current, by = "team")
}
for (nm in team_volume_features20) latest_team <- ensure20(latest_team, nm, 0)
latest_team <- latest_team |> dplyr::mutate(dplyr::across(dplyr::all_of(team_volume_features20), ~ tidyr::replace_na(num20(.x), 0)))
if (nrow(team_model) >= 64 && nrow(latest_team) > 0) {
  feats <- intersect(team_volume_features20, names(team_model))
  final_pass <- fit_fantasy_model(team_model, feats, "target_team_pass_attempts_pg", seed = SEED + 61000, n_trees = TEAM_VOLUME_FINAL_TREES_20)
  final_carry <- fit_fantasy_model(team_model, feats, "target_team_carries_pg", seed = SEED + 62000, n_trees = TEAM_VOLUME_FINAL_TREES_20)
  latest_team$team_projected_pass_attempts_pg <- clip20(predict_fantasy_model(final_pass, latest_team), 20, 48)
  latest_team$team_projected_carries_pg <- clip20(predict_fantasy_model(final_carry, latest_team), 18, 38)
  saveRDS(final_pass, "models/team_pass_volume_model_2_0.rds")
  saveRDS(final_carry, "models/team_carry_volume_model_2_0.rds")
} else {
  latest_team$team_projected_pass_attempts_pg <- clip20(latest_team$team_prior_pass_attempts_pg, 20, 48)
  latest_team$team_projected_carries_pg <- clip20(latest_team$team_prior_carries_pg, 18, 38)
}
readr::write_csv(latest_team, paste0("data/processed/team_projection_table_2_0_", CURRENT_SEASON, ".csv"))

# ------------------------------------------------------------
# Teammate competition + robust usage targets from raw player/team totals.
# ------------------------------------------------------------
# HOTFIX 3: historical RB carry share is reconstructed from raw nflverse
# player/team totals rather than model_table$team. Older 1.x caches may carry
# an unusable team field even when the player-season production columns are valid.
for (nm in c("season", "position", "targets", "carries")) player_raw <- ensure20(player_raw, nm, 0)

player_usage_by_team <- player_raw |>
  dplyr::mutate(
    season = as.integer(season),
    team = as.character(team),
    player_id = as.character(player_id),
    position = toupper(as.character(position)),
    targets = num20(targets),
    carries = num20(carries)
  ) |>
  dplyr::filter(position %in% c("RB", "WR", "TE"), !is.na(team), team != "", !is.na(player_id), player_id != "") |>
  dplyr::group_by(season, team, player_id) |>
  dplyr::summarise(
    targets = sum(targets, na.rm = TRUE),
    carries = sum(carries, na.rm = TRUE),
    .groups = "drop"
  )

player_team_totals20 <- player_usage_by_team |>
  dplyr::group_by(season, team) |>
  dplyr::summarise(
    player_sum_targets = sum(targets, na.rm = TRUE),
    player_sum_carries = sum(carries, na.rm = TRUE),
    .groups = "drop"
  )

player_usage_by_team <- player_usage_by_team |>
  dplyr::left_join(
    team_actual |> dplyr::select(season, team, team_attempts_total, team_carries_total),
    by = c("season", "team"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    player_team_totals20,
    by = c("season", "team"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    usage_team_attempts = dplyr::if_else(
      is.finite(num20(team_attempts_total)) & num20(team_attempts_total) > 0,
      num20(team_attempts_total),
      num20(player_sum_targets)
    ),
    usage_team_carries = dplyr::if_else(
      is.finite(num20(team_carries_total)) & num20(team_carries_total) > 0,
      num20(team_carries_total),
      num20(player_sum_carries)
    ),
    usage_target_share = clip20(safe_div20(targets, usage_team_attempts), 0, 1),
    usage_carry_share = clip20(safe_div20(carries, usage_team_carries), 0, 1)
  )

# One row per player-season. For traded players, weight each team-stint share by
# the player's own opportunity in that stint.
player_usage_targets20 <- player_usage_by_team |>
  dplyr::group_by(season, player_id) |>
  dplyr::summarise(
    raw_target_share_20 = if (sum(targets, na.rm = TRUE) > 0) {
      weighted_mean_safe20(usage_target_share, pmax(targets, 1))
    } else 0,
    raw_carry_share_20 = if (sum(carries, na.rm = TRUE) > 0) {
      weighted_mean_safe20(usage_carry_share, pmax(carries, 1))
    } else 0,
    .groups = "drop"
  ) |>
  dplyr::distinct(season, player_id, .keep_all = TRUE)

prior_usage <- player_usage_by_team |>
  dplyr::mutate(target_season = season + 1L, teammate_id = player_id) |>
  dplyr::select(target_season, team, teammate_id, usage_target_share, usage_carry_share) |>
  dplyr::distinct(target_season, team, teammate_id, .keep_all = TRUE)

build_competition20 <- function(keys, team_col) {
  names(keys)[names(keys) == team_col] <- "team"
  keys <- keys |>
    dplyr::mutate(
      season = as.integer(season),
      player_id = as.character(player_id),
      team = as.character(team)
    ) |>
    dplyr::distinct(season, player_id, team, .keep_all = TRUE)

  if (nrow(prior_usage) == 0) {
    return(keys |> dplyr::mutate(
      prior_teammate_max_target_share = 0, prior_teammate_top2_target_share = 0, prior_teammate_target_hhi = 0,
      prior_teammate_max_carry_share = 0, prior_teammate_top2_carry_share = 0, prior_teammate_carry_hhi = 0
    ))
  }

  usage <- prior_usage |>
    dplyr::mutate(
      target_season = as.integer(target_season),
      team = as.character(team),
      teammate_id = as.character(teammate_id)
    ) |>
    dplyr::distinct(target_season, team, teammate_id, .keep_all = TRUE)

  # Intentional many-to-many comparison of each player with teammates; collapse
  # immediately to one player-season row.
  keys |>
    dplyr::left_join(
      usage,
      by = c("season" = "target_season", "team"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(is.na(teammate_id) | teammate_id != player_id) |>
    dplyr::group_by(season, player_id, team) |>
    dplyr::summarise(
      prior_teammate_max_target_share = ifelse(all(is.na(usage_target_share)), 0, max(usage_target_share, na.rm = TRUE)),
      prior_teammate_top2_target_share = sum_top_n20(usage_target_share, 2),
      prior_teammate_target_hhi = sum(num20(usage_target_share)^2, na.rm = TRUE),
      prior_teammate_max_carry_share = ifelse(all(is.na(usage_carry_share)), 0, max(usage_carry_share, na.rm = TRUE)),
      prior_teammate_top2_carry_share = sum_top_n20(usage_carry_share, 2),
      prior_teammate_carry_hhi = sum(num20(usage_carry_share)^2, na.rm = TRUE),
      .groups = "drop"
    )
}
model_comp <- build_competition20(model |> dplyr::select(season, player_id, team) |> dplyr::distinct(), "team")
proj_comp <- build_competition20(projection |> dplyr::transmute(season = CURRENT_SEASON, player_id, current_team) |> dplyr::distinct(), "current_team")
rm(player_raw, prior_usage, player_usage_by_team, player_team_totals20)
invisible(gc(full = TRUE))

# ------------------------------------------------------------
# Career-before-season efficiency. This is the stable player prior
# used by empirical-Bayes shrinkage.
# ------------------------------------------------------------
for (nm in c("attempts", "carries", "targets", "receptions", "passing_yards", "interceptions",
             "rushing_yards", "receiving_yards", "passing_tds", "rushing_tds", "receiving_tds")) {
  model <- ensure20(model, nm, 0)
  model[[nm]] <- num20(model[[nm]])
}
model <- model |>
  dplyr::arrange(player_id, season) |>
  dplyr::group_by(player_id) |>
  dplyr::mutate(
    career_attempts_before = dplyr::lag(cumsum(attempts), default = 0),
    career_carries_before = dplyr::lag(cumsum(carries), default = 0),
    career_targets_before = dplyr::lag(cumsum(targets), default = 0),
    career_completions_before = dplyr::lag(cumsum(receptions), default = 0),
    career_pass_yards_before = dplyr::lag(cumsum(passing_yards), default = 0),
    career_interceptions_before = dplyr::lag(cumsum(interceptions), default = 0),
    career_rush_yards_before = dplyr::lag(cumsum(rushing_yards), default = 0),
    career_rec_yards_before = dplyr::lag(cumsum(receiving_yards), default = 0),
    career_pass_tds_before = dplyr::lag(cumsum(passing_tds), default = 0),
    career_rush_tds_before = dplyr::lag(cumsum(rushing_tds), default = 0),
    career_rec_tds_before = dplyr::lag(cumsum(receiving_tds), default = 0),
    prior_int_rate_raw20 = dplyr::lag(safe_div20(interceptions, attempts), default = 0),
    prior_actual_season20 = dplyr::lag(season)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    career_yards_per_attempt = safe_div20(career_pass_yards_before, career_attempts_before),
    career_interception_rate = safe_div20(career_interceptions_before, career_attempts_before),
    career_yards_per_carry = safe_div20(career_rush_yards_before, career_carries_before),
    career_catch_rate = safe_div20(career_completions_before, career_targets_before),
    career_yards_per_target = safe_div20(career_rec_yards_before, career_targets_before),
    career_pass_td_rate = safe_div20(career_pass_tds_before, career_attempts_before),
    career_rush_td_rate = safe_div20(career_rush_tds_before, career_carries_before),
    career_rec_td_rate = safe_div20(career_rec_tds_before, career_targets_before),
    prior_interception_rate = ifelse(prior_actual_season20 == season - 1, prior_int_rate_raw20, 0),
    prior_attempts_n = pmax(0, prior_pass_attempts_pg * prior_games),
    prior_carries_n = pmax(0, prior_carries_pg * prior_games),
    prior_targets_n = pmax(0, prior_targets_pg * prior_games)
  )

# Current projection career histories through TRAIN_END.
career_latest <- model |>
  dplyr::filter(season <= TRAIN_END) |>
  dplyr::group_by(player_id) |>
  dplyr::summarise(
    career_attempts_before = sum(attempts, na.rm = TRUE),
    career_carries_before = sum(carries, na.rm = TRUE),
    career_targets_before = sum(targets, na.rm = TRUE),
    career_pass_yards_before = sum(passing_yards, na.rm = TRUE),
    career_interceptions_before = sum(interceptions, na.rm = TRUE),
    career_rush_yards_before = sum(rushing_yards, na.rm = TRUE),
    career_receptions_before20 = sum(receptions, na.rm = TRUE),
    career_rec_yards_before = sum(receiving_yards, na.rm = TRUE),
    career_pass_tds_before = sum(passing_tds, na.rm = TRUE),
    career_rush_tds_before = sum(rushing_tds, na.rm = TRUE),
    career_rec_tds_before = sum(receiving_tds, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    career_yards_per_attempt = safe_div20(career_pass_yards_before, career_attempts_before),
    career_interception_rate = safe_div20(career_interceptions_before, career_attempts_before),
    career_yards_per_carry = safe_div20(career_rush_yards_before, career_carries_before),
    career_catch_rate = safe_div20(career_receptions_before20, career_targets_before),
    career_yards_per_target = safe_div20(career_rec_yards_before, career_targets_before),
    career_pass_td_rate = safe_div20(career_pass_tds_before, career_attempts_before),
    career_rush_td_rate = safe_div20(career_rush_tds_before, career_carries_before),
    career_rec_td_rate = safe_div20(career_rec_tds_before, career_targets_before)
  ) |>
  dplyr::select(-career_receptions_before20)

latest_int <- model |>
  dplyr::filter(season == TRAIN_END) |>
  dplyr::transmute(player_id, prior_interception_rate = safe_div20(interceptions, attempts)) |>
  dplyr::distinct(player_id, .keep_all = TRUE)

# ------------------------------------------------------------
# Target construction and opportunity/context joins.
# ------------------------------------------------------------
model <- model |>
  dplyr::left_join(player_usage_targets20, by = c("season", "player_id"), relationship = "many-to-one") |>
  dplyr::left_join(
    team_actual |> dplyr::select(season, team, team_attempts_total, team_carries_total),
    by = c("season", "team"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(team_oof, by = c("season", "team"), relationship = "many-to-one") |>
  dplyr::left_join(model_comp, by = c("season", "player_id", "team"), relationship = "many-to-one") |>
  dplyr::mutate(
    team_projected_pass_attempts_pg = dplyr::coalesce(team_projected_pass_attempts_pg, team_prior_pass_attempts_pg),
    team_projected_carries_pg = dplyr::coalesce(team_projected_carries_pg, team_prior_carries_pg),
    team_prior_redzone_rush_rate = dplyr::coalesce(num20(team_prior_redzone_rush_rate), pmax(0, 1 - num20(team_prior_redzone_pass_rate))),
    target_attempts_n = pmax(0, num20(attempts)),
    target_carries_n = pmax(0, num20(carries)),
    target_targets_n = pmax(0, num20(targets)),
    target_pass_attempts_pg = safe_div20(attempts, games),
    target_carries_pg = safe_div20(carries, games),
    target_target_share = clip20(
      dplyr::coalesce(
        dplyr::na_if(num20(target_share), 0),
        dplyr::na_if(num20(raw_target_share_20), 0),
        safe_div20(targets, team_attempts_total)
      ), 0, 1
    ),
    target_carry_share = clip20(
      dplyr::coalesce(
        dplyr::na_if(num20(raw_carry_share_20), 0),
        safe_div20(carries, team_carries_total)
      ), 0, 1
    ),
    target_catch_rate = ifelse(target_targets_n >= MIN_TARGETS_EFFICIENCY, safe_div20(receptions, targets), NA_real_),
    target_yards_per_target = ifelse(target_targets_n >= MIN_TARGETS_EFFICIENCY, safe_div20(receiving_yards, targets), NA_real_),
    target_yards_per_carry = ifelse(target_carries_n >= MIN_CARRIES_EFFICIENCY, safe_div20(rushing_yards, carries), NA_real_),
    target_yards_per_attempt = ifelse(target_attempts_n >= MIN_ATTEMPTS_EFFICIENCY, safe_div20(passing_yards, attempts), NA_real_),
    target_interception_rate = ifelse(target_attempts_n >= MIN_ATTEMPTS_EFFICIENCY, safe_div20(interceptions, attempts), NA_real_),
    target_pass_td_rate = ifelse(target_attempts_n >= MIN_ATTEMPTS_EFFICIENCY, safe_div20(passing_tds, attempts), NA_real_),
    target_rush_td_rate = ifelse(target_carries_n >= MIN_CARRIES_EFFICIENCY, safe_div20(rushing_tds, carries), NA_real_),
    target_rec_td_rate = ifelse(target_targets_n >= MIN_TARGETS_EFFICIENCY, safe_div20(receiving_tds, targets), NA_real_),
    target_competition_index = pmax(0, tidyr::replace_na(prior_teammate_top2_target_share, 0)),
    carry_competition_index = pmax(0, tidyr::replace_na(prior_teammate_top2_carry_share, 0)),
    team_pass_opportunity_index = pmax(0, team_projected_pass_attempts_pg * pmax(prior_target_share, 0.01)),
    team_rush_opportunity_index = pmax(0, team_projected_carries_pg * pmax(safe_div20(prior_carries_pg, team_prior_carries_pg), 0.01))
  )

projection <- projection |>
  dplyr::left_join(latest_team |> dplyr::select(team, team_projected_pass_attempts_pg, team_projected_carries_pg), by = c("current_team" = "team")) |>
  dplyr::left_join(proj_comp |> dplyr::select(-season), by = c("player_id", "current_team" = "team")) |>
  dplyr::left_join(career_latest, by = "player_id") |>
  dplyr::left_join(latest_int, by = "player_id") |>
  dplyr::mutate(
    team_projected_pass_attempts_pg = dplyr::coalesce(team_projected_pass_attempts_pg, team_prior_pass_attempts_pg),
    team_projected_carries_pg = dplyr::coalesce(team_projected_carries_pg, team_prior_carries_pg),
    team_prior_redzone_rush_rate = dplyr::coalesce(num20(team_prior_redzone_rush_rate), pmax(0, 1 - num20(team_prior_redzone_pass_rate))),
    prior_attempts_n = pmax(0, prior_pass_attempts_pg * prior_games),
    prior_carries_n = pmax(0, prior_carries_pg * prior_games),
    prior_targets_n = pmax(0, prior_targets_pg * prior_games),
    target_competition_index = pmax(0, tidyr::replace_na(prior_teammate_top2_target_share, 0)),
    carry_competition_index = pmax(0, tidyr::replace_na(prior_teammate_top2_carry_share, 0)),
    team_pass_opportunity_index = pmax(0, team_projected_pass_attempts_pg * pmax(prior_target_share, 0.01)),
    team_rush_opportunity_index = pmax(0, team_projected_carries_pg * pmax(safe_div20(prior_carries_pg, team_prior_carries_pg), 0.01))
  )

# Rookies / missing-career players receive zeros here; the shrinkage engine will
# move those zeros to position means because their exposure counts are zero.
rate_cols <- c(
  "career_attempts_before", "career_carries_before", "career_targets_before",
  "career_yards_per_attempt", "career_interception_rate", "career_yards_per_carry",
  "career_catch_rate", "career_yards_per_target", "career_pass_td_rate",
  "career_rush_td_rate", "career_rec_td_rate", "prior_interception_rate"
)
for (nm in rate_cols) {
  model <- ensure20(model, nm, 0); projection <- ensure20(projection, nm, 0)
  model[[nm]][!is.finite(num20(model[[nm]]))] <- 0
  projection[[nm]][!is.finite(num20(projection[[nm]]))] <- 0
}
for (nm in OPPORTUNITY_EXTRA_FEATURES_20) {
  model <- ensure20(model, nm, 0); projection <- ensure20(projection, nm, 0)
  model[[nm]][!is.finite(num20(model[[nm]]))] <- 0
  projection[[nm]][!is.finite(num20(projection[[nm]]))] <- 0
}

# Guardrail diagnostics for the two targets that were suspicious in 1.3.
rb_carry <- model |> dplyr::filter(position == "RB", is.finite(target_carry_share))
qb_int <- model |> dplyr::filter(position == "QB", is.finite(target_interception_rate))
rb_sd20 <- if (nrow(rb_carry) > 1) stats::sd(rb_carry$target_carry_share, na.rm = TRUE) else NA_real_
qb_sd20 <- if (nrow(qb_int) > 1) stats::sd(qb_int$target_interception_rate, na.rm = TRUE) else NA_real_
rb_unique20 <- dplyr::n_distinct(round(num20(rb_carry$target_carry_share), 6), na.rm = TRUE)
qb_unique20 <- dplyr::n_distinct(round(num20(qb_int$target_interception_rate), 6), na.rm = TRUE)

if (nrow(rb_carry) > 20 && (!is.finite(rb_sd20) || rb_sd20 < 1e-6 || rb_unique20 < 3)) {
  stop(
    "2.0 target guardrail: RB carry share is unexpectedly constant after raw-stat reconstruction. ",
    "rows=", nrow(rb_carry), ", unique=", rb_unique20,
    ", min=", sprintf("%.4f", min(rb_carry$target_carry_share, na.rm = TRUE)),
    ", max=", sprintf("%.4f", max(rb_carry$target_carry_share, na.rm = TRUE)),
    ". Check raw player/team carry totals."
  )
}
if (nrow(qb_int) > 20 && (!is.finite(qb_sd20) || qb_sd20 < 1e-6 || qb_unique20 < 3)) {
  stop(
    "2.0 target guardrail: QB interception rate is unexpectedly constant. ",
    "rows=", nrow(qb_int), ", unique=", qb_unique20,
    ", min=", sprintf("%.4f", min(qb_int$target_interception_rate, na.rm = TRUE)),
    ", max=", sprintf("%.4f", max(qb_int$target_interception_rate, na.rm = TRUE)),
    "."
  )
}

readr::write_csv(model, "data/processed/opportunity_model_table_2_0.csv")
readr::write_csv(projection, paste0("data/processed/opportunity_projection_table_2_0_", CURRENT_SEASON, ".csv"))

cat("[2.0 DATA] Training rows: ", nrow(model), "\n", sep = "")
cat("[2.0 DATA] Projection candidates: ", nrow(projection), "\n", sep = "")
cat("[2.0 DATA] RB carry-share SD: ", sprintf("%.4f", rb_sd20), " | unique: ", rb_unique20, "\n", sep = "")
cat("[2.0 DATA] QB INT-rate SD: ", sprintf("%.4f", qb_sd20), " | unique: ", qb_unique20, "\n", sep = "")
cat("[2.0 DATA] Opportunity/shrinkage feature store complete.\n")
