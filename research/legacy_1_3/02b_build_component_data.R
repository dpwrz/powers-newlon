# ============================================================
# STEP 2B - BUILD 1.3 COMPONENT TARGETS, TEAM VOLUME + COMPETITION
# ============================================================
source("config.R")
source("research/legacy_1_3/R/component_engine.R")
ensure_packages(c("dplyr", "readr", "tidyr", "janitor", "rpart"))

model_path <- "data/processed/model_table.csv"
projection_path <- paste0("data/processed/projection_table_", CURRENT_SEASON, ".csv")
if (!file.exists(model_path) || !file.exists(projection_path)) stop("Run 02_build_features.R first.")

model <- readr::read_csv(model_path, show_col_types = FALSE)
projection <- readr::read_csv(projection_path, show_col_types = FALSE)
team_raw <- readr::read_csv("data/raw/team_stats.csv", show_col_types = FALSE) |> janitor::clean_names()
player_raw <- readr::read_csv("data/raw/player_stats.csv", show_col_types = FALSE) |> janitor::clean_names()
team_context <- if (file.exists("data/raw/team_context.csv")) {
  readr::read_csv("data/raw/team_context.csv", show_col_types = FALSE) |> janitor::clean_names()
} else data.frame()

num <- function(x) suppressWarnings(as.numeric(x))
safe_div13 <- function(a, b) ifelse(is.finite(b) & b > 0, a / b, 0)
ensure13 <- function(df, nm, default = 0) { if (!nm %in% names(df)) df[[nm]] <- default; df }
sum_top_n <- function(x, n = 2) {
  x <- sort(x[is.finite(x)], decreasing = TRUE)
  if (length(x) == 0) return(0)
  sum(utils::head(x, n))
}

# ------------------------------------------------------------
# Team season table. Team-level predictions are tiny (~32 rows/year)
# and let opportunity be derived from an expected team volume rather
# than simply repeating the player's prior per-game usage.
# ------------------------------------------------------------
for (nm in c("season", "team", "attempts", "carries", "passing_yards", "rushing_yards", "passing_tds", "rushing_tds")) {
  team_raw <- ensure13(team_raw, nm, 0)
}
team_actual <- team_raw |>
  dplyr::mutate(
    season = as.integer(season), team = as.character(team),
    games = dplyr::if_else(season >= 2021, 17, 16),
    team_pass_attempts_pg = safe_div13(num(attempts), games),
    team_carries_pg = safe_div13(num(carries), games),
    team_pass_yd_pg = safe_div13(num(passing_yards), games),
    team_rush_yd_pg = safe_div13(num(rushing_yards), games),
    team_points_proxy_pg = safe_div13(
      num(passing_yards) * SCORING$pass_yd + num(passing_tds) * SCORING$pass_td +
        num(rushing_yards) * SCORING$rush_yd + num(rushing_tds) * SCORING$rush_td,
      games
    ),
    team_attempts_total = num(attempts), team_carries_total = num(carries)
  ) |>
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

if (nrow(team_context) > 0 && all(c("season", "team") %in% names(team_context))) {
  tc <- team_context |>
    dplyr::mutate(season = as.integer(season) + 1, team = as.character(team)) |>
    dplyr::transmute(
      season, team,
      team_prior_pass_rate = num(team_pass_rate),
      team_prior_neutral_pass_rate = num(team_neutral_pass_rate),
      team_prior_plays_pg = num(team_plays_pg),
      team_prior_redzone_pass_rate = num(team_redzone_pass_rate),
      team_prior_deep_throw_rate = num(team_deep_throw_rate),
      team_prior_shotgun_rate = num(team_shotgun_rate),
      team_prior_no_huddle_rate = num(team_no_huddle_rate),
      team_prior_avg_air_yards = num(team_avg_air_yards)
    ) |>
    dplyr::distinct(season, team, .keep_all = TRUE)
  team_model <- team_model |> dplyr::left_join(tc, by = c("season", "team"))
}
for (nm in TEAM_VOLUME_FEATURES) team_model <- ensure13(team_model, nm, 0)
team_model <- team_model |>
  dplyr::mutate(dplyr::across(dplyr::all_of(TEAM_VOLUME_FEATURES), ~ tidyr::replace_na(num(.x), 0)))
readr::write_csv(team_model, "data/processed/team_component_model_table.csv")

# Honest team-volume OOF predictions by target season.
team_oof <- list()
for (yr in sort(unique(team_model$season))) {
  te <- team_model |> dplyr::filter(season == yr)
  tr <- team_model |> dplyr::filter(season < yr)
  if (nrow(te) == 0) next
  if (nrow(tr) >= 64) {
    feats <- intersect(TEAM_VOLUME_FEATURES, names(team_model))
    pass_fit <- fit_fantasy_model(tr, feats, "target_team_pass_attempts_pg", seed = SEED + yr + 11000, n_trees = TEAM_VOLUME_VALIDATION_TREES)
    carry_fit <- fit_fantasy_model(tr, feats, "target_team_carries_pg", seed = SEED + yr + 12000, n_trees = TEAM_VOLUME_VALIDATION_TREES)
    pass_pred <- component_clip(predict_fantasy_model(pass_fit, te), 20, 48)
    carry_pred <- component_clip(predict_fantasy_model(carry_fit, te), 18, 38)
  } else {
    pass_pred <- component_clip(te$team_prior_pass_attempts_pg, 20, 48)
    carry_pred <- component_clip(te$team_prior_carries_pg, 18, 38)
  }
  team_oof[[length(team_oof) + 1]] <- data.frame(
    season = te$season, team = te$team,
    team_projected_pass_attempts_pg = pass_pred,
    team_projected_carries_pg = carry_pred
  )
}
team_oof <- dplyr::bind_rows(team_oof)
readr::write_csv(team_oof, "data/processed/team_volume_oof.csv")

# 2026 team-volume projections use all historical target seasons.
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
      team_prior_pass_rate = num(team_pass_rate), team_prior_neutral_pass_rate = num(team_neutral_pass_rate),
      team_prior_plays_pg = num(team_plays_pg), team_prior_redzone_pass_rate = num(team_redzone_pass_rate),
      team_prior_deep_throw_rate = num(team_deep_throw_rate), team_prior_shotgun_rate = num(team_shotgun_rate),
      team_prior_no_huddle_rate = num(team_no_huddle_rate), team_prior_avg_air_yards = num(team_avg_air_yards)
    ) |>
    dplyr::distinct(team, .keep_all = TRUE)
  latest_team <- latest_team |> dplyr::left_join(tc_current, by = "team")
}
for (nm in TEAM_VOLUME_FEATURES) latest_team <- ensure13(latest_team, nm, 0)
latest_team <- latest_team |>
  dplyr::mutate(dplyr::across(dplyr::all_of(TEAM_VOLUME_FEATURES), ~ tidyr::replace_na(num(.x), 0)))

if (nrow(team_model) >= 64 && nrow(latest_team) > 0) {
  feats <- intersect(TEAM_VOLUME_FEATURES, names(team_model))
  final_pass <- fit_fantasy_model(team_model, feats, "target_team_pass_attempts_pg", seed = SEED + 21000, n_trees = TEAM_VOLUME_FINAL_TREES)
  final_carry <- fit_fantasy_model(team_model, feats, "target_team_carries_pg", seed = SEED + 22000, n_trees = TEAM_VOLUME_FINAL_TREES)
  latest_team$team_projected_pass_attempts_pg <- component_clip(predict_fantasy_model(final_pass, latest_team), 20, 48)
  latest_team$team_projected_carries_pg <- component_clip(predict_fantasy_model(final_carry, latest_team), 18, 38)
  saveRDS(final_pass, "models/team_pass_volume_model.rds")
  saveRDS(final_carry, "models/team_carry_volume_model.rds")
} else {
  latest_team$team_projected_pass_attempts_pg <- component_clip(latest_team$team_prior_pass_attempts_pg, 20, 48)
  latest_team$team_projected_carries_pg <- component_clip(latest_team$team_prior_carries_pg, 18, 38)
}
readr::write_csv(latest_team, paste0("data/processed/team_projection_table_", CURRENT_SEASON, ".csv"))

# ------------------------------------------------------------
# Teammate target/carry competition from the PRIOR season on the
# player's target/current team. This is pre-season information.
# ------------------------------------------------------------
for (nm in c("season", "team", "player_id", "position", "targets", "carries")) player_raw <- ensure13(player_raw, nm, 0)
prior_usage <- player_raw |>
  dplyr::mutate(season = as.integer(season), team = as.character(team), player_id = as.character(player_id), position = toupper(as.character(position))) |>
  dplyr::filter(position %in% c("RB", "WR", "TE"), !is.na(team), team != "") |>
  dplyr::group_by(season, team, player_id) |>
  dplyr::summarise(targets = sum(num(targets), na.rm = TRUE), carries = sum(num(carries), na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(team_actual |> dplyr::select(season, team, team_attempts_total, team_carries_total), by = c("season", "team")) |>
  dplyr::mutate(
    usage_target_share = pmin(1, pmax(0, safe_div13(targets, team_attempts_total))),
    usage_carry_share = pmin(1, pmax(0, safe_div13(carries, team_carries_total))),
    target_season = season + 1,
    teammate_id = player_id
  ) |>
  dplyr::select(target_season, team, teammate_id, usage_target_share, usage_carry_share)

build_competition <- function(keys, team_col) {
  if (nrow(keys) == 0 || nrow(prior_usage) == 0) return(keys |> dplyr::mutate(
    prior_teammate_max_target_share = 0, prior_teammate_top2_target_share = 0, prior_teammate_target_hhi = 0,
    prior_teammate_max_carry_share = 0, prior_teammate_top2_carry_share = 0, prior_teammate_carry_hhi = 0
  ))
  names(keys)[names(keys) == team_col] <- "team"
  joined <- keys |>
    dplyr::left_join(prior_usage, by = c("season" = "target_season", "team")) |>
    dplyr::filter(is.na(teammate_id) | teammate_id != player_id) |>
    dplyr::group_by(season, player_id, team) |>
    dplyr::summarise(
      prior_teammate_max_target_share = ifelse(all(is.na(usage_target_share)), 0, max(usage_target_share, na.rm = TRUE)),
      prior_teammate_top2_target_share = sum_top_n(usage_target_share, 2),
      prior_teammate_target_hhi = sum(usage_target_share^2, na.rm = TRUE),
      prior_teammate_max_carry_share = ifelse(all(is.na(usage_carry_share)), 0, max(usage_carry_share, na.rm = TRUE)),
      prior_teammate_top2_carry_share = sum_top_n(usage_carry_share, 2),
      prior_teammate_carry_hhi = sum(usage_carry_share^2, na.rm = TRUE),
      .groups = "drop"
    )
  joined
}

model_comp <- build_competition(model |> dplyr::select(season, player_id, team) |> dplyr::distinct(), "team")
proj_keys <- projection |> dplyr::transmute(season = CURRENT_SEASON, player_id, current_team) |> dplyr::distinct()
proj_comp <- build_competition(proj_keys, "current_team")

# ------------------------------------------------------------
# Component targets and derived interaction features.
# ------------------------------------------------------------
model <- model |>
  dplyr::left_join(team_actual |> dplyr::select(season, team, team_attempts_total, team_carries_total), by = c("season", "team")) |>
  dplyr::left_join(team_oof, by = c("season", "team")) |>
  dplyr::left_join(model_comp, by = c("season", "player_id", "team")) |>
  dplyr::mutate(
    team_projected_pass_attempts_pg = dplyr::coalesce(team_projected_pass_attempts_pg, team_prior_pass_attempts_pg),
    team_projected_carries_pg = dplyr::coalesce(team_projected_carries_pg, team_prior_carries_pg),
    target_pass_attempts_pg = safe_div13(num(attempts), num(games)),
    target_carries_pg = safe_div13(num(carries), num(games)),
    target_targets_pg = safe_div13(num(targets), num(games)),
    target_target_share = dplyr::if_else(num(target_share) > 0, pmin(1, num(target_share)), pmin(1, safe_div13(num(targets), team_attempts_total))),
    target_carry_share = pmin(1, safe_div13(num(carries), team_carries_total)),
    target_catch_rate = dplyr::if_else(num(targets) >= 5, pmin(1, pmax(0, safe_div13(num(receptions), num(targets)))), NA_real_),
    target_yards_per_target = dplyr::if_else(num(targets) >= 5, pmax(0, safe_div13(num(receiving_yards), num(targets))), NA_real_),
    target_yards_per_carry = dplyr::if_else(num(carries) >= 3, pmax(0, safe_div13(num(rushing_yards), num(carries))), 0),
    target_yards_per_attempt = dplyr::if_else(num(attempts) >= 30, pmax(0, safe_div13(num(passing_yards), num(attempts))), NA_real_),
    target_pass_td_rate = dplyr::if_else(num(attempts) >= 30, pmax(0, safe_div13(num(passing_tds), num(attempts))), NA_real_),
    target_interception_rate = dplyr::if_else(num(attempts) >= 30, pmax(0, safe_div13(num(interceptions), num(attempts))), NA_real_),
    target_rush_td_rate = dplyr::if_else(num(carries) >= 3, pmax(0, safe_div13(num(rushing_tds), num(carries))), 0),
    target_rec_td_rate = dplyr::if_else(num(targets) >= 5, pmax(0, safe_div13(num(receiving_tds), num(targets))), NA_real_)
  )

projection <- projection |>
  dplyr::left_join(latest_team |> dplyr::select(team, team_projected_pass_attempts_pg, team_projected_carries_pg), by = c("current_team" = "team")) |>
  dplyr::left_join(proj_comp |> dplyr::select(-season), by = c("player_id", "current_team" = "team"))

add_13_interactions <- function(df) {
  needed <- c(
    "prior_teammate_max_target_share", "prior_teammate_top2_target_share", "prior_teammate_target_hhi",
    "prior_teammate_max_carry_share", "prior_teammate_top2_carry_share", "prior_teammate_carry_hhi",
    "rec_prior_deep_target_rate", "qb_prior_deep_throw_rate", "rec_prior_redzone_target_rate", "qb_prior_redzone_throw_rate",
    "rec_prior_middle_target_rate", "qb_prior_middle_throw_rate", "qb_prior_rb_target_rate", "rec_prior_short_target_rate",
    "team_projected_pass_attempts_pg", "team_projected_carries_pg", "team_prior_carries_pg", "prior_target_share", "prior_carries_pg"
  )
  for (nm in needed) df <- ensure13(df, nm, 0)
  df |>
    dplyr::mutate(
      target_competition_index = pmax(0, prior_teammate_top2_target_share),
      carry_competition_index = pmax(0, prior_teammate_top2_carry_share),
      qb_wr_deep_synergy = pmax(0, rec_prior_deep_target_rate * qb_prior_deep_throw_rate),
      qb_wr_redzone_synergy = pmax(0, rec_prior_redzone_target_rate * qb_prior_redzone_throw_rate),
      qb_te_middle_synergy = pmax(0, rec_prior_middle_target_rate * qb_prior_middle_throw_rate),
      qb_te_redzone_synergy = pmax(0, rec_prior_redzone_target_rate * qb_prior_redzone_throw_rate),
      qb_rb_receiving_synergy = pmax(0, qb_prior_rb_target_rate * rec_prior_short_target_rate),
      team_pass_opportunity_index = pmax(0, team_projected_pass_attempts_pg * pmax(prior_target_share, 0.01)),
      team_rush_opportunity_index = pmax(0, team_projected_carries_pg * pmax(prior_carries_pg / pmax(1, team_prior_carries_pg), 0.01))
    )
}
model <- add_13_interactions(model)
projection <- add_13_interactions(projection)

# Clean new numeric features without turning intentionally missing component
# targets into zeros.
for (nm in unique(c(COMPONENT_EXTRA_FEATURES, "team_projected_pass_attempts_pg", "team_projected_carries_pg"))) {
  model <- ensure13(model, nm, 0); projection <- ensure13(projection, nm, 0)
  model[[nm]][!is.finite(num(model[[nm]]))] <- 0
  projection[[nm]][!is.finite(num(projection[[nm]]))] <- 0
}

readr::write_csv(model, "data/processed/component_model_table.csv")
readr::write_csv(projection, paste0("data/processed/component_projection_table_", CURRENT_SEASON, ".csv"))

cat("[1.3 COMPONENT DATA] Player training rows: ", nrow(model), "\n", sep = "")
cat("[1.3 COMPONENT DATA] Projection candidates: ", nrow(projection), "\n", sep = "")
cat("[1.3 COMPONENT DATA] Team target seasons: ", nrow(team_model), "\n", sep = "")
cat("[1.3 COMPONENT DATA] Team volume OOF rows: ", nrow(team_oof), "\n", sep = "")
