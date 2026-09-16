# ============================================================
# FANTASY MODEL 2.0 - SURGICAL QB INTERCEPTION / FPPG REPAIR
# Avoids rebuilding the validated 1.2 feature-engineering pipeline.
# ============================================================
cat("[2.0 HOTFIX 2] Surgical QB scoring repair loaded.\n")
source("config.R")
ensure_packages(c("dplyr", "readr", "janitor", "tidyr"))

model_path <- "data/processed/model_table.csv"
proj_path <- paste0("data/processed/projection_table_", CURRENT_SEASON, ".csv")
raw_path <- "data/raw/player_stats.csv"
marker_path <- "data/processed/qb_interception_repair_2_0.marker"

for (p in c(model_path, proj_path, raw_path)) {
  if (!file.exists(p)) stop("[HOTFIX 2] Required file missing: ", p)
}

num2 <- function(x) suppressWarnings(as.numeric(x))
safe_div2 <- function(a, b) ifelse(is.finite(b) & b > 0, a / b, 0)
ensure2 <- function(df, nm, default = 0) {
  if (!nm %in% names(df)) df[[nm]] <- default
  df
}

model <- suppressWarnings(readr::read_csv(model_path, show_col_types = FALSE, progress = FALSE)) |> janitor::clean_names()
proj <- suppressWarnings(readr::read_csv(proj_path, show_col_types = FALSE, progress = FALSE)) |> janitor::clean_names()
raw <- suppressWarnings(readr::read_csv(raw_path, show_col_types = FALSE, progress = FALSE)) |> janitor::clean_names()

# Resolve raw nflverse schema explicitly.
if (!"player_id" %in% names(raw) && "gsis_id" %in% names(raw)) raw$player_id <- raw$gsis_id
if (!"player_id" %in% names(raw)) stop("[HOTFIX 2] player_stats.csv has no player_id/gsis_id column.")
if (!"season" %in% names(raw)) stop("[HOTFIX 2] player_stats.csv has no season column.")
if (!"position" %in% names(raw)) stop("[HOTFIX 2] player_stats.csv has no position column.")

int_source <- if ("passing_interceptions" %in% names(raw)) {
  "passing_interceptions"
} else if ("interceptions" %in% names(raw)) {
  "interceptions"
} else {
  stop("[HOTFIX 2] player_stats.csv has neither passing_interceptions nor interceptions.")
}

game_source <- if ("games" %in% names(raw)) {
  "games"
} else if ("games_played" %in% names(raw)) {
  "games_played"
} else {
  NA_character_
}
if (is.na(game_source)) stop("[HOTFIX 2] player_stats.csv has neither games nor games_played.")

qb_raw <- raw |>
  dplyr::transmute(
    player_id = as.character(player_id),
    season = as.integer(season),
    position = toupper(as.character(position)),
    raw_interceptions = num2(.data[[int_source]]),
    raw_games = num2(.data[[game_source]])
  ) |>
  dplyr::filter(position == "QB", !is.na(player_id), player_id != "", is.finite(season)) |>
  dplyr::group_by(player_id, season) |>
  dplyr::summarise(
    raw_interceptions = sum(raw_interceptions, na.rm = TRUE),
    # Match the historical feature builder's player-season game handling.
    raw_games = pmin(18, sum(raw_games, na.rm = TRUE)),
    .groups = "drop"
  )

if (nrow(qb_raw) == 0 || !any(qb_raw$raw_interceptions > 0, na.rm = TRUE)) {
  stop("[HOTFIX 2] Raw QB data contains no positive interceptions; cannot safely repair scoring.")
}

for (nm in c("player_id", "position", "season", "games", "interceptions", "target_fppg")) model <- ensure2(model, nm, 0)
model <- model |>
  dplyr::mutate(
    player_id = as.character(player_id),
    position = toupper(as.character(position)),
    season = as.integer(season),
    games = num2(games),
    interceptions = num2(interceptions)
  )

qb_rows <- model$position == "QB"
model_has_real_ints <- any(qb_rows & is.finite(model$interceptions) & model$interceptions > 0, na.rm = TRUE)

# If the marker exists, this repair has already been applied. Verify rather than
# touching FPPG a second time (important for idempotence).
if (file.exists(marker_path)) {
  if (!model_has_real_ints) {
    stop("[HOTFIX 2] Repair marker exists but model_table still has zero QB interceptions. Remove the marker and rerun this repair.")
  }
  cat("[2.0 HOTFIX 2] QB scoring repair already applied; verified and skipping.\n")
  quit(save = "no", status = 0)
}

# If real interception counts are already present, no scoring repair is needed.
if (model_has_real_ints) {
  writeLines(c(
    paste0("verified_existing_interceptions=", Sys.time()),
    paste0("source_column=", int_source)
  ), marker_path)
  cat("[2.0 HOTFIX 2] Existing feature store already contains real QB interceptions; no FPPG adjustment needed.\n")
  quit(save = "no", status = 0)
}

cat("[2.0 HOTFIX 2] Stale all-zero QB interceptions confirmed. Applying surgical repair...\n")

# Preserve pre-repair tables once so the user's validated 1.2 store can always be
# recovered without downloading PBP again.
backup_model <- "data/processed/model_table_pre_2_0_qb_repair.csv"
backup_proj <- paste0("data/processed/projection_table_", CURRENT_SEASON, "_pre_2_0_qb_repair.csv")
if (!file.exists(backup_model)) file.copy(model_path, backup_model, overwrite = FALSE)
if (!file.exists(backup_proj)) file.copy(proj_path, backup_proj, overwrite = FALSE)

# Join actual interception counts to the target season. Since this branch only
# runs for an all-zero stale store, the correction is simply INT * scoring value.
model <- model |>
  dplyr::left_join(qb_raw |> dplyr::select(player_id, season, actual_interceptions = raw_interceptions), by = c("player_id", "season")) |>
  dplyr::mutate(actual_interceptions = tidyr::replace_na(actual_interceptions, 0))

# Prior-year and two-year interception counts repair lagged fantasy features
# without rebuilding the full 1.2 context pipeline.
prior_int <- qb_raw |>
  dplyr::transmute(player_id, season = season + 1L, prior_actual_interceptions = raw_interceptions)
two_int <- qb_raw |>
  dplyr::transmute(player_id, season = season + 2L, two_year_actual_interceptions = raw_interceptions)
model <- model |>
  dplyr::left_join(prior_int, by = c("player_id", "season")) |>
  dplyr::left_join(two_int, by = c("player_id", "season")) |>
  dplyr::mutate(
    prior_actual_interceptions = tidyr::replace_na(prior_actual_interceptions, 0),
    two_year_actual_interceptions = tidyr::replace_na(two_year_actual_interceptions, 0)
  )

# Ensure columns needed for scoring corrections exist.
for (nm in c("fantasy_points", "target_fantasy_points", "fppg", "target_fppg",
             "prior_fppg", "two_year_fppg", "recent_weighted_fppg", "fppg_trend",
             "prior_fantasy_points", "prior_games", "two_year_games")) {
  model <- ensure2(model, nm, 0)
  model[[nm]] <- num2(model[[nm]])
}

is_qb <- model$position == "QB"
cur_penalty <- model$actual_interceptions * SCORING$interception
prior_penalty <- model$prior_actual_interceptions * SCORING$interception
two_penalty <- model$two_year_actual_interceptions * SCORING$interception

model$interceptions[is_qb] <- model$actual_interceptions[is_qb]
model$fantasy_points[is_qb] <- model$fantasy_points[is_qb] + cur_penalty[is_qb]
model$target_fantasy_points[is_qb] <- model$target_fantasy_points[is_qb] + cur_penalty[is_qb]
model$fppg[is_qb] <- model$fppg[is_qb] + safe_div2(cur_penalty[is_qb], model$games[is_qb])
model$target_fppg[is_qb] <- model$target_fppg[is_qb] + safe_div2(cur_penalty[is_qb], model$games[is_qb])

# The old store's prior fantasy features were built from the same all-zero INT
# scoring, so adjust them using the raw prior-season interceptions.
model$prior_fppg[is_qb] <- model$prior_fppg[is_qb] + safe_div2(prior_penalty[is_qb], model$prior_games[is_qb])
model$two_year_fppg[is_qb] <- model$two_year_fppg[is_qb] + safe_div2(two_penalty[is_qb], model$two_year_games[is_qb])
model$prior_fantasy_points[is_qb] <- model$prior_fantasy_points[is_qb] + prior_penalty[is_qb]
model$recent_weighted_fppg[is_qb] <- ifelse(
  model$prior_fppg[is_qb] != 0,
  0.70 * model$prior_fppg[is_qb] + 0.30 * ifelse(model$two_year_fppg[is_qb] != 0, model$two_year_fppg[is_qb], model$prior_fppg[is_qb]),
  0
)
model$fppg_trend[is_qb] <- ifelse(
  model$prior_fppg[is_qb] != 0 & model$two_year_fppg[is_qb] != 0,
  model$prior_fppg[is_qb] - model$two_year_fppg[is_qb],
  0
)

model <- model |>
  dplyr::select(-actual_interceptions, -prior_actual_interceptions, -two_year_actual_interceptions)

# Repair the current 2026 QB lagged fantasy inputs from 2025/2024 raw INT counts.
for (nm in c("player_id", "position", "prior_fppg", "two_year_fppg", "recent_weighted_fppg",
             "fppg_trend", "prior_fantasy_points", "prior_games", "two_year_games")) {
  proj <- ensure2(proj, nm, 0)
}
proj <- proj |>
  dplyr::mutate(player_id = as.character(player_id), position = toupper(as.character(position)))

p1 <- qb_raw |> dplyr::filter(season == CURRENT_SEASON - 1L) |>
  dplyr::select(player_id, p1_int = raw_interceptions)
p2 <- qb_raw |> dplyr::filter(season == CURRENT_SEASON - 2L) |>
  dplyr::select(player_id, p2_int = raw_interceptions)
proj <- proj |>
  dplyr::left_join(p1, by = "player_id") |>
  dplyr::left_join(p2, by = "player_id") |>
  dplyr::mutate(p1_int = tidyr::replace_na(p1_int, 0), p2_int = tidyr::replace_na(p2_int, 0))

for (nm in c("prior_fppg", "two_year_fppg", "recent_weighted_fppg", "fppg_trend",
             "prior_fantasy_points", "prior_games", "two_year_games")) proj[[nm]] <- num2(proj[[nm]])

is_qb_p <- proj$position == "QB"
p1_pen <- proj$p1_int * SCORING$interception
p2_pen <- proj$p2_int * SCORING$interception
proj$prior_fppg[is_qb_p] <- proj$prior_fppg[is_qb_p] + safe_div2(p1_pen[is_qb_p], proj$prior_games[is_qb_p])
proj$two_year_fppg[is_qb_p] <- proj$two_year_fppg[is_qb_p] + safe_div2(p2_pen[is_qb_p], proj$two_year_games[is_qb_p])
proj$prior_fantasy_points[is_qb_p] <- proj$prior_fantasy_points[is_qb_p] + p1_pen[is_qb_p]
proj$recent_weighted_fppg[is_qb_p] <- ifelse(
  proj$prior_fppg[is_qb_p] != 0,
  0.70 * proj$prior_fppg[is_qb_p] + 0.30 * ifelse(proj$two_year_fppg[is_qb_p] != 0, proj$two_year_fppg[is_qb_p], proj$prior_fppg[is_qb_p]),
  0
)
proj$fppg_trend[is_qb_p] <- ifelse(
  proj$prior_fppg[is_qb_p] != 0 & proj$two_year_fppg[is_qb_p] != 0,
  proj$prior_fppg[is_qb_p] - proj$two_year_fppg[is_qb_p],
  0
)
proj <- proj |> dplyr::select(-p1_int, -p2_int)

# Atomic-ish writes: write temporary files first, then replace originals.
tmp_model <- paste0(model_path, ".hotfix2.tmp")
tmp_proj <- paste0(proj_path, ".hotfix2.tmp")
readr::write_csv(model, tmp_model)
readr::write_csv(proj, tmp_proj)
if (!file.rename(tmp_model, model_path)) {
  file.copy(tmp_model, model_path, overwrite = TRUE); unlink(tmp_model)
}
if (!file.rename(tmp_proj, proj_path)) {
  file.copy(tmp_proj, proj_path, overwrite = TRUE); unlink(tmp_proj)
}

# Verify before marking complete.
verify <- suppressWarnings(readr::read_csv(
  model_path,
  col_select = tidyselect::any_of(c("position", "interceptions", "target_fppg")),
  show_col_types = FALSE, progress = FALSE
))
verified <- nrow(verify) > 0 && any(toupper(as.character(verify$position)) == "QB" & num2(verify$interceptions) > 0, na.rm = TRUE)
if (!verified) stop("[HOTFIX 2] Repair write completed but verification failed; original backups remain available.")

writeLines(c(
  paste0("applied=", Sys.time()),
  paste0("source_column=", int_source),
  paste0("interception_scoring=", SCORING$interception),
  paste0("backup_model=", backup_model),
  paste0("backup_projection=", backup_proj)
), marker_path)

cat("[2.0 HOTFIX 2] PASS: QB interception counts and fantasy scoring repaired without rebuilding 1.2 features.\n")
cat("[2.0 HOTFIX 2] Original feature tables were preserved as pre-repair backups.\n")
