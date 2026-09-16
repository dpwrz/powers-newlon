# ============================================================
# STEP 7 - 1.2 REDRAFT + DYNASTY RANKINGS
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr"))
redraft <- readr::read_csv(paste0("output/", CURRENT_SEASON, "_projections.csv"), show_col_types = FALSE)
dynasty <- readr::read_csv("output/dynasty_rankings.csv", show_col_types = FALSE)

replacement_rows <- lapply(POSITIONS, function(pos) {
  d <- redraft |> dplyr::filter(position == pos) |> dplyr::arrange(dplyr::desc(projected_fppg))
  rank_needed <- unname(REPLACEMENT_RANK[pos])
  value <- if (nrow(d) >= rank_needed) d$projected_fppg[rank_needed] else min(d$projected_fppg, na.rm = TRUE)
  data.frame(position = pos, replacement_fppg = value)
}) |> dplyr::bind_rows()

redraft <- redraft |>
  dplyr::left_join(replacement_rows, by = "position") |>
  dplyr::mutate(
    vorp_fppg = projected_fppg - replacement_fppg,
    scarcity_adjusted_value = vorp_fppg * projected_games,
    upside_index = 100 * pmin(1, pmax(0,
      0.50 * elite_probability + 0.25 * starter_probability + 0.25 * breakout_probability
    ))
  ) |>
  dplyr::arrange(
    dplyr::desc(scarcity_adjusted_value),
    dplyr::desc(projected_fppg),
    dplyr::desc(elite_probability)
  ) |>
  dplyr::mutate(overall_rank = dplyr::row_number()) |>
  dplyr::select(
    overall_rank, position_rank, player_id, player_display_name, position,
    current_team, current_status, age, is_rookie,
    projected_fppg, projection_floor_fppg, projection_ceiling_fppg,
    projected_games, projected_fantasy_points, full_17_game_points,
    elite_probability, starter_probability, breakout_probability, upside_index,
    fantasy_outlook, vorp_fppg, scarcity_adjusted_value,
    confidence, total_uncertainty_sd, model_disagreement_sd, model_engine,
    dplyr::everything()
  )

readr::write_csv(redraft, paste0("output/final_", CURRENT_SEASON, "_rankings.csv"))
readr::write_csv(dynasty |> dplyr::mutate(overall_rank = dplyr::row_number()), "output/final_dynasty_rankings.csv")
