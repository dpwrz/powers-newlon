# ============================================================
# STEP 6 - 1.2 AGE-AWARE DYNASTY PROJECTIONS
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr"))
base <- readr::read_csv(paste0("output/", CURRENT_SEASON, "_projections.csv"), show_col_types = FALSE)

age_multiplier <- function(position, age) {
  age[!is.finite(age) | age <= 0] <- 26
  out <- rep(1, length(age))
  out[position == "QB"] <- ifelse(age[position == "QB"] <= 28, 1.01, ifelse(age[position == "QB"] <= 33, 1.00, 0.965^(age[position == "QB"] - 33)))
  out[position == "RB"] <- ifelse(age[position == "RB"] <= 23, 1.03, ifelse(age[position == "RB"] <= 25, 1.00, 0.90^(age[position == "RB"] - 25)))
  out[position == "WR"] <- ifelse(age[position == "WR"] <= 24, 1.025, ifelse(age[position == "WR"] <= 28, 1.00, 0.94^(age[position == "WR"] - 28)))
  out[position == "TE"] <- ifelse(age[position == "TE"] <= 25, 1.025, ifelse(age[position == "TE"] <= 30, 1.00, 0.95^(age[position == "TE"] - 30)))
  pmax(0.45, out)
}

out <- list()
current_fppg <- base$projected_fppg
for (yr in 0:DYNASTY_YEARS) {
  if (yr == 0) {
    yr_fppg <- current_fppg
  } else {
    yr_age <- base$age + yr
    current_fppg <- current_fppg * age_multiplier(base$position, yr_age)
    yr_fppg <- current_fppg
  }
  out[[length(out) + 1]] <- base |>
    dplyr::transmute(
      player_id, player_display_name, position, current_team,
      dynasty_year = CURRENT_SEASON + yr,
      projected_age = age + yr,
      projected_fppg = yr_fppg,
      projected_fantasy_points = yr_fppg * projected_games,
      confidence,
      elite_probability, starter_probability, breakout_probability
    )
}

dynasty <- dplyr::bind_rows(out)
readr::write_csv(dynasty, "output/dynasty_projections.csv")

dynasty_rank <- dynasty |>
  dplyr::mutate(
    discount = 1 / (1.10 ^ (dynasty_year - CURRENT_SEASON)),
    discounted_value = projected_fantasy_points * discount
  ) |>
  dplyr::group_by(player_id, player_display_name, position, current_team) |>
  dplyr::summarise(
    dynasty_value = sum(discounted_value, na.rm = TRUE),
    year1_fppg = dplyr::first(projected_fppg),
    year3_fppg = dplyr::nth(projected_fppg, pmin(3, dplyr::n())),
    elite_probability = dplyr::first(elite_probability),
    starter_probability = dplyr::first(starter_probability),
    breakout_probability = dplyr::first(breakout_probability),
    confidence = dplyr::first(confidence),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(dynasty_value))
readr::write_csv(dynasty_rank, "output/dynasty_rankings.csv")
