# Regression test for 2026 current-stat opponent join.
suppressPackageStartupMessages(library(dplyr))

wk_chr_test <- function(x) as.character(x)
current_weekly <- tibble::tibble(
  season = c(2026L, 2026L), week = c(1L, 1L), team = c("BUF", "BUF"),
  player_id = c("p1", "p2"), opponent = c("NYJ", NA_character_)
)
team_schedule <- tibble::tibble(
  season = 2026L, week = 1L, team = "BUF", opponent = "NYJ"
)
cur_sched <- team_schedule |>
  dplyr::select(season, week, team, opponent_sched = opponent)
joined <- current_weekly |>
  dplyr::left_join(cur_sched, by = c("season", "week", "team"), relationship = "many-to-one") |>
  dplyr::mutate(opponent = dplyr::coalesce(wk_chr_test(opponent), wk_chr_test(opponent_sched))) |>
  dplyr::select(-dplyr::any_of("opponent_sched"))
stopifnot("opponent" %in% names(joined))
stopifnot(!any(c("opponent.x", "opponent.y") %in% names(joined)))
stopifnot(all(joined$opponent == "NYJ"))
cat("[PASS] Current-stat opponent join preserves one canonical opponent column.\n")
