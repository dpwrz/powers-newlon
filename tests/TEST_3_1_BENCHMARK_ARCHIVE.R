# ============================================================
# FANTASY MODEL 3.1 - BENCHMARK ARCHIVE INTEGRITY TESTS
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "readr", "tibble", "tidyr", "purrr", "httr2", "jsonlite", "nflreadr"))
source("R/benchmark_archive_31.R")

stopifnot(abs(bench31_half_ppr_from_stats(list(pass_yd = 250, pass_td = 2, pass_int = 1, rush_yd = 20)) - 18) < 1e-8)

payload <- list(
  `1234` = list(
    player_id = "1234",
    position = "WR",
    team = "AAA",
    stats = list(pts_half_ppr = 12.5, rec = 5, rec_yd = 75, rec_td = 0)
  )
)
sp <- bench31_normalize_sleeper_payload(payload, "WR")
stopifnot(nrow(sp) == 1, sp$sleeper_id[[1]] == "1234", abs(sp$sleeper_projection[[1]] - 12.5) < 1e-8)

now <- Sys.time()
weekly <- tibble::tibble(
  week = c(3, 3),
  player_id = c("future-player", "started-player"),
  player_display_name = c("Future Player", "Started Player"),
  position = c("WR", "WR"),
  team = c("AAA", "BBB"),
  opponent = c("CCC", "DDD"),
  projected_weekly_fppg = c(14, 15),
  is_actual = c(0, 0),
  weekly_position_rank = c(10, 9),
  weekly_floor = c(8, 9),
  weekly_ceiling = c(20, 21),
  expected_abs_error = c(4, 4),
  projection_confidence = c("medium", "medium"),
  model31_promoted = c(TRUE, TRUE),
  production_base_31 = c(13.5, 14.5)
)
schedule_team <- tibble::tibble(
  week = c(3, 3),
  team = c("AAA", "BBB"),
  opponent = c("CCC", "DDD"),
  kickoff = c(now + 3600, now - 3600),
  score_started = c(FALSE, TRUE)
)
mr <- bench31_model_rows(weekly, schedule_team, 3, now)
stopifnot(nrow(mr) == 1, mr$player_id[[1]] == "future-player")

# A stale prior-week backup must not drag the archive back to an old week.
weekly_clock <- dplyr::bind_rows(
  weekly,
  tibble::tibble(
    week = 2, player_id = "old-backup", player_display_name = "Old Backup",
    position = "WR", team = "ZZZ", opponent = "YYY",
    projected_weekly_fppg = 1, is_actual = 0, weekly_position_rank = 99,
    weekly_floor = 0, weekly_ceiling = 3, expected_abs_error = 2,
    projection_confidence = "low", model31_promoted = FALSE,
    production_base_31 = 1
  )
)
schedule_clock <- dplyr::bind_rows(
  schedule_team,
  tibble::tibble(
    week = 4, team = "EEE", opponent = "FFF",
    kickoff = now + 7 * 86400, score_started = FALSE
  )
)
stopifnot(bench31_active_week(weekly_clock, schedule_clock, now) == 3L)
stopifnot(is.finite(as.numeric(bench31_parse_kickoff("2026-09-17", "20:15:00"))))

existing <- mr
existing$projection <- 10
existing$captured_at_utc <- "2026-09-17T10:00:00Z"
newer <- mr
newer$projection <- 14
newer$captured_at_utc <- "2026-09-17T11:00:00Z"
updated <- bench31_update_archive(existing, newer)
stopifnot(nrow(updated) == 1, abs(updated$projection[[1]] - 14) < 1e-8)

frozen <- existing
frozen$player_id <- "already-frozen"
combined <- bench31_update_archive(frozen, newer)
stopifnot(nrow(combined) == 2, any(combined$player_id == "already-frozen"), any(combined$player_id == "future-player"))

cat("Fantasy Model 3.1 benchmark archive integrity tests passed.\n")
