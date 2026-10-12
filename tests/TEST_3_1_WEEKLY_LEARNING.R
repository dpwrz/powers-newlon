# ============================================================
# FANTASY MODEL 3.1 - WEEKLY LEARNING SAFETY TESTS
# ============================================================
source("config.R")
source("R/weekly_engine_31.R")
source("R/weekly_training_history_31.R")

base <- data.frame(
  season = 2026L, week = 1L, player_id = "p1", position = "WR",
  weekly_fppg = 10, game_final = TRUE,
  feature_source = "strict_reconstruction", leakage_audited = TRUE,
  pregame_captured_at = NA_character_, kickoff_at = "2026-09-10T00:00:00Z",
  stringsAsFactors = FALSE
)
incoming <- dplyr::bind_rows(
  transform(base, weekly_fppg = 99),
  transform(base, week = 2L, weekly_fppg = 12)
)
out <- fm31_append_training_history(base, incoming)
stopifnot(nrow(out) == 2L)
stopifnot(out$weekly_fppg[out$week == 1L][1] == 10)
stopifnot(out$weekly_fppg[out$week == 2L][1] == 12)

bad_exact <- transform(
  base, week = 3L, feature_source = "pregame_archive",
  pregame_captured_at = "2026-09-10T20:00:00Z",
  kickoff_at = "2026-09-10T19:00:00Z"
)
err <- try(fm31_append_training_history(out, bad_exact), silent = TRUE)
stopifnot(inherits(err, "try-error"))

good <- do.call(rbind, lapply(1:4, function(w) {
  data.frame(
    season = 2026L, week = w,
    player_id = paste0("p", w, "_", 1:8),
    position = "WR",
    actual_fppg = 10 + (1:8) / 10,
    production_base_31 = 12 + (1:8) / 10,
    candidate_fppg_31 = 11 + (1:8) / 10,
    starter_cohort = TRUE,
    relevant_cohort = TRUE,
    stringsAsFactors = FALSE
  )
}))
gate <- fm31_weekly_live_gate(good, "WR", min_weeks = 3L, min_starters = 12L)
stopifnot(isTRUE(gate$promoted[1]))
stopifnot(gate$all_mae_gain_pct[1] > 0)
stopifnot(gate$starter_mae_gain_pct[1] > 0)

too_early <- good[good$week <= 2, , drop = FALSE]
gate2 <- fm31_weekly_live_gate(too_early, "WR", min_weeks = 3L, min_starters = 12L)
stopifnot(!isTRUE(gate2$promoted[1]))
stopifnot(grepl("enough_weeks", gate2$failed_checks[1]))

cat("Model 3.1 weekly learning safety checks passed.\n")
