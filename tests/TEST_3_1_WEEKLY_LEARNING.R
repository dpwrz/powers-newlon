# Fantasy Model 3.1 weekly learning integrity tests.
source("config.R")
ensure_packages(c("dplyr","readr","xgboost"))
source("R/weekly_engine_31.R")
source("R/weekly_learning_31.R")

td <- tempfile("fm31-learning-")
dir.create(td, recursive = TRUE)
feature_path <- file.path(td, "features.csv")
status_path <- file.path(td, "status.csv")
ledger_path <- file.path(td, "ledger.csv")
current_path <- file.path(td, "current.csv")
accuracy_path <- file.path(td, "accuracy.csv")

make_weekly <- function(value = 10) {
  z <- data.frame(
    season = 2026L,
    week = 7L,
    player_id = c("p1","p2"),
    player_display_name = c("One","Two"),
    position = c("QB","WR"),
    team = c("AAA","BBB"),
    opponent = c("BBB","AAA"),
    projected_weekly_fppg = c(20,12),
    production_base_31 = c(19,11),
    blend_alpha_31 = c(.2,.3),
    model31_promoted = c(TRUE,TRUE),
    availability_factor = c(1,1),
    is_actual = c(0,0),
    stringsAsFactors = FALSE
  )
  for (nm in MODEL31_SAFE_FEATURES) if (!nm %in% names(z)) z[[nm]] <- 0
  z$roll3_fppg <- value
  z
}

schedule <- data.frame(
  week = c(7L,7L),
  team = c("AAA","BBB"),
  opponent = c("BBB","AAA"),
  kickoff = as.POSIXct(c("2026-10-18 17:00:00","2026-10-18 17:00:00"), tz = "UTC"),
  score_started = c(FALSE,FALSE),
  stringsAsFactors = FALSE
)

t1 <- as.POSIXct("2026-10-18 15:00:00", tz = "UTC")
wl31_capture_features(make_weekly(10), schedule, t1, 2026, feature_path, status_path)
a <- readr::read_csv(feature_path, show_col_types = FALSE)
stopifnot(nrow(a) == 2)
stopifnot(all(a$roll3_fppg == 10))

# Still pregame: latest feature snapshot replaces the older feature row.
t2 <- as.POSIXct("2026-10-18 16:00:00", tz = "UTC")
wl31_capture_features(make_weekly(15), schedule, t2, 2026, feature_path, status_path)
b <- readr::read_csv(feature_path, show_col_types = FALSE)
stopifnot(nrow(b) == 2)
stopifnot(all(b$roll3_fppg == 15))
cap_txt <- format(as.POSIXct(b$captured_at_utc, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
stopifnot(all(cap_txt == "2026-10-18T16:00:00Z"))

# After kickoff: archive must remain frozen.
t3 <- as.POSIXct("2026-10-18 18:00:00", tz = "UTC")
wl31_capture_features(make_weekly(99), schedule, t3, 2026, feature_path, status_path)
c <- readr::read_csv(feature_path, show_col_types = FALSE)
stopifnot(nrow(c) == 2)
stopifnot(all(c$roll3_fppg == 15))

acc <- data.frame(
  week = c(7L,7L),
  player_id = c("p1","p2"),
  position = c("QB","WR"),
  actual_fppg = c(25,8),
  stringsAsFactors = FALSE
)
readr::write_csv(acc, accuracy_path)

cur1 <- wl31_update_ledger(feature_path, accuracy_path, ledger_path, current_path, 2026, 1.5)
led1 <- readr::read_csv(ledger_path, show_col_types = FALSE)
stopifnot(nrow(led1) == 2)
stopifnot(nrow(cur1) == 2)

# Identical rerun is idempotent.
cur2 <- wl31_update_ledger(feature_path, accuracy_path, ledger_path, current_path, 2026, 1.5)
led2 <- readr::read_csv(ledger_path, show_col_types = FALSE)
stopifnot(nrow(led2) == 2)

# A stat correction creates a new revision rather than deleting history.
acc$actual_fppg[acc$player_id == "p1"] <- 24.5
readr::write_csv(acc, accuracy_path)
Sys.sleep(1)
cur3 <- wl31_update_ledger(feature_path, accuracy_path, ledger_path, current_path, 2026, 1.5)
led3 <- readr::read_csv(ledger_path, show_col_types = FALSE)
stopifnot(nrow(led3) == 3)
stopifnot(nrow(cur3) == 2)
stopifnot(abs(cur3$actual_fppg[cur3$player_id == "p1"] - 24.5) < 1e-9)
stopifnot(sum(led3$player_id == "p1") == 2)

# Coverage eligibility requires every modeled position and strong coverage.
full_current <- data.frame(
  week = rep(8L, 8),
  position = rep(POSITIONS, each = 2),
  player_id = paste0("x", seq_len(8)),
  stringsAsFactors = FALSE
)
full_actual <- full_current
full_actual$actual_fppg <- 1
cov <- wl31_coverage_by_week(full_current, full_actual)
stopifnot(identical(wl31_eligible_weeks(cov), 8L))

partial <- full_current[full_current$position != "TE", , drop = FALSE]
cov2 <- wl31_coverage_by_week(partial, full_actual)
stopifnot(length(wl31_eligible_weeks(cov2)) == 0)

good_metric <- data.frame(
  position = "QB", n = 80, oos_weeks = 2, starter_n = 24,
  MAE_gain_pct = 1.2, RMSE_gain_pct = 0.3,
  starter_MAE_gain_pct = 0.8, week_wins = 2
)
stopifnot(isTRUE(wl31_online_gate(good_metric, TRUE)$promote))
stopifnot(!isTRUE(wl31_online_gate(good_metric, FALSE)$promote))

cat("Model 3.1 weekly learning integrity tests passed.\n")
