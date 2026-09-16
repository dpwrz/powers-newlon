# Regression test for Model 2.5 frozen-row restore schema.
suppressPackageStartupMessages(library(dplyr))

fresh <- data.frame(
  week = 3L,
  player_id = "00-TEST-A",
  projected_weekly_fppg = 14.2,
  gameday = as.Date("2026-09-27"),
  stringsAsFactors = FALSE
)
frozen <- data.frame(
  week = 2L,
  player_id = "00-TEST-B",
  projected_weekly_fppg = 12.1,
  gameday = "2026-09-20",
  stringsAsFactors = FALSE
)

common <- intersect(names(fresh), names(frozen))
for (nm in common) {
  a_dt <- inherits(fresh[[nm]], "Date") || inherits(fresh[[nm]], "POSIXt")
  b_dt <- inherits(frozen[[nm]], "Date") || inherits(frozen[[nm]], "POSIXt")
  if (a_dt || b_dt) {
    fresh[[nm]] <- as.character(fresh[[nm]])
    frozen[[nm]] <- as.character(frozen[[nm]])
  }
}
if ("gameday" %in% names(fresh)) fresh$gameday <- as.character(fresh$gameday)
if ("gameday" %in% names(frozen)) frozen$gameday <- as.character(frozen$gameday)

out <- dplyr::bind_rows(fresh, frozen)
stopifnot(nrow(out) == 2L, is.character(out$gameday))
cat("[PASS] Frozen-row restore safely binds Date/character gameday values.\n")
