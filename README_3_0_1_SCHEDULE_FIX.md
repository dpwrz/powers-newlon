# Fantasy Model 3.0.1 — schedule fallback fix

This maintenance release prevents current-season projection refreshes from failing when `nflreadr::load_schedules()` times out.

Changes:
- `RUN_2_5_AUTO_REFRESH.R` falls back to the cached current player stats and schedule.
- `pipeline/11_project_weekly_2026.R` reuses the live/static schedule cache before making another remote request.
- The same projection pipeline now reuses the player-week cache written by the auto-refresh runner instead of downloading current stats a second time.
- `schedule_team_rows21()` now returns a typed empty schema rather than a columnless `data.frame()`.
- Added `tests/TEST_3_0_SCHEDULE_FALLBACK.R`.

Run:
```r
setwd("C:/Users/dpowe/OneDrive/Documents/Powers-Newlon Model")
source("tests/TEST_3_0_SCHEDULE_FALLBACK.R")
Sys.setenv(FM_FORCE_REFRESH = "true")
source("RUN_AUTO_REFRESH.R")
```
