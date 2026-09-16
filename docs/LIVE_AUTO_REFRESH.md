# Fantasy Model 2.5 Live / Automatic Refresh

This layer does **not** retrain the weekly model on every poll. It keeps the validated position champions frozen and updates their inputs whenever public current-season state changes.

## State flow

1. Poll current-season nflverse player-week stats and schedule/lines.
2. Compare a compact fingerprint with the last successful run.
3. If nothing changed, exit quickly.
4. If a game/stat/line changed, refresh current-season defensive PBP context when appropriate.
5. Run the existing 2.5 live scorer (`pipeline/23_project_weekly_2026_2_5.R`).
6. Rewrite **every unplayed week through Week 18** and ROS.
7. Freeze the latest forecast for each player before his game becomes actual.
8. Score completed player-weeks (MAE, RMSE, bias, correlation, starter MAE, interval coverage, start/sit pair accuracy).
9. Export `web/public/data/model_snapshot.json`.
10. GitHub Actions commits only compact live state and the website snapshot. Cloudflare redeploys automatically from the push.

## What “live” means on the free stack

This is post-game / newly-published-data live. It is not a true every-play NFL feed. A true in-game play model should be a separate model trained on partial-game state, not the completed-game weekly model.

## Local manual run

```r
setwd("C:/Users/dpowe/OneDrive/Documents/Powers-Newlon Model")
source("tests/TEST_2_5_LIVE_AUTOMATION.R")
Sys.setenv(FM_FORCE_REFRESH = "true")
source("runners/RUN_2_5_AUTO_REFRESH.R")
```

After the first forced run, normal runs can omit `FM_FORCE_REFRESH`; the runner will no-op if the public state has not changed.
