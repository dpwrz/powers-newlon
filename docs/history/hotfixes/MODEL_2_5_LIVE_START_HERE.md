# Fantasy Model 2.5 Live + Web 0.6

This overlay adds automatic current-season refresh, a persistent final-pregame forecast archive, live weekly accuracy scoring, production-vs-incumbent tracking, website Start/Sit, and a weekly Performance scorecard.

## First local run (Windows)

From the Fantasy Model root:

```r
setwd("C:/Users/dpowe/OneDrive/Documents/Powers-Newlon Model")
source("tests/TEST_2_5_LIVE_AUTOMATION.R")
Sys.setenv(FM_FORCE_REFRESH = "true")
source("runners/RUN_2_5_AUTO_REFRESH.R")
```

The runner **does not retrain** Model 2.5. It uses the already validated position champions. When new public current-season state appears it:

1. caches the new player-week/schedule state;
2. updates current defensive PBP context after completed games;
3. freezes completed player-weeks, then rebuilds only the current unplayed/future forecasts through Week 18 and ROS;
4. freezes final pregame forecasts without overwriting players whose games have started;
5. scores completed weeks;
6. compares live 2.5 error with the incumbent forecast it replaced;
7. updates the website snapshot.

The 2.3.2 live feedback controller uses the compact frozen pregame archive, so prior completed-week forecast errors continue to affect future projections in automated GitHub runs.

## Website check

```powershell
cd "C:\Users\dpowe\OneDrive\Documents\Powers-Newlon Model\web"
npm run build
npm run dev:full
```

Web 0.6 adds:

- **Start / Sit** — connected-roster lineup optimizer plus 2–4 player comparison using Fantasy Model projections only.
- **Performance** — weekly production MAE/RMSE/bias, starter/relevant MAE, close-call Start/Sit accuracy, interval calibration, position splits, biggest misses and live 2.5-vs-incumbent error improvement.
- Existing **Season Outlook** continues to show all W1–W18 projections and movement versus the prior published snapshot.

## GitHub + Cloudflare

See `docs/GITHUB_CLOUDFLARE_LIVE_SETUP.md`.

## Data timing

This free stack is automatic after nflverse publishes new player-week/PBP state. It is not a true play-by-play model. Do not interpret a scheduled poll as an in-game projection update.

## Web 0.7 league scoring + realtime overlay

This patch also adds `R/league_scoring_engine_25.R` and `R/live_game_state_engine_25.R`. Run the smoke test after overlaying the patch:

```r
source("tests/TEST_2_5_LIVE_SCORING.R")
```

The website applies connected Sleeper `scoring_settings` to the structured weekly stat line. True in-game player-stat updates run through Cloudflare, not GitHub Actions; see `docs/LIVE_LEAGUE_SCORING.md` and `web/LIVE_REALTIME_SETUP.md`.

## Forward-only + final-error feedback update
Completed player-weeks are now frozen permanently. Only unplayed player-weeks are reprojected. ROS is computed as actual points from completed rows plus projections for remaining rows.

The final Model 2.5 forecast's own historical error can also feed future forecasts, but only after an honest validation gate. Run this once after the normal 2.5 accuracy tournament:

```r
source("runners/RUN_2_5_FEEDBACK_TOURNAMENT.R")
source("tests/TEST_2_5_ERROR_FEEDBACK.R")
```

The automatic refresh then scores newly completed games *before* rebuilding future weeks, so the newest completed-week error is available immediately for the next projection pass. See `docs/FINAL_ERROR_FEEDBACK.md`.
