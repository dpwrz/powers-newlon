# Fantasy Model 2.5 Live + Web 0.7

This overlay upgrades the existing Model 2.5 Live / Web 0.6 production stack with:

- exact connected-Sleeper-league scoring conversion for Fantasy Model projections,
- a Live game center,
- optional realtime NFL player-stat integration through a server-side provider adapter,
- a conservative live rest-of-game model,
- bounded/decaying temporary future-week impacts,
- existing Start/Sit and Performance views,
- the existing automatic forward-only postgame/ROS/accuracy pipeline.

Start with:

- `MODEL_2_5_LIVE_START_HERE.md`
- `docs/EXISTING_GITHUB_CLOUDFLARE_UPGRADE_0_7.md`
- `docs/LIVE_LEAGUE_SCORING.md`
- `web/LIVE_REALTIME_SETUP.md`

## 0.7 forward-only / error-feedback refinement
- completed player-weeks are immutable and are never re-predicted
- partial NFL weeks are handled player-by-player (completed Thursday players stay frozen while Sunday/Monday players remain prospective)
- ROS = completed actuals + remaining future projections
- the final 2.5 frozen pregame error is now eligible to become lagged state for future forecasts
- final-error point corrections are position-specific and disabled unless they pass their own chronological validation tournament
- recent realized absolute error also updates expected-error calibration for future unplayed rows
