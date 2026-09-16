# Fantasy Model 3.0 production architecture

## Persistent model layer

The validated production core is position-specific. The guarded 2.5 direct challenger only enters a position where the honest chronological promotion gate passed; otherwise the prior incumbent remains production. A separate final-forecast error-feedback model may adjust later unplayed player-weeks only where its own chronological validation passed.

## Forward-only seasonal state

1. Poll current player-week stats and schedule.
2. Score newly completed games against the last frozen pregame forecast.
3. Add actual football state and final-model error to historical state.
4. Freeze completed player-weeks permanently.
5. Re-score only unplayed player-weeks.
6. Rebuild ROS as actual points to date + projected remaining points.
7. Export weekly performance and the Web 1.0 snapshot.

A partial NFL week is handled at player-week granularity: completed Thursday players are frozen while unplayed Sunday/Monday players remain prospective.

## Live website layer

Web 1.0 reads Sleeper league/roster/scoring state but never Sleeper rankings, ADP or projections. It applies the selected Sleeper league scoring rules to Fantasy Model structured stat-line output. Optional realtime player statistics are fetched server-side by the Cloudflare Pages Function adapter; secrets never enter the browser bundle.

## User-facing surfaces

Web 1.0 includes Home, Rankings, Weekly, Season Outlook, Player, Start/Sit, Dynasty, Quality, Performance, Live, My Team, League, Draft Room, Trade Center, AI GM, Data Health and Setup.
