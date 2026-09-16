# Fantasy Model 3.0 + Web 1.0

This is the consolidated production release. It folds the validated Model 2.5 accuracy tournament, forward-only live refresh, final-forecast error feedback, league-specific Sleeper scoring, live game-state support, Start/Sit, weekly performance reporting, dynasty/GM modules, and the Web 1.0 product surface into one clean project.

## Production behavior

- Completed player-weeks are immutable. They are never re-predicted.
- Only unplayed player-weeks are re-scored when new information arrives.
- ROS is actual points to date plus projections for remaining player-weeks.
- Final pregame projections are archived before games and used for weekly MAE/RMSE/bias/rank/performance reporting.
- Realized final-model error can influence later projections only through the chronologically validated, bounded feedback layer.
- Sleeper supplies league state and scoring settings only; Fantasy Model supplies football projections and decision intelligence.
- Realtime individual-player game updates are optional and use the server-side live provider adapter. Without that provider the site still has Sleeper matchup state, league-specific pregame projections, postgame auto-refresh, Start/Sit and performance reporting.

## Versioning note

The release is **Fantasy Model 3.0.0** and **Web 1.0.0**. Internal files retain historical component names such as `weekly_engine_25.R` and `pipeline/23_project_weekly_2026_2_5.R` so validation artifacts and lineage remain auditable. The supported production entrypoints are the new `RUN_3_0_*` runners.
