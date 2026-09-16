# Fantasy Model 3.0 pre-Week-1 hotfix

## Problem
When `nflreadr::load_player_stats()` returns no current-season weekly rows before the first games are complete, the normalized current-week table previously became a zero-column `data.frame()`. The 2.3.2 live controller then attempted `filter(position == pos)`, causing `object 'position' not found`.

## Fix
- `normalize_weekly_stats21()` now returns a typed zero-row weekly table with canonical columns.
- `13_project_weekly_2026_2_3_2.R` explicitly treats an empty current season as an empty live PID state.
- No projection methodology or trained model is changed. This is execution-safety only.

## Expected behavior
Before Week 1, the model proceeds with no current-season PID feedback. Once completed 2026 player-week data exists, the controller resumes using prior observed errors normally.
