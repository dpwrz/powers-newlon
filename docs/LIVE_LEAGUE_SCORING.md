# Model 2.5 Live — league scoring and in-game state

The production weekly champion remains the validated Model 2.5 position-specific stack. League scoring is a post-model stat-line transformation:

`league projection = validated base projection + score(projected stat line, league rules) - score(projected stat line, base rules)`

This preserves the learned residual/calibration signal instead of replacing the model with a simple fantasy-point calculator.

The in-game state filter is deliberately separate. It consumes cumulative live game statistics and estimates expected final opportunities/stat line with bounded pace and efficiency updates. The live role surprise can affect near-term future weeks in the UI, but those changes are temporary until finalized game stats enter the normal R pipeline.

R helpers:
- `R/league_scoring_engine_25.R`
- `R/live_game_state_engine_25.R`
- `tests/TEST_2_5_LIVE_SCORING.R`

The realtime transport is implemented in the website/Cloudflare layer because GitHub Actions are not a suitable per-play execution environment.
