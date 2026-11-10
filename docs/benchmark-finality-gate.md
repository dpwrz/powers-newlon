# Weekly benchmark finality fix

The frozen competitor archive contains 7,441 rows; the earlier empty result was a large-file fetch limitation.

## Root cause

`live25_completed_mask()` in `R/live_refresh_engine_25.R` previously treated finite home/away **scores** as proof that a game was complete when `result` was unavailable. Live games can have scores. This prematurely admits partial player-week statistics into `pipeline/24_score_live_accuracy_2_5.R`, which already uses the completion mask.

The patch **fails closed**: games are complete only when the nflverse schedule has a numeric final `result`. When no final result exists, the game is not scored. This avoids rebuilding completed weeks and protects pregame forecasts.

The accuracy table's `captured_at` is the **pregame projection capture time**, not the actuals ingestion time. Earlier assertions that this timestamp proved premature actuals were mistaken. The validation script now checks schedule final results instead.

## Verification

Run the refresh after nflverse has published all final results. Use `python scripts/check_benchmark_finality.py --week N --last-kickoff-utc YYYY-MM-DDTHH:MM:SSZ` to validate archive timestamps, schedule completion, and scored-team membership before reporting. This script needs the checked-out `data/raw/schedules_live_2026.csv`.

This is a code fix awaiting integration/production validation; no historical leaderboard is asserted. If a provider has no frozen rows, report it as unavailable. Historical actuals generated under the old score-based completion rule should be regenerated from final weekly stats.
