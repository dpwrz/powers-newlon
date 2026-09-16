# Fantasy Model 2.2.2 Hotfix

This hotfix hardens the optional weekly Next Gen Stats (NGS) ingestion path in `09_build_weekly_data.R`.

## Why
The weekly build could terminate during the historical NGS stage before printing a normal R traceback. NGS is useful but optional and should never prevent the core weekly model from running.

## Changes
- Loads NGS one season and one stat type at a time.
- Writes compact per-season/type checkpoints under `data/raw/ngs_2_2_checkpoints/`.
- Reuses successful checkpoints on later runs.
- Wraps both NGS download and normalization in error handling.
- Skips malformed/unavailable NGS slices instead of failing the weekly build.
- Keeps NGS missingness as missing/availability signal; core role, opportunity, matchup, PBP-defense and game-environment features continue normally.
- Adds `RESUME_2_2_WEEKLY_FROM_STEP2.R` so a completed weekly PBP defense step does not need to be rerun.

## Run
After applying the root patch and restarting R:

```r
source("RESUME_2_2_WEEKLY_FROM_STEP2.R")
```
