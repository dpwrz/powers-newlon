# Fantasy Model 2.4.2 hotfix

This hotfix repairs the Model 2.4 talent/context build after 2.4.1 reached the defensive-style stage.

## Fixes

- Uses the current nflverse participation identifiers `nflverse_game_id` and `play_id`/`nflverse_play_id` instead of assuming a `game_id` field.
- Uses the current FTN charting identifiers `nflverse_game_id` and `nflverse_play_id`.
- Invalid/empty defensive-style checkpoints are rebuilt instead of silently reused.
- FTN seasons with no valid defense key are skipped safely rather than failing a join.
- Adds defensive-stage progress diagnostics so a future source/schema failure is visible immediately.
- Expands NCAA-to-NFL identity matching with NFL display/legal/common/football-name aliases plus PFR draft-name aliases.
- Reuses existing NCAA checkpoints; no college redownload is required.
- Adds the NCAA `rec_td` field alias.

## Resume

Apply the root hotfix over the existing project, restart R, then run:

```r
source("RESUME_2_4_FROM_SIGNAL_AUDIT.R")
```

The completed 2.3.2 stores remain untouched. Step 1 will reuse college checkpoints and rebuild only invalid defensive-style checkpoints.
