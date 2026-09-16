# Fantasy Model 2.5 Live — Current Stats Opponent Join Hotfix

## What this fixes
Once 2026 player-week stats exist, `normalize_weekly_stats21()` can already supply an
`opponent` column. `pipeline/11_project_weekly_2026.R` then joined the schedule, which
also supplied `opponent`. dplyr renamed the pair to `opponent.x` and `opponent.y`.
The next `filter(!is.na(opponent))` therefore failed because there was no column named
exactly `opponent`.

The patch now:
- renames the schedule field to `opponent_sched` before the join;
- coalesces the current-stat opponent with the schedule opponent into one canonical
  `opponent` column;
- declares the schedule join as many-to-one;
- declares the intentional player x schedule expansion as many-to-many, removing the
  harmless dplyr warning shown during projection-grid creation.

## Install
Extract this zip over the Fantasy Model project root and allow it to replace:

`pipeline/11_project_weekly_2026.R`

## Run
Restart R, then:

```r
setwd("C:/Users/dpowe/OneDrive/Documents/Powers-Newlon Model")
source("tests/TEST_CURRENT_STATS_OPPONENT_JOIN.R")
source("pipeline/23_project_weekly_2026_2_5.R")
```

You do not need to rerun the Model 2.5 accuracy tournament or feedback tournament.
