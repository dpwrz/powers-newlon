# Fantasy Model 2.0 - Hotfix 3

This is an execution/data-construction hotfix. The Model 2.0 statistical architecture is unchanged.

## Fixes

- Rebuilds historical RB carry-share targets directly from raw nflverse player/team totals rather than depending on the inherited model-table `team` field.
- Collapses player usage to one row per season/team/player before computing shares.
- Collapses the carry-share target to one row per player-season before joining it to the model table.
- Makes the teammate comparison join explicitly many-to-many and immediately summarizes it back to one player-season row.
- Adds fallback team carry/target denominators from player-stat totals if the compact team-stat denominator is unavailable.
- Adds stronger diagnostics showing RB carry-share and QB interception-rate variation.
- Adds `TEST_2_0_OPPORTUNITY_STEP.R` so Step 1 can be tested before rerunning all model stages.

## Run

Restart R after overwriting the files, then run:

```r
source("TEST_2_0_OPPORTUNITY_STEP.R")
```

If it passes, run:

```r
source("MOBILE_RUN_FAST.R")
```
