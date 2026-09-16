# Fantasy Model 2.2 on iPhone / Posit Cloud

The project is intentionally split into isolated R processes so memory is returned after each major stage.

## Existing project

```r
source("TEST_2_2_PREREQS.R")
source("MOBILE_RUN_WEEKLY_FAST.R")
```

If the prerequisite check reports that the weekly PBP defense store is missing, run:

```r
source("MOBILE_RUN_WEEKLY.R")
```

## Clean project

```r
source("MOBILE_RUN.R")
source("MOBILE_RUN_WEEKLY.R")
```

## App

```r
source("RUN_APP.R")
```

## Failure logs

The weekly runners print the tail of the failed stage automatically. Full logs live under `logs/weekly_stage_*.log` or `logs/weekly22_stage_*.log`.

## Do not rebuild historical PBP every week

Once `data/raw/weekly_defense_context_raw.csv` and the season checkpoints exist, use the fast runner. Only the current-season checkpoint needs refreshing as new games are played.
