# Fantasy Model 2.2 Weekly Runbook

## First weekly build

```r
source("TEST_2_2_PREREQS.R")
source("MOBILE_RUN_WEEKLY.R")
```

Stages:

1. resumable weekly PBP defense context
2. strict pre-kickoff player-week feature store
3. chronological validation + final weekly models
4. 2026 weekly forecasts + rest of season

## Normal refresh after the historical PBP checkpoint exists

```r
source("MOBILE_RUN_WEEKLY_FAST.R")
```

This reuses historical PBP defense summaries and rebuilds the live player-week state, validation/models, and 2026 projections.

## Current-season defense-only refresh

```r
source("REFRESH_WEEKLY_DEFENSE_CURRENT.R")
```

This refreshes only the current-season compact PBP defense checkpoint and then recombines it with the historical store.

## Launch app

```r
source("RUN_APP.R")
```

## What to send back after validation

The most useful files for the next accuracy pass are:

- `output/weekly_2_2_model_quality_report.txt`
- `output/weekly_2_2_validation_metrics.csv`
- `output/weekly_2_2_architecture_comparison.csv`
- `output/weekly_2_2_matchup_calibration.csv`
- `output/weekly_2_2_cohort_metrics.csv`
- `output/weekly_2_2_opportunity_metrics_by_year.csv`
- `output/weekly_2_2_feature_importance.csv`
- `output/weekly_2_2_selected_stack.csv`

## Why there are multiple weekly models

- `neutral`: player role/quality without opponent features
- `structured`: predicts opportunity then reconstructs a shrunk stat line
- `matchup`: learns the opponent-specific residual adjustment
- `direct`: all legal pregame features in one direct challenger
- `stack`: combines the architectures only when historical OOF results justify the weights
