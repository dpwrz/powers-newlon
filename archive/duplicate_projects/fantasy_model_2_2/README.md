# Fantasy Model 2.2

Fantasy Model 2.2 is the **Weekly Accuracy Engine**. It preserves the validated Model 2.0 season architecture as the preseason/long-horizon prior and rebuilds the weekly layer around decomposed opportunity, matchup, stat-line, and decision modeling.

## Core idea

2.1 proved weekly information helps but recent FPPG/volume dominated the monolithic learner. 2.2 prevents matchup signal from being drowned out by separating:

1. neutral player role,
2. expected opportunity,
3. shrunk neutral stat line,
4. explicit opponent matchup delta,
5. full direct weekly challenger,
6. chronological stack with the 2.0/role prior.

See `MODEL_2_2_ARCHITECTURE.md`.

## App

The included Shiny app adds dedicated `Home`, `Rankings`, `Weekly`, `Start/Sit`, `ROS`, `Player`, `Dynasty`, `League`, and `Quality` views. See `APP_2_2_DESIGN.md` for what changed and the next UX priorities.

## Existing Posit project: recommended path

Keep your existing `data/`, `output/`, `models/`, `settings/`, and PBP checkpoints. Copy the 2.2 project files over the project root, restart R, then run:

```r
source("TEST_2_2_PREREQS.R")
source("MOBILE_RUN_WEEKLY_FAST.R")
source("RUN_APP.R")
```

The fast weekly runner reuses `data/raw/weekly_defense_context_raw.csv`.

If the weekly defense checkpoint does not exist yet:

```r
source("MOBILE_RUN_WEEKLY.R")
```

## Clean project

A clean project needs to build the season prior before the weekly engine:

```r
source("MOBILE_RUN.R")
source("MOBILE_RUN_WEEKLY.R")
source("RUN_APP.R")
```

Or use:

```r
source("RUN_2_2_COMPLETE.R")
```

which builds the season foundation only when it is missing and then launches the 2.2 weekly build.

## Main 2.2 validation outputs

- `output/weekly_2_2_model_quality_report.txt`
- `output/weekly_2_2_validation_metrics.csv`
- `output/weekly_2_2_architecture_comparison.csv`
- `output/weekly_2_2_matchup_calibration.csv`
- `output/weekly_2_2_cohort_metrics.csv`
- `output/weekly_2_2_topn_accuracy.csv`
- `output/weekly_2_2_selected_stack.csv`
- `output/weekly_2_2_feature_importance.csv`

Compatibility copies are also written to the 2.1 filenames used by older dashboard code.

## Production outputs

- `output/weekly_2026_projections.csv`
- `output/week_<N>_rankings.csv`
- `output/rest_of_season_2026.csv`

## Optional engine research

The core runner stays lightweight and reliable on Posit Cloud. If `ranger` and/or `xgboost` are already installed, run:

```r
source("BENCHMARK_2_2_ENGINES.R")
```

This produces an honest challenger benchmark without changing production models.

## Important promotion rule

Do not assume 2.2 is better because it is more complex. Promote it only if the new weekly report improves the same historical player-week sample, especially the fantasy-relevant starter cohorts and rank/start-sit metrics.
