# Fantasy Model 2.5 — Start Here

Copy this patch over the root of the existing Fantasy Model project. It does not delete or replace 2.4/3.0 code.

Run in this order:

```r
setwd("C:/Users/dpowe/OneDrive/Documents/Powers-Newlon Model")
source("INSTALL_MODEL_2_5.R")
source("tests/TEST_2_5_PREREQS.R")
source("runners/RUN_2_5_ACCURACY_TOURNAMENT.R")
```

Review:

- `output/weekly_2_5_validation_metrics.csv`
- `output/weekly_2_5_cohort_metrics.csv`
- `output/weekly_2_5_year_metrics.csv`
- `output/weekly_2_5_bootstrap_confidence.csv`
- `output/weekly_2_5_champion_manifest.csv`
- `output/weekly_2_5_model_quality_report.txt`

If the promotion table is sensible, either run the guarded live projection:

```r
source("pipeline/23_project_weekly_2026_2_5.R")
```

or rerun everything in one guarded command later:

```r
source("runners/RUN_2_5_COMPLETE.R")
```

The live step only changes positions that passed the strict 2.5 gate. All other positions keep the existing production champion.
