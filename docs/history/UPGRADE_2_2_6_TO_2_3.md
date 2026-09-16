# Upgrade 2.2.6 → 2.3

This is a modeling revision, not a bug hotfix. Existing season data, weekly PBP context, NGS checkpoints, saved leagues and outputs can be retained.

After copying the root patch into the existing project and restarting R:

```r
source("RESUME_2_3_WEEKLY_FROM_STEP3.R")
```

Use that path if `data/processed/weekly_model_table_2_2.csv` or `_2_3.csv` already exists. It reuses the historical feature store and reruns only 2.3 validation/training plus live projection.

For a normal full weekly refresh:

```r
source("MOBILE_RUN_WEEKLY_FAST.R")
```

For a clean project:

```r
source("RUN_2_3_COMPLETE.R")
```
