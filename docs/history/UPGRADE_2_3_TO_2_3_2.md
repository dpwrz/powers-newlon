# Upgrade Fantasy Model 2.3 -> 2.3.2

Do not delete your project or rebuild PBP/weekly features.

1. Extract the root patch over the existing Fantasy Model 2.3 project.
2. Restart R.
3. Run:

```r
source("TEST_2_3_2_PREREQS.R")
source("RESUME_2_3_2_FROM_SIGNAL.R")
```

The upgrade reuses `output/weekly_2_3_validation_predictions.csv` and the existing weekly feature store. It runs only the new signal/error-feedback validation and then refreshes 2026 projections.

After completion:

```r
source("RUN_APP.R")
```

If 2.3 has not yet completed, use:

```r
source("RUN_2_3_2_COMPLETE.R")
```
