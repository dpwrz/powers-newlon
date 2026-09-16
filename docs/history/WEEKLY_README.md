# 2.3 Weekly Workflow

### Full weekly build
```r
source("MOBILE_RUN_WEEKLY.R")
```
This builds/refreshes PBP defense context, player-week features, honest 2.3 validation/training, and 2026 weekly/ROS projections.

### Normal in-season refresh
```r
source("MOBILE_RUN_WEEKLY_FAST.R")
```
The fast runner reuses the compact historical PBP checkpoint.

### Resume helpers
```r
source("RESUME_2_3_WEEKLY_FROM_STEP2.R")
source("RESUME_2_3_WEEKLY_FROM_STEP3.R")
source("RESUME_2_3_WEEKLY_FROM_STEP4.R")
```
Use the latest completed stage to avoid rebuilding expensive work.

### Leakage rule
Current-week outcomes, snaps, NGS and PBP can never become predictors for that same historical player-week. Historical calibration uses only prior out-of-sample prediction errors.


## Fantasy Model 2.3.2 signal + feedback upgrade
If Model 2.3 has already completed, do not rebuild the historical weekly/PBP stores. Run:

```r
source("TEST_2_3_2_PREREQS.R")
source("RESUME_2_3_2_FROM_SIGNAL.R")
```

2.3.2 learns stable pre-kickoff signals that explain remaining OOF error, adds expected-production gap features, and tests a bounded PID-like prior-error controller. A position is used live only if it passes the honest 2.3 promotion gate. See `MODEL_2_3_2_ARCHITECTURE.md`.

## 2.4 signal-first workflow

For a completed 2.3.2 project:

```r
source("TEST_2_4_PREREQS.R")
source("RESUME_2_4_FROM_SIGNAL_AUDIT.R")
```

This reuses the historical weekly/PBP stores, builds the new player-talent and defense-style contexts, runs honest 2.4 signal attribution/validation, and refreshes 2026 projections only for positions that pass the promotion gate.
