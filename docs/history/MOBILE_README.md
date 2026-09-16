# Posit Cloud / iPhone — Fantasy Model 2.3

For a clean project:
```r
source("RUN_2_3_COMPLETE.R")
```

For an existing project with the season foundation and weekly PBP checkpoint:
```r
source("MOBILE_RUN_WEEKLY_FAST.R")
```

After completion:
```r
source("RUN_APP.R")
```

The heavy stages run in isolated R processes through `RUN_STAGE_20.R` to stay within Posit Cloud memory limits. NGS remains checkpointed by season/type. 2.3 adds no compilation-heavy required package; the meta learner uses base-R constrained grids and ridge linear algebra.


## Fantasy Model 2.3.2 signal + feedback upgrade
If Model 2.3 has already completed, do not rebuild the historical weekly/PBP stores. Run:

```r
source("TEST_2_3_2_PREREQS.R")
source("RESUME_2_3_2_FROM_SIGNAL.R")
```

2.3.2 learns stable pre-kickoff signals that explain remaining OOF error, adds expected-production gap features, and tests a bounded PID-like prior-error controller. A position is used live only if it passes the honest 2.3 promotion gate. See `MODEL_2_3_2_ARCHITECTURE.md`.
