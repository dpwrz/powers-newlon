# Fantasy Model 2.0 — Hotfix 4

Fixes the first-iteration walk-forward validation crash in `04b_validate_2_0.R`.

## Cause
On the first validation season/position, the out-of-sample history list is empty. `dplyr::bind_rows(rows)` therefore has no columns, but the validator immediately tried to filter `position` and `target_year`. The intended safe-start logic was never reached.

## Fix
The validator now creates a typed empty prior-OOF table when no earlier holdouts exist. That correctly triggers:
- identity calibration,
- no residual correction,
- 100% Model 1.2 guardrail weight
for the first holdout.

## Resume
If Step 1 and Step 2 already completed, restart R and run:

```r
source("RESUME_2_0_FROM_STEP3.R")
```

You do not need to rebuild the opportunity feature store or the direct OOF baseline.
