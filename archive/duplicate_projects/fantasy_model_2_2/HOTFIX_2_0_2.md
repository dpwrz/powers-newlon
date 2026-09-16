# Fantasy Model 2.0 - Hotfix 2

This hotfix removes the failing full `02_build_features.R` repair path.

Instead, `REPAIR_QB_SCORING_2_0.R` surgically repairs the existing validated 1.2 feature store using the raw nflverse `passing_interceptions` field. It:

- preserves the existing 1.2 model/projection feature tables as backups;
- repairs historical QB interception counts and target fantasy scoring;
- repairs QB prior/two-year FPPG inputs used by the direct guardrail;
- repairs 2026 QB lagged fantasy inputs;
- is marker-protected so it cannot double-apply the interception penalty;
- does not rebuild PBP or the full 1.2 feature pipeline.

After installing the root-level patch, restart R and run:

```r
source("MOBILE_RUN_FAST.R")
```
