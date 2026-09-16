# Fantasy Model 2.0 Hotfix 1

Fixes current nflverse interception schema compatibility. Current player stats expose `passing_interceptions`; inherited 1.x code expected `interceptions`, which could silently create all-zero QB interceptions. This affected QB fantasy scoring and caused the 2.0 target guardrail to stop Step 1.

The fast runner now detects a stale direct feature store and rebuilds `02_build_features.R` once using the correct alias. It reuses the existing compact PBP context files; raw PBP is not rebuilt.

After applying the patch, restart R and run:

```r
source("MOBILE_RUN_FAST.R")
```

Expected marker:

`[HOTFIX 1] Stale QB interception scoring detected...`

The corrected 2.0 run will re-establish the direct QB baseline, so old QB validation numbers from 1.x are not perfectly comparable. RB/WR/TE scoring is unaffected by this specific alias correction.
