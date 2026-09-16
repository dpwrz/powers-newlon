# Fantasy Model 2.2.4 Hotfix

This hotfix fixes the 2.2.3 weekly resume runner. `RUN_STAGE_20.R` requires two trailing arguments: the stage script and its logfile. The 2.2.3 resume runner accidentally passed only the script and attempted to redirect stdout/stderr externally, so Step 2 failed before `09_build_weekly_data.R` actually started.

2.2.4 passes both required arguments and leaves logging to the existing stage wrapper, matching all other production runners. No modeling, feature, data, or validation logic changes. Existing season, PBP-defense, and NGS checkpoints remain reusable.

After applying the root patch and restarting R, run:

```r
source("RESUME_2_2_WEEKLY_FROM_STEP2.R")
```
