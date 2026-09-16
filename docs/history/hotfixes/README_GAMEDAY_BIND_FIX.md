# Model 2.5 Live — frozen `gameday` bind hotfix

Fixes:

```
Error in dplyr::bind_rows():
! Can't combine ..1$gameday <character> and ..2$gameday <date>.
```

The error occurs after the upstream 2.4 refresh when Model 2.5 restores already-completed player-weeks. The refreshed schedule can carry `gameday` as a Date, while the previously written weekly CSV is read back as character.

The patched `pipeline/23_project_weekly_2026_2_5.R` normalizes shared Date/POSIX columns before the frozen-row bind and explicitly keeps `gameday` as character. Completed player-weeks remain immutable; only unplayed rows are reprojected.

After copying over the project root, run:

```r
setwd("C:/Users/dpowe/OneDrive/Documents/Powers-Newlon Model")
source("tests/TEST_2_5_FROZEN_GAMEDAY_BIND.R")
Sys.setenv(FM_FORCE_REFRESH = "true")
source("runners/RUN_2_5_AUTO_REFRESH.R")
```

No accuracy or feedback retraining is required.
