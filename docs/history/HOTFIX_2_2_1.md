# Fantasy Model 2.2.1 Hotfix

## Fixed
`02_build_features.R` used `cummean()` without a namespace inside an isolated Rscript stage. `ensure_packages()` verifies the dplyr namespace but intentionally does not attach the package, so a clean project could fail at Step 2/12 with the traceback ending in `dplyr::lag(cummean(fppg), 1)`.

The expression is now:

```r
dplyr::lag(dplyr::cummean(fppg), 1)
```

No model logic or validation methodology changed.

## Resume after this failure
Restart R and run:

```r
source("RESUME_2_2_FROM_STEP2.R")
```

After the season foundation finishes, run:

```r
source("RUN_2_2_COMPLETE.R")
```

The complete helper will detect the completed season files and proceed into the weekly 2.2 build.
