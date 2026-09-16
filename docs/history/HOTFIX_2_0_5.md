# Fantasy Model 2.0 - Hotfix 5

## Failure fixed
During 2023 QB walk-forward validation the residual model failed with missing features such as:
- `projected_target_share`
- `projected_targets_pg`
- `projected_carry_share`

The 2022 out-of-fold rows from all positions had been combined with `dplyr::bind_rows()`. That produces a union of every column name. A QB-only slice therefore still contained WR/RB component column *names*, even though their QB values were all missing. The old residual feature selector used column-name presence alone, so it could train a QB residual tree with position-inapplicable fields. A standalone 2023 QB prediction frame correctly did not contain those fields and prediction stopped.

## Hotfix 5 behavior
- Residual component features are now explicitly position-specific.
- All-missing and constant residual features are removed before training.
- Residual model prediction defensively aligns the new frame to the trained schema and uses the same zero-imputation convention as the underlying tree engine if a legitimate trained feature is unexpectedly absent.
- Hotfix 4's first-holdout safe-start remains included.

## Run
Steps 1 and 2 already succeeded. Restart R, overwrite the patch at the project root, then run:

```r
source("RESUME_2_0_FROM_STEP3.R")
```

The validator must restart Step 3 from 2022 because the failed validation stage had not yet written its final OOF files. It does not rebuild the opportunity store or the 1.2 direct OOF predictions.
