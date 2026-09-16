# Fantasy Model 3.0 Hotfix — Role Validation Target Scope

## Symptom

During `pipeline/18_train_validate_role_models_3_0.R`, validation could stop on the first target with:

`Can't extract column with target. Subscript target must be size 1, not <n>.`

## Cause

`tibble()` evaluates columns sequentially. The OOF constructor created a column named `target` and then evaluated `test[[target]]` in the same call. The newly created `target` column masked the scalar loop variable containing the target column name (for example, `"pass_attempts"`).

## Fix

- Capture `target_name` and `actual_target` before constructing the tibble.
- Use explicit `.data` / `.env` references in walk-forward position/year filters.
- Harden `fm30_fit_role_model()` against dynamic-column masking by resolving the position and target names before filtering.
- Mark the historical injury join as intentionally many-to-many; it is reduced to the latest prior player-week immediately afterward.

This hotfix changes execution safety only. It does not change the promotion thresholds or production 2.4.3 forecast.
