# Hotfix 3.0.4 — Sleeper identity fallback join collision

## Symptom
`Tibble columns must have compatible sizes. Size 506: Existing data. Size 0: Column gsis_id.`

## Cause
The name + position fallback joined a Sleeper player frame and the model player frame while both still used the column name `gsis_id`. Because `gsis_id` was not a join key in that fallback, dplyr correctly renamed the two columns to suffixed variants. The result constructor then asked for a non-existent unsuffixed `gsis_id`, producing a zero-length vector next to the matched player rows.

## Fix
The model identifier is renamed to `model_gsis_id` before the fallback join and is explicitly copied into the canonical output `gsis_id`. The exact-GSIS path is unchanged.

No projection, dynasty-value, role-model, or validation methodology changes are included in this hotfix.
