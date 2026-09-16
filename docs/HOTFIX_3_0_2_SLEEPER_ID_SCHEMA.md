# Fantasy Model 3.0.2 Hotfix — Sleeper identity schema

## Symptom
After selecting a Sleeper league, Setup could show:

`In argument: gsis_id. Caused by error: object 'gsis_id' not found`

## Root cause
The 3.0 identity bridge assumed every cached/live Sleeper player frame contained a literal `gsis_id` column.  Older cached league/player frames or alternate flattened cache schemas can omit or rename that field.  The identity mapper then referenced `gsis_id` inside `dplyr::mutate()` and stopped before it could fall back to name/position matching.

## Fix
- Normalize Sleeper player schemas before identity matching.
- Add safe aliases for `sleeper_id` and GSIS IDs.
- Migrate cached league-state player frames on load.
- Normalize the Sleeper table again at the dynasty-value boundary.
- Preserve name + position + team fallback matching when GSIS IDs are unavailable.

This changes application robustness only.  It does not modify the 2.4.3 projection model or the 3.0 role-model validation results.
