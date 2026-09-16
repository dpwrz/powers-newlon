# Fantasy Model 3.0.6 hotfix — portable audit safety

- Makes the 2.4 row-level decision-error audit optional rather than a blocker.
- Adds a memory-safe column-select read for the 66 MB 2.4 validation-prediction file.
- Writes `output/decision_relevant_audit_status_3_0.csv` describing whether the full audit ran.
- Uses compact 2.4 cohort metrics as a lightweight fallback when available.
- Adds `runners/RESUME_3_0_AFTER_ROLE_PROJECTION.R` so completed role forecasts are not recomputed.
- No production projection methodology is changed.
