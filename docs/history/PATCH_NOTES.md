# Patch Notes — 2.3

- Promotes project label to Fantasy Model 2.3 (experimental until validated).
- Adds `R/weekly_engine_23.R`.
- Reconstructs the 2.1 weekly model as a chronological challenger.
- Calibrates raw 2.2 matchup deltas from prior OOF forecast errors, separately for positive/negative matchups.
- Adds phase-aware OOF meta stacking across 2.1, prior, neutral, calibrated structured and full-direct predictions.
- Adds a ridge residual correction that is automatically disabled unless prior inner validation improves MAE.
- Adds a final 2.1/2.2/2.3 cross-version guardrail.
- Adds starter-aware tuning objective, projection-tier diagnostics, confidence calibration and projection snapshot history.
- Updates the app for confidence, historical expected error, raw-vs-calibrated matchup effects, residual correction and version weights.
