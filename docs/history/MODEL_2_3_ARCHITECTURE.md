# Fantasy Model 2.3 — Historical Forecast Learning

## Goal
2.3 turns the model's own historical out-of-sample predictions into a new source of signal. It does **not** train on in-sample fitted values. Every historical tuning decision for a holdout season uses only predictions/outcomes from earlier holdout seasons.

## Production architecture

1. **Model 2.0 season prior** — long-horizon anchor.
2. **Reconstructed Model 2.1 challenger** — monolithic weekly model retained because 2.1 beat 2.2 honest history at QB/WR/TE.
3. **Model 2.2 neutral/direct/opportunity models** — retained as complementary signals.
4. **Historical matchup calibration** — raw 2.2 matchup deltas are shrunk separately for favorable and difficult matchups using prior OOF residuals.
5. **Phase-aware meta stack** — learns different prediction weights for W1-3, W4-7, W8-12 and W13-18. Inputs are 2.1, prior, neutral, calibrated structured and full direct predictions.
6. **Guarded ridge residual calibration** — predicts remaining historical error from model disagreement, projection level, role trend, matchup delta, implied points, spread and season phase. It is enabled only when an earlier-season inner holdout improves MAE and is tightly capped.
7. **Cross-version guardrail** — compares reconstructed 2.1, raw chronological 2.2 and candidate 2.3 using prior OOF history. A newer architecture cannot force itself into production merely because it is newer.
8. **Empirical distribution + confidence** — final OOF residuals power floor/ceiling/boom/bust ranges; disagreement history maps current model spread to expected absolute error and High/Medium/Low confidence.

## Why this exists
2.2 validation showed:
- the decomposed architecture added signal;
- RB improved honestly;
- 2.1 remained slightly better for QB/WR/TE;
- raw matchup direction was useful but magnitude was too aggressive, especially favorable QB/WR matchups;
- all-history 2.2 stack selection looked better than the rolling score, suggesting historical forecast behavior is valuable but must be learned without leakage.

## Strict chronology
For 2023, 2.3 can tune only from 2022 OOF forecasts. For 2024 it can use 2022-2023. For 2025 it can use 2022-2024. The final 2026 configuration may use all completed 2022-2025 OOF history.

The first holdout uses the conservative prior because no historical OOF calibration exists yet.

## Starter-aware objective
Meta and version weights primarily minimize overall MAE while giving a modest extra weight to starter-cohort MAE. Starter status is determined from pregame baseline rank, never the actual weekly finish.

## New outputs
- `output/weekly_2_3_validation_predictions.csv`
- `output/weekly_2_3_validation_metrics.csv`
- `output/weekly_2_3_version_comparison.csv`
- `output/weekly_2_3_matchup_calibration.csv`
- `output/weekly_2_3_matchup_calibration_parameters.csv`
- `output/weekly_2_3_selected_legacy_weights.csv`
- `output/weekly_2_3_selected_meta_weights.csv`
- `output/weekly_2_3_selected_version_weights.csv`
- `output/weekly_2_3_rolling_meta_weights.csv`
- `output/weekly_2_3_rolling_version_weights.csv`
- `output/weekly_2_3_rolling_residual_calibration.csv`
- `output/weekly_2_3_cohort_metrics.csv`
- `output/weekly_2_3_validation_by_phase.csv`
- `output/weekly_2_3_error_by_projection_tier.csv`
- `output/weekly_2_3_confidence_calibration.csv`
- `output/weekly_2_3_residual_pool.csv`
- `output/weekly_2_3_model_quality_report.txt`

## Interpretation rule
Do not promote 2.3 because its final 2026 selected weights look good. Promotion is based on `weekly_2_3_validation_metrics.csv` and `weekly_2_3_version_comparison.csv`, especially starter/relevant cohorts. Because 2.3 was designed after inspecting the same 2022-2025 2.2 errors, prospective 2026 forecast tracking is the strongest next test.
