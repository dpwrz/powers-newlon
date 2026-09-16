# Validation and Promotion

Fantasy Model 2.4.3 preserves the project's honest historical evaluation chain. The 2.4 challenger is evaluated against the locked 2.3 / 2.3.2 out-of-sample history rather than replacing the benchmark with in-sample results.

## Active validation chain

1. The season foundation is built or reused.
2. Historical weekly features are reconstructed with pre-kickoff information only.
3. The 2.3 weekly/meta-calibration history is created or reused.
4. The 2.3.2 signal/error-feedback challenger is created or reused.
5. The 2.4 talent/context/response challenger is validated against that locked history.
6. Promotion gates determine which 2.4 components are allowed into production projections.

## Key output families

The authoritative validation artifacts remain in `output/`, including:

- `weekly_2_4_validation_metrics.csv`
- `weekly_2_4_validation_predictions.csv`
- `weekly_2_4_promotion.csv`
- `weekly_2_4_locked_benchmark.csv`
- `weekly_2_4_cohort_metrics.csv`
- `weekly_2_4_confidence_calibration.csv`
- `weekly_2_4_model_quality_report.txt`

Earlier 2.3 and 2.3.2 validation outputs are retained because they are part of the benchmark lineage used by 2.4.
