# Fantasy Model 2.4 Architecture

Fantasy Model 2.4 freezes Models 2.3/2.3.2 as honest weekly benchmarks and rebuilds the challenger around interpretable, pre-kickoff football signals.

## Core layers

1. **Player talent prior** — college production, combine athleticism, draft capital, age/career stage.
2. **Experience gate** — prospect information is strongest before NFL evidence exists and decays as NFL games accumulate.
3. **NFL role/opportunity** — projected pass attempts, carries, targets, snaps/routes, team volume and expected opportunity.
4. **Opponent style** — lagged blitz, pressure, 5+ rushers, man/zone, coverage family and box structure when available.
5. **Hierarchical response** — opponent-style effects are estimated at position, archetype and player levels with shrinkage toward the parent estimate.
6. **Signal attribution** — raw correlations are reported, but model weighting is based on unique out-of-sample value after controlling for the locked forecast and correlated features.
7. **Strict promotion gate** — 2.4 must improve MAE and RMSE, preserve starter MAE, and avoid material Pearson/rank-correlation losses before replacing the locked forecast for a position.

## Leakage rule

Every historical player-week is forecast only from information available before kickoff. College/combine/draft measurements are static pre-NFL priors. Defensive-style game observations are lagged. Player-response maps for a validation season are trained only on earlier seasons. Current-game outcomes never enter that game's features.

## Main outputs

- `output/rookie_talent_rankings_2026.csv`
- `output/rookie_talent_signal_audit_2_4.csv`
- `output/weekly_2_4_signal_attribution.csv`
- `output/weekly_2_4_signal_attribution_by_year.csv`
- `output/weekly_2_4_permutation_importance_by_year.csv`
- `output/weekly_2_4_player_response_summary.csv`
- `output/weekly_2_4_validation_metrics.csv`
- `output/weekly_2_4_cohort_metrics.csv`
- `output/weekly_2_4_confidence_calibration.csv`
- `output/weekly_2_4_locked_benchmark.csv`
- `output/weekly_2_4_promotion.csv`
- `output/weekly_2_4_model_quality_report.txt`
