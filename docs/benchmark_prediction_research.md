# Prediction research protocol

Use final reconciled player-week outcomes and immutable pre-kickoff feature snapshots. Do not train on realized volume, snap share, final injuries or closing lines unless known at capture time. Keep archived production and competitor forecasts unchanged.

## Priority experiments
1. QB: expected dropbacks, designed rush and scramble rates, pressure susceptibility versus opposing pressure, offensive line availability, implied team scoring, and receiver availability.
2. RB: lagged snap and carry share, high-value touches inside the five, targets and two-minute role, backfield injuries and implied game script.
3. WR: lagged route participation (only where available before kickoff), targets per route, air-yards share, alignment/matchup interaction, teammate availability and QB efficiency.
4. TE: route participation versus blocking, red-zone target share, teammate availability, and team pass volume.
5. All: decayed opportunity trends, opponent strength adjusted for prior opponents, injury/role uncertainty, expected touchdowns with shrinkage, lagged player residuals, weather and market context as-of capture.

## Evaluation
Use expanding chronological folds and keep all records from each test week together. Fit features and calibration only on earlier weeks. Evaluate direct incumbent, structured challenger, and residual correction separately. Compare incremental ablations on identical test cohorts, report MAE/RMSE/bias, starter and relevant score accuracy, rank/top-N, start/sit, and by-position results. Use paired week-block bootstrap uncertainty across sufficiently many historical weeks; do not promote based on one week. Predeclare promotion gates, require improved held-out MAE without meaningful degradation in decision metrics, and keep incumbent if uncertain.

## Data contracts
Preserve season, week, stable player ID, team, opponent, game ID, exact capture timestamp, data source and feature availability time. Archive component estimates including receptions for full-PPR conversion. Missing features carry explicit indicators, not invented numbers. Log identity rejections, actual-game coverage and forecast coverage. Distinguish actual top-N starter analysis from pregame projected starter analysis; add a common ex-ante starter cohort for actionable evaluation. A player absent from offensive statistics is not automatically a zero: establish active/inactive status separately. Never use opponent projections to train an independent challenger without a separate labeled ensemble experiment.
