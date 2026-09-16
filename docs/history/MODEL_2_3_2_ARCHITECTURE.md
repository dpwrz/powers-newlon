# Fantasy Model 2.3.2 — Signal Discovery + Error Feedback

## Goal
Lower weekly MAE/RMSE and improve correlation without adding older seasons by extracting more information from the existing honest historical forecast record.

## Core idea
2.3.2 treats forecasting as both a feed-forward prediction problem and a bounded feedback-control problem.

1. **Feed-forward signal discovery** asks which pre-kickoff variables consistently explain actual fantasy production and, more importantly, the residual error left by the existing model.
2. **Expected production** exposes the 2.2 structured neutral stat-line forecast as `expected_fppg_232` and measures the gap between expected opportunity production and the current base forecast.
3. **Contextual residual learning** uses a small ridge model trained only on prior OOF forecasts to learn *why* the base historically undershot or overshot.
4. **PID-like feedback** uses only a player's already-observed prior forecast errors. P = previous signed error, I = decayed/capped persistent error, D = change in recent error.
5. **Production promotion gate** keeps locked 2.3 for any position where 2.3.2 does not improve honest chronological MAE while protecting RMSE/correlation.

## Why the controller is not a literal industrial PID
Weekly fantasy outcomes contain touchdown and game-script noise and the underlying system changes when player role, team, health or opponent changes. A literal accumulating integral term would chase noise and suffer windup. 2.3.2 therefore uses decay, hard integral/derivative caps, a hard total-correction cap and learned zero-valued gains when feedback does not generalize.

## Leakage rule
For historical Week N, the controller may use the observed error from Weeks < N only. The actual outcome of Week N is applied only after the Week N prediction is complete. Signal-controller parameters for a holdout season are learned only from earlier OOF seasons.

## Signal qualification
Candidate pre-kickoff variables are scored on:
- Pearson relationship with actual FPPG
- Spearman relationship with actual FPPG
- correlation with the remaining base-model residual
- year-to-year sign stability
- year-level residual-correlation strength
- coverage

Highly redundant variables are pruned before ridge fitting. A signal model is disabled unless an inner historical season shows actual multi-metric improvement.

## Multi-metric objective
Controller selection rewards:
- lower MAE
- lower RMSE
- lower starter MAE
- higher Pearson correlation
- higher rank correlation

A tiny MAE gain cannot justify a material RMSE/correlation loss.

## Main outputs
- `weekly_2_3_2_validation_metrics.csv`
- `weekly_2_3_2_cohort_metrics.csv`
- `weekly_2_3_2_signal_lab.csv`
- `weekly_2_3_2_error_reason_audit.csv`
- `weekly_2_3_2_rolling_controller_parameters.csv`
- `weekly_2_3_2_selected_controller.csv`
- `weekly_2_3_2_confidence_calibration.csv`
- `weekly_2_3_2_promotion.csv`
- `weekly_2_3_2_model_quality_report.txt`

## 2026 feedback behavior
Before Week 1 there is no live error state, so P/I/D are zero. After completed games, saved pregame snapshots are joined to actual fantasy points and the next projection can use only those completed historical errors. This makes 2026 a genuine prospective adaptive test rather than a retrospective retune.
