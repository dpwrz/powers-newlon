# Fantasy Model 2.5 — Accuracy Tournament

## Goal

Reduce honest weekly fantasy-point error for decision-relevant players without replacing a proven incumbent merely because a challenger looks better on aggregate depth-player MAE.

## Why 2.5 exists

The 2.4 signal layer improved aggregate MAE slightly at QB/RB/WR but did not beat the incumbent for fantasy starters. TE was the only 2.4 position that earned promotion. The direct weekly learner underneath the stack is still based largely on bagged `rpart` trees. 2.5 introduces a modern nonlinear direct challenger while retaining the existing guardrails.

## Challenger

- XGBoost regression (`reg:pseudohubererror`, with squared-error fallback).
- 114 explicit numeric pre-kickoff variables.
- No current-game box-score outcomes, current-game NGS values, current snaps/shares, or residual-derived leakage fields.
- Chronological training only.
- Conservative blend with the current position-specific production champion rather than a wholesale replacement.

## Honest validation

For each target year 2022–2025:

1. Train the direct model only on seasons before the target year.
2. Predict the target year.
3. Use the current production champion as the base (2.4 where it earned promotion, otherwise the locked 2.3/2.3.2 forecast).
4. Select blend alpha only from earlier 2.5 OOF years. The first target year uses a predeclared 0.15 alpha.
5. Record All / Relevant / Starter MAE, RMSE, Pearson correlation, and rank correlation.

## Decision objective

Alpha selection emphasizes:

- 45% Starter MAE
- 30% Relevant-player MAE
- 15% All-player MAE
- 5% Starter RMSE
- 5% Relevant-player RMSE

## Promotion gate

A position is promoted only when the honest challenger improves aggregate All, Relevant, and Starter MAE; does not worsen aggregate RMSE; preserves correlation/rank correlation; shows starter MAE improvement in at least 3 of 4 validation years; shows starter RMSE improvement in at least 3 of 4 years; and has at least 95% cluster-bootstrap probability that starter MAE is lower.

## Live 2026

`23_project_weekly_2026_2_5.R` first rebuilds the full 2.4 production grid. Then, only positions that passed the 2.5 gate are blended toward the direct XGBoost forecast. Availability is applied after the pre-availability blend. Every unplayed week is updated, then ROS is recomputed from the new weekly grid.

## Next experiments after 2.5

The next accuracy tournament should test role forecasts as structured inputs rather than blindly adding a role residual correction. Priority targets are next-week pass attempts (QB), carries + targets (RB), and targets/route participation (WR/TE), plus teammate-absence opportunity redistribution and regime-change detection. Every addition must be cross-fitted and pass the same starter/relevant-player guardrails.
