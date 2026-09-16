# Fantasy Model 3.0 — Accuracy / Data Lab

## Objective

Attack the largest remaining source of weekly fantasy error: uncertainty in future role and opportunity, while preserving the locked 2.4.3 fantasy-point forecast until a challenger proves an end-to-end improvement.

## Role targets

QB:
- pass attempts
- pass-attempt share
- offensive snap share

RB:
- carries
- targets
- carry share
- target share
- offensive snap share

WR / TE:
- targets
- target share
- offensive snap share

## Regime features

The engine detects abrupt role movement from:

- roll-3 vs roll-5 snap acceleration
- target acceleration
- carry acceleration
- pass-attempt acceleration
- recent role/FPPG trends
- teammate availability pressure

It emits:

- `role_shift_magnitude_30`
- `role_regime_probability_30`
- `role_regime_direction_30`

## Injury opportunity pressure

Historical NFL injury reports are joined to each injured player's most recent prior role. Team-week features estimate the target/carry/pass/snap share that was at risk before kickoff.

Those missing opportunities are then allocated as **candidate redistribution pressure**, proportional to the recent role of remaining teammates. This is a feature for the role model, not an unvalidated direct fantasy-point bonus.

## Phase-aware information age

The model emits:

- `phase_preseason_weight_30`
- `phase_recent_weight_30`
- `phase_stable_weight_30`

Preseason/static information decays as games are played; recent information grows in importance. Large regime-change probability reduces the assumption that the old role is stable.

## Validation

`pipeline/18_train_validate_role_models_3_0.R` performs season-forward validation across 2022–2025. Each role target is compared against a simple lagged-role baseline.

A target is promoted for the **role output only** when:

- MAE improves by at least the configured minimum, and
- RMSE does not degrade beyond the configured tolerance.

This promotion does not alter the 2.4.3 fantasy-point projection.

## Decision-relevant audit

`pipeline/21_audit_decision_relevant_error_3_0.R` reports error separately for:

- fantasy starter cohort
- relevant non-starters
- depth players
- early/mid/late-season phases

It also labels the biggest misses with a coarse likely error family so the next projection release can target the failure modes that matter most to lineup decisions.
