# Fantasy Model 2.0 Architecture

## Core idea

Fantasy production is decomposed into layers with different levels of predictability.

### Layer 1: Team environment

Estimate the size of each offense's opportunity pool:

- pass attempts/game
- carries/game
- pace/play volume and PBP-derived tendencies

### Layer 2: Player opportunity

Use ML where the 1.3 experiment demonstrated stronger year-ahead signal:

- QB attempts and rush attempts
- RB carry share and target share
- WR target share and carries
- TE target share

Teammate competition, PBP context and position-specific team/QB fit can influence these projections.

### Layer 3: Efficiency shrinkage

Efficiency rates are noisy. 2.0 combines:

- recent player rate
- career player rate
- exposure/sample size
- position-level historical mean

The amount of regression is selected from historical training data.

### Layer 4: Touchdown expectation

TD rates are shrunk like other rates and then allowed a bounded red-zone/context adjustment. This reduces dependence on volatile prior-year TD conversion while still allowing offensive environment to matter.

### Layer 5: Structured fantasy projection

The opportunity and rate estimates are recombined into a football stat line and then scored using the base fantasy settings.

### Layer 6: Residual correction

A small capped model learns systematic remaining errors from prior honest out-of-sample structured projections. It cannot move the base structured estimate by more than the configured residual cap.

### Layer 7: Direct-model guardrail

For each position, 2.0 tests blends of the structured/residual projection and the validated 1.2 direct model. Historical walk-forward results select the 2026 weight.

This means a position can be:

- 100% 1.2 if 2.0 adds no value
- a hybrid if both help
- primarily 2.0 if the new architecture clearly earns it

## Why this differs from 1.3

1.3 demonstrated that target share and other opportunity variables were much more predictable than yards-per-target and TD rates. 2.0 therefore does not treat every component as an independent free-standing ML target.
