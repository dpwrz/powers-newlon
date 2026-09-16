# Model History

This file is a navigation summary. Full historical design and patch documents are preserved in `docs/history/`.

## 2.0 — Season architecture
Established the validated season foundation, opportunity/residual components, calibration, and season-level projections.

## 2.1 — Weekly baseline
Introduced the player-week projection engine and matchup-oriented weekly forecasting.

## 2.2 — Decomposed weekly challengers
Added neutral, structured, direct, opportunity, matchup, stacking, and related weekly challengers. Historical 2.2 engine code remains because later versions reconstruct or compare against it.

## 2.3 — Historical forecast learning
Added historical out-of-sample meta-calibration, phase-aware blending, matchup calibration, residual calibration, and version guardrails.

## 2.3.2 — Signal and feedback revision
Added signal discovery, expected-production-gap features, and bounded historical error-feedback controllers. Its honest OOF predictions form part of the locked benchmark used by 2.4.

## 2.4 / 2.4.3 — Current packaged project
Adds player talent priors, experience-aware gating, expected opportunity, opponent style, hierarchical player/archetype response, signal attribution, and strict promotion. `VERSION.txt` records the packaged project as 2.4.3.
