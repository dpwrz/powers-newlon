# Fantasy Model 2.4 Patch Notes

- Adds college player production and roster-history ingestion with season checkpoints.
- Adds NFL combine athletic measurements and derived speed/BMI/athleticism features.
- Adds draft capital and age-at-draft to the player talent prior.
- Adds an experience-gated talent prior that decays as real NFL evidence accumulates.
- Adds optional expected-opportunity/xFP history when the nflreadr source is available.
- Adds lagged opponent blitz, pressure, pass-rusher count, man/zone, coverage-family and box-context features when supported by available participation/FTN data.
- Adds deterministic QB/RB/WR/TE archetypes.
- Adds hierarchical position -> archetype -> player matchup-response estimates with shrinkage.
- Adds signal attribution: Pearson/Spearman, partial residual relationships, year stability, redundancy pruning, standardized coefficients and OOF permutation importance.
- Adds a stricter multi-metric promotion gate against the locked 2.3/2.3.2 benchmark.
- Keeps the prior models intact as fallbacks when a 2.4 position does not earn promotion.

## 2.4.3
- Repaired optional ffopportunity/xFP normalization using the current weekly schema.
- Added typed/failure-safe xFP checkpoints and rolling history.
- Added conservative NCAA↔NFL first-initial/surname fallback matching when exact preferred-name matching fails.
