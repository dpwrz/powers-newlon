# Clean-install profile

This package contains the complete production source tree, trained legacy production models, the historical table and 2.4 validation artifacts required to build the 3.0 production challengers, Web 1.0, deployment workflows, dynasty/GM modules, tests and documentation.

To keep the downloadable ZIP practical, redundant historical duplicates and stale generated 2026 projection output were removed:

- `weekly_model_table_2_1.csv` and `weekly_model_table_2_2.csv` (the production `weekly_model_table_2_3.csv` is included)
- old 2.2/2.3/2.3.2 validation-prediction dumps not needed by the 3.0 first-run path
- stale pre-refresh `weekly_2026_projections.csv`
- the old all-season raw `player_weekly_stats.csv`; current-season state is fetched automatically and the processed production training table is included

No production source, Web 1.0 source, trained incumbent model, or artifact required by `RUN_FIRST_TIME_SETUP.R` was intentionally removed.
