# Changelog

## 3.0 portable hotfix — role-validation target scope

- Fixed Step 18 target-name masking inside `tibble()` that caused `test[[target]]` to receive an entire column instead of a scalar column name.
- Hardened role-model dynamic position/target lookup.
- Silenced the expected historical many-to-many injury join warning by declaring the relationship explicitly.
- Added `tests/TEST_ROLE30_TARGET_SCOPE.R`.


## 2.4.3 — Clean project package

Structural cleanup only:

- Added `run.R` as the stable current build entrypoint.
- Grouped numbered stages under `pipeline/`.
- Grouped orchestration and resume scripts under `runners/`.
- Grouped prerequisite and benchmark scripts under `tests/`.
- Grouped repair/refresh/stage helpers under `maintenance/`.
- Consolidated current documentation under `docs/` and historical notes under `docs/history/`.
- Archived the redundant embedded `fantasy_model_2_2/` project copy.
- Archived existing log files and left a clean live `logs/` directory.
- Preserved current `data/`, `models/`, `output/`, `settings/`, `research/`, `R/`, and Shiny assets.
- Updated active script references to the cleaned directory structure.

No intentional modeling, scoring, feature, training, or validation methodology change was made as part of this cleanup.

## 3.0.0 — Dynasty Intelligence

- Added Sleeper league/roster/draft/traded-pick integration.
- Added GSIS/Sleeper player identity bridge.
- Added league-aware dynasty value engine and future-pick curve.
- Added competitive-window, roster-need and league-power analysis.
- Added live draft recommendations and trade-down frameworks.
- Added dynasty trade finder and custom trade evaluator.
- Added optional OpenAI Responses API GM explanation layer.
- Added 3.0 role/regime/injury-redistribution shadow data engine.
- Added walk-forward role validation and strict target-level promotion gates.
- Added decision-relevant starter/relevant/depth error audit.
- Replaced the legacy 2.3.2-era app surface with the 3.0 Dynasty app; archived the previous app.
- Production fantasy-point projection remains Fantasy Model 2.4.3.

## 3.0 pre-Week-1 execution hotfix
- Made empty current-season weekly data schema-safe before Week 1.
- Prevented the 2.3.2 live PID state builder from filtering a zero-column table.
- No model/forecast methodology changes.
