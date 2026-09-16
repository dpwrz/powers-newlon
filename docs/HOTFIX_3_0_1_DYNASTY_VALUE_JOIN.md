# Fantasy Model 3.0.1 Hotfix — Dynasty value join / cached analysis

## Symptom
After a Sleeper league was loaded, the Setup page could show a cached analysis error similar to:

`Assigned data pmin(...) must be compatible with existing data. Existing data has 589 rows. Assigned data has 0 rows.`

## Root cause
`output/final_dynasty_rankings.csv` already contains `confidence`. The 3.0 player-identity bridge then joined the current-season rankings, which also contain `confidence`. A plain `left_join()` renamed them to `confidence.x` and `confidence.y`. The dynasty value engine later requested `d$confidence`, received a zero-length vector, and attempted to assign a zero-length `pmin()` result into the 589-player table.

## Fix
- Merge season context into canonical names explicitly; no `.x`/`.y` collision remains.
- Make dynasty-value optional feature access length-safe.
- Harden Sleeper market columns, avoid the Sleeper/model `gsis_id` join collision, and de-duplicate by GSIS ID.
- Strip ANSI styling from Shiny error messages.
- Increase mobile bottom-nav item width and wrap labels to prevent overlapping tabs.

This hotfix changes application execution safety only. It does not change the 2.4.3 production projection model.
