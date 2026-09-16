# Fantasy Model 2.2.3 Hotfix

This hotfix addresses a Posit Cloud memory spike in `09_build_weekly_data.R` after NGS successfully joins.

## Changes
- Compacts nflverse weekly player stats immediately after normalization so unused raw columns do not survive through every join/copy.
- Replaces the player-season `split() -> lapply() -> bind_rows()` rolling-feature implementation with grouped vectorized `dplyr::mutate()` calculations.
- Preserves strict pre-kickoff leakage rules: every role, efficiency, and NGS feature remains lagged before the target week.
- Keeps the 2.2.2 season/type NGS checkpoints; existing checkpoint files are reused automatically.
- Adds progress messages around the memory-heavy sections to make any future failure easier to isolate.

No statistical architecture, scoring rule, or validation policy changed.
