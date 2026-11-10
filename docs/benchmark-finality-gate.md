# Benchmark finality gate

The frozen archive is **not empty**. The large-file `fetch_file` response can be empty; Git blob retrieval confirms 7,441 archived rows, including 555 Fantasy Model and 528 Sleeper rows for Week 9.

The actual-results table is a separate problem. For Week 9, 297 player rows include Buffalo/Minnesota players whose `captured_at` is **before Monday night's kickoff**. Such records cannot substantiate a finalized Week 9 benchmark, even if they contain numeric `actual_fppg`.

Run `python scripts/check_benchmark_finality.py --week 9 --last-kickoff-utc <verified UTC kickoff>` before computing or delivering. The gate checks all actual rows were captured at least four hours after the final kickoff and every frozen snapshot predates its player's kickoff. The four-hour delay is a *minimum safety check*, not proof the provider finalized the box score; an authoritative final-game status and actual-score reconciliation are still required.

Do not use `live_accuracy_player_weeks_2026.csv` as completed-week ground truth until this gate passes. Regenerate actuals after the final game, validate player/team IDs and PPR scoring, then join frozen providers by season/week/player ID on identical cohorts. Treat missing competitor data as unavailable rather than inferred. Only then calculate and deliver the report.

This check does not change existing jobs or claim to repair the producer; it makes the observed finality defect reproducible and prevents silent publication of pre-final actuals.
