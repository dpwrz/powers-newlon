# Benchmark evidence integrity repair

This change adds a fail-closed integrity check. The existing snapshot manifest claims thousands of successful captures, while `output/benchmark_pregame_archive_2026.csv` is empty. A manifest is **not** evidence that a snapshot was persisted.

Run `python scripts/audit_benchmark_archive.py`. GitHub Actions also runs it Tuesday/Wednesday and when relevant output files change. It fails loudly and preserves JSON diagnostics rather than allowing an apparently successful benchmark based on missing data.

**Important:** This is a detection/guardrail repair, not a reconstruction of missing historical frozen projections. Do not backdate or relabel historical endpoint backfills as frozen snapshots. The producer that writes `benchmark_pregame_archive_2026.csv` must be changed to write to a temporary file, flush/fsync, atomically replace the archive, read it back, verify season/week/provider/player counts, and **only then** append a successful manifest entry. A failure must emit a failed manifest status and preserve the last known-good archive.

Before a weekly leaderboard is delivered, separately validate finalized game results (including Monday games), stable player IDs, team/week joins, and exact matched cohorts. Delivery status must be tracked separately from repository job completion. This audit intentionally does not invent missing player-level projections or claim those steps have already been repaired.
