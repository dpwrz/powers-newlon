# Model 2.5 Live — Forward-Only Refresh

Completed player-weeks are immutable production history.

On every automatic refresh:

1. previously completed rows in `output/weekly_<season>_projections.csv` are loaded and frozen;
2. current public NFL state is refreshed so newly completed games can be finalized once;
3. Model 2.5 is called only for rows with `is_actual == 0`;
4. rankings/distributions/top-N probabilities are recalculated only for those unplayed rows;
5. ROS is recomputed as **actual points to date + projections for remaining games**;
6. completed weeks remain available to the website as actual/history rows and for accuracy reporting, but are never re-predicted.

This also works during partial weeks. A completed Thursday player stays frozen while Sunday/Monday player-weeks can continue to update.

The upstream incumbent stack may still reconstruct an internal state grid in order to ingest newly completed games and current context. Those historical rows are restored from the frozen production ledger before the 2.5 challenger is scored, and the 2.5 learner is never called on completed player-weeks.
