# Fantasy Model 2.3 App Changes

The app remains mobile-first but now exposes the model's historical-learning layer instead of only showing a point estimate.

## Weekly / Start-Sit
Weekly cards and tables now surface:
- final projection, floor, ceiling and Top-N/boom probabilities;
- calibrated matchup delta;
- model confidence (High/Medium/Low);
- historical expected absolute error for similar model-disagreement states;
- model spread/disagreement.

Start/Sit comparisons retain matchup/role/game-environment explanations and add confidence/error context so a 0.5-point edge is not presented as decisive when the models strongly disagree.

## Player page
The weekly stat-line panel now shows:
- neutral structured FPPG;
- raw 2.2 matchup delta;
- calibrated 2.3 matchup delta;
- historical residual correction;
- model spread;
- final weekly projection;
- 2.1/2.2/2.3 version-guardrail weights.

## Quality page
The weekly architecture table now compares the prior, reconstructed 2.1, chronological 2.2 and each 2.3 stage on the same OOF player-weeks. The stack table is phase-aware, and matchup calibration shows raw vs calibrated delta vs actual historical delta.

## Projection history
Each successful weekly refresh appends a lightweight snapshot to `output/projection_history_2026.csv`. This is audit/display data only and is never fed back as a current-week feature. It creates the foundation for a future "projection changed" chart and alert system.

## Next app priorities after 2.3
1. League roster sync / roster-aware start-sit and waiver recommendations.
2. Projection-history chart on player pages.
3. Data freshness panel for PBP, snaps, NGS, lines and availability.
4. Actual-vs-projected weekly audit dashboard.
5. Watchlist and material projection-change alerts.
