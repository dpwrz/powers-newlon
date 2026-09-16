# Fantasy Model 2.1 — Weekly Projection Architecture

Fantasy Model 2.1 keeps the validated 2.0 season model as a **preseason / long-horizon prior** and adds a separate strict pre-kickoff weekly engine.

## Forecast chain

```text
Validated 2.0 season prior
        +
Current player role
(targets, carries, pass attempts, snap share)
        +
Current team volume
        +
Opponent-adjusted defensive results
        +
Compact PBP defensive style / efficiency
        +
Schedule & game environment
(home/away, rest, total, spread, implied team points)
        +
Optional availability / injury signals
        ↓
Weekly position model
        ↓
Validated blend with dynamic season/current-role prior
        ↓
Weekly FPPG + floor / ceiling + matchup delta
        ↓
Rest-of-season aggregation
```

## Leakage rule

For a historical player-week, every rolling player, team and defense statistic excludes that week. Walk-forward validation trains on prior **seasons** and predicts an entire unseen holdout season. No result, score, snap, target, injury outcome, PBP event or defensive result from the predicted week is allowed to enter that week's features.

## Defense model

2.1 does not rely on raw "fantasy points allowed" alone. It calculates opponent-position residuals: what a defense allowed **relative to the entering expectation of the players it faced**. It also uses selected PBP metrics such as pass/rush EPA, success rate, explosive passes, depth/location tendencies, sacks/QB hits and red-zone TD rates.

## Role model

Recent role is represented by lagged 3- and 5-game production and opportunity, plus offensive snap percentage. This helps separate a real role change from a one-week fantasy scoring spike.

## Season aggregation

Completed weeks use actual fantasy points. Future weeks retain their matchup-specific projections. Rest-of-season and full-season totals therefore update as the year progresses rather than simply multiplying one FPPG estimate by remaining games.

## Known limitation

NFLverse's public injury-data source currently ends after 2024. Historical injury information is preserved for training where available, but current injury adjustment is intentionally fail-safe/optional until a reliable live source is connected. The rest of the weekly model does not depend on that feed.
