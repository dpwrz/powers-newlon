# Fantasy Model Web 0.7 — League Scoring + Live Game State

## League-specific scoring
When a Sleeper league is connected, Web 0.7 reads that league's `scoring_settings` and rescales the structured Fantasy Model stat line into the league scoring system. The validated Model 2.5 projection is preserved as the base; the app adds only the difference between the structured stat line under Fantasy Model base scoring and the same stat line under the selected league scoring.

League-scored projections are now used in:
- Weekly rankings
- Season Outlook / ROS totals
- Start / Sit
- connected-team projected starter strength
- GM/team context

Sleeper projections, rankings and values remain disabled.

## Live tab
A new **Live** tab supports two levels:

1. **Score-only mode (no paid provider):** Sleeper supplies live matchup totals under the league's own scoring. If Sleeper's matchup payload includes per-player point detail, the site displays those actual player points too.
2. **Realtime player-stat mode:** configure a server-side Sportradar NFL key in Cloudflare. The Pages Function polls the v7 weekly schedule and live Game Statistics feeds, maps Sportradar player IDs through Sleeper identity data, scores the live stat line with the selected Sleeper league rules, and runs the conservative live game-state filter.

## Live model behavior
The within-game filter updates:
- current league fantasy points;
- expected final-game fantasy points;
- expected final opportunities/stat line;
- a bounded role-surprise signal;
- temporary W+1/W+2/W+3 projection impacts.

It does **not** retrain Model 2.5 and it does not permanently rewrite ROS every play. After the game, the existing GitHub/R auto-refresh ingests finalized public stats and officially regenerates all remaining weeks + ROS.

## Scoring coverage
The model can exactly rescore components present in the structured weekly stat line (passing yards/TD/INT/attempts, rushing carries/yards/TD, receiving targets/receptions/yards/TD, plus common yardage and TE-premium bonuses). If a league uses an exotic scoring key for a component not currently projected, the Live page reports it rather than silently pretending the conversion is exact.
