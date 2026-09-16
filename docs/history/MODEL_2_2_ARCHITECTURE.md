# Fantasy Model 2.2 — Weekly Accuracy Engine

Fantasy Model 2.2 is an accuracy-focused weekly revision built on the validated Model 2.0 season engine and the 2.1 weekly foundation.

## Promotion rule

2.2 is **experimental until honest weekly walk-forward validation beats the 2.1 production weekly model on the same player-week sample**, especially the fantasy-relevant starter cohorts. The 2.0 season model remains the long-horizon prior and guardrail.

## Why 2.2 exists

Model 2.1 proved that recent role, snaps, game environment, and opponent information improve weekly forecasts, but feature importance showed the monolithic weekly learner remained dominated by recent FPPG/volume. Matchup buckets also showed that the model under-reacted to favorable and difficult defenses.

2.2 therefore separates the problem into models that have different jobs instead of asking one tree ensemble to learn everything at once.

## Architecture

```text
Validated Model 2.0 season projection
              │
              ▼
       Long-horizon player prior
              │
      ┌───────┴────────┐
      │                │
      ▼                ▼
Neutral role       Game environment
player usage       spread / total / rest
snaps / trends     expected team volume
NGS traits              │
      │                 │
      └───────┬─────────┘
              ▼
       OPPORTUNITY MODELS
 QB attempts/carries
 RB carries/targets
 WR targets/carries
 TE targets
              │
              ▼
      SHRUNK STAT LINE
 preseason efficiency prior
 + current-season evidence
 + exposure-based shrinkage
              │
              ▼
      NEUTRAL STRUCTURED FPPG
              │
              ▼
       MATCHUP DELTA MODEL
 opponent-adjusted positional residuals
 PBP EPA/success/pressure/explosives
 deep/middle/red-zone tendencies
 player-archetype × defense interactions
              │
              ▼
 STRUCTURED + MATCHUP PROJECTION
              │
       ┌──────┴─────────┐
       │                │
       ▼                ▼
Neutral direct      Full direct challenger
(no opponent)       (all pregame features)
       │                │
       └──────┬─────────┘
              ▼
     CHRONOLOGICAL STACK
 2.0/role prior + neutral + structured/matchup + full direct
              │
              ▼
        FINAL WEEKLY FORECAST
              │
       empirical OOF residuals
              ▼
 median / floor / ceiling / boom / bust / Top-N probability
              │
              ▼
       Rest-of-season sum of week-specific forecasts
```

## Strict anti-leakage rules

For a historical player-week, every feature must have been knowable before kickoff.

- Player rolling production excludes the target week.
- Snap share excludes the target week.
- NGS fields are joined to completed games and lagged before use.
- Team volume rolls exclude the target week.
- Defense positional residuals and PBP metrics are lagged.
- Historical matchup models train on cross-fitted structured residuals, not in-sample residuals.
- Outer validation holds out complete seasons.
- Stack weights for an outer holdout are chosen only from earlier out-of-sample holdouts.
- Closing/pre-kickoff market fields can be used; post-game score/result fields never are.

## 2.2 feature families

### Neutral role

Slow/medium-moving player quality and role state:

- Model 2.0 preseason prior
- 3/5-game FPPG
- season-to-date FPPG
- targets/carries/pass attempts
- target/carry/pass-attempt shares
- offensive snap share and snap trend
- role trend and opportunity per snap
- prior and current shrunk efficiency rates
- optional lagged Next Gen Stats

Opponent and betting-defense fields are deliberately excluded.

### Opportunity

Adds team/game environment to the neutral role state:

- expected team plays
- expected pass rate
- spread / total / implied team points
- favorite/underdog role interactions
- availability context where reliable

Outputs are weekly attempts, carries, and/or targets by position.

### Matchup delta

Learns the part of fantasy production not explained by the neutral structured stat line:

- opponent-adjusted positional residuals
- pass/rush EPA allowed
- success rates
- sack/QB-hit pressure
- explosive/deep/middle passing
- red-zone pass/rush TD tendencies
- player archetype × defense weakness interactions
- game environment

The predicted matchup adjustment is capped by position to prevent sparse defensive samples from moving a projection unrealistically.

### Full direct challenger

The original weekly concept remains as a challenger. It sees all legal pregame features and predicts weekly fantasy points directly. It can earn stack weight only through historical out-of-sample performance.

## Validation outputs

2.2 produces:

- `output/weekly_2_2_validation_metrics.csv`
- `output/weekly_2_2_validation_by_year.csv`
- `output/weekly_2_2_architecture_comparison.csv`
- `output/weekly_2_2_matchup_calibration.csv`
- `output/weekly_2_2_cohort_metrics.csv`
- `output/weekly_2_2_topn_accuracy.csv`
- `output/weekly_2_2_opportunity_metrics_by_year.csv`
- `output/weekly_2_2_stack_candidates.csv`
- `output/weekly_2_2_selected_stack.csv`
- `output/weekly_2_2_feature_importance.csv`
- `output/weekly_2_2_residual_pool.csv`
- `output/weekly_2_2_model_quality_report.txt`

## Fantasy-relevant validation

Overall MAE is not enough. 2.2 also reports cohorts selected from **pregame baseline rank only**:

- QB starter: top 12
- RB starter: top 24
- WR starter: top 36
- TE starter: top 12

Broader relevant-player cutoffs are QB24/RB60/WR80/TE36. These cohorts prevent fringe players from making the model look better while start/sit accuracy stagnates.

## Projection distributions

The production forecast uses honest out-of-sample residuals to estimate:

- median
- 20th-percentile floor
- 80th-percentile ceiling
- boom probability
- bust probability
- position Top-N probability

These are decision-support distributions, not claims that NFL outcomes are normally distributed.

## Optional challenger engines

The core project remains `rpart`-only for Posit Cloud reliability. `BENCHMARK_2_2_ENGINES.R` can test optional `ranger` and `xgboost` challengers when those packages are already installed. They do not become production models merely because they are more sophisticated; they must win the same chronological validation.

## Known data limitations

- The public nflverse injury source currently does not provide a reliable live post-2024 feed. The project preserves historical injury features and keeps current availability fail-safe rather than fabricating data.
- True live slot/outside receiver alignment is not universally available from the public sources used here. `data/external/receiver_alignment.csv` remains an optional preseason input.
- Far-future schedule rows often do not yet have betting totals/spreads. The ROS engine uses neutral defaults until those fields become available, then weekly refreshes can replace them.
