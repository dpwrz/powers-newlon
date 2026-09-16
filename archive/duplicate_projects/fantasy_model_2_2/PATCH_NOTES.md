# Fantasy Model 2.2 Notes

## Model changes

- Preserves validated Model 2.0 as the long-horizon prior.
- Replaces the 2.1 monolithic-only weekly architecture with separate neutral, opportunity/stat-line, matchup-delta, and full-direct learners.
- Matchup learner is trained on cross-fitted structured residuals.
- Adds rolling/shrunk efficiency state and optional lagged weekly Next Gen Stats.
- Adds explicit player-archetype × defensive weakness features.
- Adds fantasy-relevant starter/relevant cohort validation.
- Adds architecture ablation, matchup calibration, Top-N rank accuracy, and opportunity-component metrics.
- Uses a constrained chronological stack; the first holdout starts safely at 100% prior.
- Adds empirical OOF projection distributions for floor/ceiling/boom/bust/Top-N probability.

## App changes

- New Home and Rankings views.
- Weekly distributions and explicit matchup delta.
- Dedicated Start/Sit comparison workflow.
- Dedicated Rest-of-Season view.
- Weekly stat line and architecture context on Player Detail.
- 2.2 ablation/cohort/matchup diagnostics on Quality.

## Compatibility

2.2 writes compatibility copies to the existing weekly output names so downstream app/league code continues to work.

## Important

2.2 is experimental until its honest weekly validation beats 2.1 on the same player-week sample.
