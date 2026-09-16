# Model 2.5 final-forecast error feedback

The production model now treats its own completed-week forecast error as future state, but only in a leakage-safe and validation-gated way.

## What becomes historical state
After a player-week is complete, the frozen pregame 2.5 forecast is compared with the actual result:

`error = actual fantasy points - frozen pregame projection`

For future unplayed weeks the engine can use only PRIOR completed observations:

- last signed error
- exponentially decayed signed player error
- rolling 3 signed error
- rolling 3 absolute error
- error trend
- recent position-level signed bias
- recent position-level absolute error

A positive signed error means the model had been under-projecting that player. A negative signed error means it had been over-projecting him.

## Why the prior error is not simply added to next week
Fantasy outcomes are noisy. A +12 miss caused by two long touchdowns should not create a +12 next-week correction. Player error is shrunk toward position bias, gains are small, corrections are capped, and the effect decays for weeks farther into the future.

## Honest validation gate
Run once after Model 2.5 validation:

```r
source("runners/RUN_2_5_FEEDBACK_TOURNAMENT.R")
```

The tournament reconstructs each historical target week using only errors available before that week. It selects player- and position-level feedback gains from prior OOF seasons and promotes the feedback layer only if it improves the existing 2.5 production forecast on starter/relevant/all-player error guardrails.

If a position fails, its live feedback correction stays exactly zero.

## Live order
The automatic refresh now runs in this order:

1. poll new NFL stats/schedule state
2. score any newly completed player-weeks against their frozen pregame forecasts
3. make those errors available as lagged state
4. rebuild only unplayed player-weeks
5. apply final-2.5 error feedback only for promoted positions
6. recompute ROS from actual completed rows + future projected rows
7. update the pregame archive and performance reports
8. export the web snapshot

Completed player-weeks remain immutable and are never re-predicted.
