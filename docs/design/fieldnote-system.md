# Powers–Newlon Fantasy Manager: The Fieldnote System

**Design study v0.1 · Interactive fixture-backed prototype · Not a production release**

## Why this exists

Previous UI iterations changed colors and renamed conventional fantasy-app screens but retained the basic structure of roster lists, card stacks, and generic tab bars. The result resembled Sleeper and accumulated inconsistent CSS overrides. This design resets the interface's *information architecture*, its visual hierarchy, and its interaction model.

## Product promise

> An independent fantasy intelligence workspace. Open it to understand the week, make a reasoned decision, and see the evidence.

The prediction engine is the product's differentiator. The league provider is merely a data source. Screens therefore orbit **questions and decisions**, not the provider's roster navigation.

## Four primary workspaces

| Workspace | Primary user question | Key affordances |
|---|---|---|
| **Briefing** | What matters this week? | Editorial matchup, projected gap, most consequential recommendation, opportunity radar, lineup exposure |
| **Decision Lab** | What should I do, and why? | Side-by-side player workbench, range/trend context, recommended actions, reviewed state |
| **Scout** | What does our model know about this player? | Searchable index, position filters, projection bands, accessible sliding player dossier |
| **League** | Where do I stand in the competitive landscape? | Standings with record/points sorting, own-team emphasis, short- and long-horizon context |

Advanced dynasty tools, trade simulation, draft preparation, injury status, model accuracy, and alerts should become contextual subviews within those workspaces rather than another 5–10 root tabs.

## What deliberately changes

- **No fantasy-app clone patterns.** No oversized pastel matchup cards, emoji rankings, fake win probabilities, generic KPI tile mosaics, or endless same-weight rectangles.
- **Editorial visual hierarchy.** Distinct typographic emphasis, open space, fieldnote-style section indices, disciplined dividers, and restrained accent colors.
- **Confidence earned by clarity.** Every numeric output must identify whether it is a model projection, actual fantasy points, lineup scenario, historical production, or a speculative recommendation.
- **App-like without animated clutter.** A persistent navigation shell, screen transitions only when context changes, bottom-sheet physics for player details, and no layout shifts for live updates.
- **True responsive design.** Desktop workspace rail, mobile compact navigation, no horizontally squashed data tables, and touch targets at least 44 px where practicable.

## Visual language

### Palette

| Token | Value | Role |
|---|---|---|
| Paper | `#F2F1EC` | Warm neutral background; reduces the glaring all-white feeling |
| Parchment | `#E9E8E0` | Supporting surfaces / editorial contrast |
| Ink | `#20302F` | Display and body text |
| Deep forest | `#1E4040` | Matchup hero and strategic focus areas |
| Field green | `#286F61` | Interactive emphasis, model analytics |
| Burnt clay | `#CB6748` | Deliberate, sparing attention / section numbering |
| Divider | `#D8DCD5` | Hierarchy without stacked cards |

Avoid gradients except a nearly invisible atmospheric field in the matchup hero. No unnecessary noise overlay, glass blur over important text, or luminous neon accent systems.

### Typography

- Display: Georgia or a properly licensed editorial serif, 54–103 px depending on viewport.
- UI: native system font stack for sharpness, performance, and accessibility.
- Scores: large tabular figures, physically separate from labels and supporting forecasts.
- Overlines: 9–10 px, tracked uppercase, only for semantic hierarchy.
- All controls remain readable at common mobile viewport sizes.

### Motion

- Route transition: 220–280 ms fade/slide using `cubic-bezier(.22,1,.36,1)`.
- Player dossier: ~320 ms physical panel movement, opaque surface from the first frame.
- Micro-interactions: 120–180 ms on focus/press/hover.
- Honor `prefers-reduced-motion: reduce` with immediate state changes.
- Never animate live scores or numerical model outputs in ways that imply false information.

## Data contract for production migration

The fixture-backed prototype is intentionally isolated. Integration with the current R/nflverse/Powers–Newlon model should happen through a tested read-only adapter, not by copying provider logic into screen components.

```
Overview {
  leagueId, week, refreshedAt,
  matchup: {
    myTeam, opponent,
    currentStarterProjection: {mine, theirs, missingPlayers},
    optimalLineupProjection?: {mine, theirs},
    actuals?: {mine, theirs, status, lastUpdatedAt}
  },
  decisions: Decision[],
  starters: PlayerForecast[]
}
Decision {
  kind, title, evidence[], projectedGain?, risk?,
  isAvailable?, canAction?, blockingReasons[],
  source, snapshotTime, expiresAt?
}
PlayerForecast {
  id, name, position, nflTeam, opponent,
  projectedPpr, floorPpr, ceilingPpr,
  confidence, seasonActualPpg,
  history: {week, projection, actual?, gamePlayed}[]
}
```

**Required invariants:**

1. Home and League show the same current-starter matchup total for the same league and week.
2. Actual scores, frozen pregame forecasts, and in-game model estimates are separate values. Never label observed points as projections.
3. Sleeper is used for rosters, ownership, scoring, game/league metadata; the Powers–Newlon Model owns the projections.
4. Win probability appears only if a separately validated probability model exists.
5. A proposed drop cannot be hidden from the action sheet. Dynasty and short-term consequences must be distinguishable.
6. Data freshness labels reflect the actual last successful source/snapshot update.
7. No completed weeks are silently re-predicted. Benchmarking retains locked forecasts.
8. All recommendations are advisory until transaction-write support and league eligibility checks exist.

## Production replacement plan

1. **Foundation / data bridge:** Create `modelAdapter`, `leagueAdapter`, normalized types, routing and navigation state. Keep legacy UI online. Add fixture tests for all statuses.
2. **Briefing / Decision Lab:** Replace the current Home and optimizer screens, wire real model outputs, verify real PPR deltas, eligibility, action explanations, and responsiveness.
3. **Scout / player dossier:** Replace existing player profile and leaderboard modules with a dedicated searchable index and nonblocking inspector. Include actual/projection historical plot, news, and uncertainty.
4. **League / advanced tools:** Rebuild standings, team rosters, trade builder and dynasty/draft interfaces within the new design language. Preserve full data capabilities; don't hide features to reduce UI complexity.
5. **Cross-device QA and release:** Test 320/390/768/1024/1440 px, real iOS Safari PWA, keyboard and screen reader, reduced motion, offline/stale paths, performance, and weekly model accuracy. Launch only after review.

## Prototype interactions

- Navigate between four workspaces.
- Change side-by-side player comparison.
- Review and unreview decisions (local prototype state).
- Search and filter player index.
- Open/close player dossier, Escape to dismiss, keyboard focus trap, add player to Decision Lab.
- Sort league standings by record or points.
- Responsive desktop rail / mobile dock.

No network requests are made by the concept. **All player/score data is explicitly illustrative.**

## What is not yet implemented

This design study is not a production app, does not use your current Sleeper account, does not write transactions, and does not alter the model. Its purpose is to establish the visual and behavioral quality bar before source integration. In particular, real live scoring, availability-aware optimizer logic, dynasty trade simulation, model analytics, saved league profiles, and robust navigation history need implementation and testing.