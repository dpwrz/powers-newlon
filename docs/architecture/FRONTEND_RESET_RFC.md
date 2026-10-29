# Powers–Newlon Fantasy Manager — Frontend Reset RFC

**Status:** Proposed architecture and release contract. Not a production implementation.
**Principle:** Build a trustworthy decision-support product, not a fantasy-league clone or a sequence of visual reskins.

## Why we need a rebuild

The existing web client combines navigation, DOM rendering, Sleeper fetching, model interpretation, lineup logic, and player-detail interactions in a single ~126 KB `web/src/main.js`. The styles include ~83 KB of legacy `styles.css` plus ~70 KB of overrides in `native-ui.css`. Global refreshes replace sizeable pieces of the DOM, causing focus and interaction risk. Much of the UI has been made by layering visual treatment onto legacy layouts. Prior tests started with a connected league and missed the first-run connection flow.

A targeted league-input repair was merged in PR #25. It is a stabilization step, **not** a reason to continue extending the current frontend.

## Product definition

Powers–Newlon Fantasy Manager is a **personal fantasy football intelligence and decision application**. It should answer three questions with clearly sourced evidence:

1. **What is happening?** Current league, week, live actuals and frozen model projections, notable player and matchup changes.
2. **What should I consider doing?** Start/sit, roster moves, trades and dynasty strategy, ranked by achievable impact and uncertainty.
3. **Why?** Opportunities, matchup context, availability, expected stat line, longer-term value, and how reliable the underlying signal is.

It must remain recognizable as Powers–Newlon rather than resembling Sleeper's interface. Names, visual identity and flow should be intentionally designed—not borrowed from a component gallery.

## Non-negotiable invariants

- Keep the established **R/Powers–Newlon Model**. Redesign must not silently change model predictions or backtesting.
- **No Sleeper projections** in generated recommendations. Sleeper supplies league metadata, rosters, status and actual scoring, not the forecasting engine.
- Weekly snapshots must distinguish **locked pregame predictions**, **currently calculated forecasts**, **actual PPR points**, and **unavailable data**. Never mislabel prediction share as win probability.
- Preserve actual capabilities: league connection, lineup and bench, opponent matchup, Explore, Start/Sit, moves, trade and dynasty values, NFL scores, and model diagnostics/accuracy.
- No fake real-world player recommendations or sample league data in production; fixtures must be restricted to test and design environments.
- No automatic Sleeper roster transactions unless an authenticated, explicitly authorized integration is built and separately reviewed.
- Cloudflare Pages remains a reasonable deployment target.

## Proposed architecture

```text
R / nflverse model pipeline
        |
        v
Validated, versioned model artifacts
  - players, weekly projections and uncertainty
  - dynasty/pick values
  - immutable pregame predictions for accuracy
        |
        v
Typed domain adapter  <--- Sleeper read-only API client
  - player identities        league / roster / matchup / statuses
  - projection provenance    actual fantasy scores
  - canonical scoring        owned lineup and slots
        |
        v
Application data layer (cached queries, explicit loading/stale/error states)
        |
        v
Typed view-model selectors
  - matchup summary reused across pages
  - optimal lineup / candidate replacement
  - actionable move eligibility
  - player evidence
        |
        v
Independent responsive UI
```

### Frontend technology

Start a **new application folder** (e.g. `app-next/`) instead of rewriting `web/src/main.js` again. Proposed stack: **React + TypeScript + Vite**, with React Router, TanStack Query for asynchronous data, Zod (or equivalent) for validating external payloads, and carefully authored CSS modules/design tokens. Avoid importing a pre-made "fantasy dashboard" component library. This selection is pragmatic, not a mandate if prototype evaluation shows a better fit.

Design components should include a persistent AppShell, ConnectionFlow, MatchupOverview, DecisionList, PlayerDossier, PlayerSearch, LineupView, TradeWorkbench, Status/Provenance, and a small set of accessible sheet/dialog primitives. A component owns its focused inputs and is not destroyed by unrelated data refreshes.

### Data and performance

- Define TypeScript schemas for model and league data. Treat missing and zero values differently.
- Build a canonical `MatchupViewModel` once; Home and League must render the same projections and use the same PPR scoring rules.
- Key queries by league ID, season and week. Cancel stale requests when the selection changes; cache data with visible last-updated timestamps.
- Fetch heavyweight model artifacts once per version and consider partitioned, compact week/position data. Do not put large play-by-play in the browser.
- Refresh visible live scores without replacing navigation or active form nodes. Expensive optimization occurs on relevant source updates, not on every UI render.
- Offline/disconnected states must be honest: show previously cached data with a timestamp or an actionable retry, not quietly claim the model is current.

## Visual and interaction direction

Start with **information architecture and tasks**, not colors or page art. Distinguish the brand through precise typographic hierarchy, restrained visualization, evidence-first recommendations, and a clear story of where a projection comes from. Use fewer but better surfaces; avoid the familiar stacked sports-app cards, oversized player-photo lists, and endless nested tabs.

Explore two genuinely different high-fidelity concepts **with the same real workflow fixtures** and compare actual phone interactions. Do not treat the Fieldnote demo as an approved design: it was a fixture-backed concept and had no real league connection.

Motion guidelines: stable app shell, fast route response, purposeful 160–260 ms transitions on transforms/opacity, no layout thrashing, bottom-sheet gestures only when implemented reliably, reduced-motion support, preserved scroll/selection and predictable back behavior.

## First five tested user journeys

1. **Connect:** first-time iPhone visitor enters Sleeper username, chooses season, finds real leagues, selects one; keyboard stays visible while typing and no refresh loses focus.
2. **Read the week:** user can immediately understand their selected week, opponent, actuals vs model projections, data freshness and uncertainty.
3. **Make a lineup decision:** user compares two eligible players, reads why one projects higher and what is uncertain, then returns to the same place.
4. **Research a player:** user searches, filters, opens a player dossier with projection/stat line, confidence, news context and dynasty view, then exits with scroll preserved.
5. **Evaluate a transaction:** user builds a realistic trade/waiver proposal with roster and league constraints, understands short-term vs long-term consequences, and is not misled into thinking a transaction was executed.

## Release gates (all required)

- Browser journey tests in **WebKit (Safari)** and Chromium, covering first-run, saved-user, error, reconnect, stale data, keyboard interactions, and every top-level destination.
- Automated screenshot comparisons at 320, 390, 768 and 1440 px; no horizontal overflow or unreadable contrast.
- No component remount during typing, no duplicate event handlers, and focus/selection retained through refreshes.
- Shared matchup fixture proves **numerical equality** between all presentation surfaces; missing projection values render as missing, not zero.
- The source of every displayed numeric claim is testable; model projections and Sleeper actuals remain separately labeled.
- Production build, accessibility basics, touch targets, reduced-motion and keyboard/screen-reader dialog behavior all pass.
- Run against a **real connected test league**, not only fixtures, before accepting a release.
- User reviews an actual interactive preview **before** any production cutover. Feature parity and a rollback path are documented.

## Migration and rollout

**Phase A — stabilize the current website:** preserve features and immediately fix critical input/connection failures with regression tests. No more cosmetic refactors on the legacy shell.

**Phase B — build the independent foundation:** typed model adapter, Sleeper data client, connection flow, app shell, shared matchup view model, core navigation and test harness. Host under a separate preview URL or deployment branch.

**Phase C — implement the decision experience:** player research, Start/Sit, roster opportunities, trade/dynasty tools. Move features only after equivalent results are verified against the existing app.

**Phase D — controlled cutover:** test on real iPhones/desktops and the user's actual league, compare predictions and recommendations between old/new, perform visual UX review, then promote the new deployment. Keep the old version available for rollback.

## What this proposal does not do

It does not select a final visual identity, promise real-time in-game model reforecasting, replace the R model, connect Sleeper in the Fieldnote fixture prototype, or deploy the new frontend. It supplies the **engineering and design constraints** the eventual replacement must meet.
