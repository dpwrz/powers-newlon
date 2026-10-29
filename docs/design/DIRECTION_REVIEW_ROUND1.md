# Powers–Newlon Fantasy Manager — Interface Direction Review

**Design round:** 01 — foundations and task hierarchy  
**Status:** Interactive, fixture-backed visual studies; **not** the production application  
**Source of product constraints:** Frontend Reset RFC (PR #26) and the prior Fieldnote design exploration (PR #24)  
**Model:** Keep the existing Powers–Newlon Model and R/nflverse pipeline. Sleeper contributes league/roster metadata and actual scores only, never model projections.

## Design decision

Create a separate frontend (proposed React + TypeScript), without layering another style sheet onto `web/src/main.js`. Select a single design system through practical use of the three prototypes before coding the production presentation layer.

## The three visual systems

| Direction | Product personality | Visual grammar | Strength | Trade-off |
|---|---|---|---|---|
| **01 Monograph** | Insightful, editorial, enduring | Warm ivory `#F5F3EE`, forest `#2A5348`, copper `#9F5237`, Georgia headlines, open whitespace, small indexing rules | Most distinctive, excellent explanations | More vertical space, especially for fast score checks |
| **02 Atlas** | Precise, intelligent, controlled | Mineral `#EDF2F5`, ink navy `#172D41`, deep data panel `#173D56`, blue `#236FA2`, crisp sans, monospaced labels | Fastest scan of matchup and evidence; analytical credibility | Needs strict restraint to avoid becoming a dense dashboard |
| **03 Forma** | Tactile, composed, native | Stone `#C9CFC9`, graphite-green `#314744`, warm white `#F8F8F3`, clay `#AD563A`, large actions, rounded compositions | Comfortable, thumb-first workflow | Similarities to card-based fantasy products if overused |

**Recommended base:** **Atlas**, because the product's strongest differentiation is its original projection model and its evidence. Preserve its sparse layout, measured typography, clear numeric hierarchy and well-labeled visualizations. This recommendation does not authorize blending all three directions into a visually inconsistent hybrid.

**Possible refinement:** Borrow Monograph's long-form editorial writing and use it sparingly in analysis explanations, without changing Atlas's screen architecture or visual tokens.

## Common information architecture across directions

| Workspace | Primary question | First-release function | Release-grade test |
|---|---|---|---|
| **Briefing** | What is happening in my league? | Matchup, current-starter projections, actuals, key decisions, freshness | Home/League shared projected totals with correct provenance |
| **Decisions** | What should I do and why? | Start/Sit comparisons, expected differences, uncertainty, reviewed decisions | Real model values; no unsupported transaction eligibility or win probability |
| **Scout** | What does the model know about a player? | Search, filters, player dossier, range, trend, stat line and dynasty context | Responsive search, accessible inspector, verified data sources |
| **Connect** | How do I enter or switch leagues? | Sleeper username/year, discover leagues, select league, error/retry states | Real typed text on Safari, focus preserved during updates |

Full feature parity follows: standings, roster/bench, waiver research, trade/dynasty simulator, draft planning, NFL scores, benchmarking and model data health. Those functions should be composed as contextual tools rather than copied as 12 root navigation tabs.

## Baseline design tokens (applies regardless of direction)

- **Touch:** 44 px minimum target on mobile; avoid small overflow menus for core actions.
- **Typography:** 16 px mobile inputs (prevents iOS focus zoom), 14–16 px body, 11–13 px supporting text, and tabular-figure numeric scores.
- **Spacing:** 4, 8, 12, 16, 24, 32, 48 px rhythm; fewer surfaces and no nested panels merely for decoration.
- **Motion:** 160–240 ms navigation, 250–320 ms for deliberate inspectors; transform + opacity only; reduced-motion support; never animate changing model output as if it were a betting ticker.
- **Responsive:** First-pass verification at 320, 390, 768 and 1440 px; no fixed-width data tables on mobile.
- **Status:** At minimum Ready, Loading, Refreshing, Stale, Error, Disconnected, and No League states. Progress should not unmount focused fields.
- **Provenance:** Label `Projected PPR`, `Actual PPR`, `Current lineup`, `Model-selected lineup`, `Projection generated at` and `League refreshed at` separately. Missing values must not become zero.

## Critical design decisions to lock before implementation

1. **Select the base direction.** Monograph / Atlas / Forma; adopt one coherent set of structure, type and palette.
2. **Navigation.** Primary workspace names and positions, including iPhone dock and desktop rail; use native back/forward semantics.
3. **Home hierarchy.** Matchup and one strongest model-supported decision must be available immediately; bench and detailed stats are progressive disclosure.
4. **Model transparency.** The same canonical matchup view model on every screen, with a secondary explanation for hypothetical optimized totals; no fabricated real-time model probabilities.
5. **League onboarding.** One persistent form with editable username/year, explicit lookup/error/selection states, real-provider integration and browser regression tests.
6. **Actions.** Every insight needs expected benefit, uncertainty, eligibility, time sensitivity and whether it is advisory or actionable.
7. **Appearance.** Define light/dark contrast as complete palettes. Do not rely on huge CSS override layers and `!important` exceptions to fix regressions.

## Prototype content and boundaries

All displayed numbers are fixture values: Week 8, 133.4 projected PPR for the user's example team, 148.6 for the opponent, a **−15.2** projected margin; sample quarterback comparison (Jared Goff 17.1 vs Jordan Love 18.4). These are deliberately *not* claims about the current NFL season or live account.

The prototype supports choosing a direction, navigating Briefing/Decisions/Scout/Connect, changing players in the comparison, marking a decision for review, searching/filtering players, opening a player dossier, and entering text in the demo league form. The league form **never calls Sleeper** and explicitly labels the result as a fixture.

Local browser automation exercised these interactions at **320, 390, 768 and 1440 px** in Chromium. JavaScript errors and horizontal overflow were absent in those tests. WebKit/Safari and real connected-league workflows are still required before production release.

## From design to development

**Gate 1 — Select design (this review):** Pick one visual system and review its Home, Decisions, Scout and Connect views on a real phone. Reject generic card patterns, unnecessary animations and fake metrics.

**Gate 2 — Build new frontend skeleton:** Create `app-next/` with TypeScript, typed adapters, a persistent shell, router, responsive tokens and a stable connection form. No direct changes to existing production UI.

**Gate 3 — Integrate canonical model data:** Define and validate player/league/forecast schema; create one matchup selector for shared numbers; preserve locked pre-kickoff forecasts and distinct live actuals.

**Gate 4 — Deliver usable core journeys:** Connect real league, Briefing, Start/Sit, Scout. Test real league and Safari before expanding.

**Gate 5 — Feature parity:** Roster/bench, standings, waiver moves, dynasty/trade, drafts, scores, accuracy/data health, and full navigation. Measure against current outputs to prevent model regressions.

**Gate 6 — Preview and cutover:** Deploy to a separate preview URL, conduct device-level interaction tests and side-by-side numeric audits, then release behind an explicit rollback plan. **Do not merge visual concepts into the live app until it passes these gates.**