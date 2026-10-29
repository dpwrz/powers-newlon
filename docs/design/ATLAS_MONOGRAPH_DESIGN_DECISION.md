# Powers–Newlon Fantasy Manager
## Selected interface direction: Atlas structure × Monograph colors

**Design decision v1 · October 29, 2026**  
**Status:** Selected visual direction for the independent frontend rebuild. Not a production deployment.

### The decision

Use **Atlas's information architecture, grid, hierarchy, analytical components, and system typography** with **Monograph's warm mineral palette and editorial restraint**. This is not an equal mixture of two layouts: the structural choice is Atlas, with only the color and warmth borrowed from Monograph.

The result should feel like a premium football intelligence workstation: fast to scan, calm, precise, and recognizable as Powers–Newlon. Avoid fantasy-app clones, neon esports styling, overloaded cards, decorative glass effects, and enormous whitespace that hides useful decisions.

### Visual tokens

| Role | Token | Value | Notes |
|---|---|---|---|
| Main canvas | `color.canvas` | `#F5F3EE` | Warm off-white; less clinical than Atlas blue-gray |
| Raised surface | `color.surface` | `#FFFEFB` | Input panels, player detail surfaces |
| Supporting background | `color.surfaceSubtle` | `#ECEBE4` | Section groupings, discreet selection states |
| Primary text | `color.ink` | `#25342E` | Deep green-charcoal, high contrast |
| Secondary text | `color.muted` | `#607067` | Darkened from the first Monograph study for accessible body text |
| Primary action | `color.action` | `#2A5348` | Forest green, unmistakably primary |
| Data hero background | `color.hero` | `#23463E` | Deep forest-green projection stage |
| Hero text | `color.heroInk` | `#F6F2E9` | Warm, clear figures |
| Editorial accent | `color.copper` | `#9F5237` | Low-volume emphasis, indices and notable changes |
| Chart / positive | `color.sage` | `#A9C9B4` | Data series, not a universal success indicator |
| Chart / comparator | `color.parchment` | `#E2D7C2` | Distinguishes opponent without red/green reliance |
| Borders | `color.line` | `#D7D9D1` | Thin structural division |
| Navigation selected | `color.selection` | `#E0EBE5` | Muted sage background |

Contrast targets on the warm canvas: ink ~11.8:1; secondary text ~4.7:1; copper ~5.1:1; action green ~7.8:1. Verify those values in the final interface with the actual font sizes and adjacent backgrounds.

### Structure and typography

- Keep **Atlas's layout**: an ordered analytical instrument, comparative score bars, a concise recommendation queue, model uncertainty context, and dense-but-readable rows.
- Use a system sans-serif for main headlines, interface text and labels. Serif can appear **only in longer written analysis or explainers if it genuinely improves reading**, not across core numeric and control surfaces.
- Matchup PPR figures get tabular numerals, a clear sign and unit, and labels for timeframe and source. Never show a projected-score percentage as win probability.
- Monospaced overlines distinguish model metadata and update provenance, but should not shrink below legible mobile sizes.
- Fewer visual boxes: section borders and whitespace first, surfaces second. Favor charts and evidence over arbitrary metric-card mosaics.
- Small corner radius (approximately 4–6 px) for instrumentation and buttons. Player detail panels may have a modestly softer radius if required for mobile touch interactions.

### Core screen grammar

**1. Briefing — What matters this week?** A compact league/week header; single prominent matchup instrument with current-starter PPR and actuals distinctly labeled; top two evidence-backed opportunities; concise forecast uncertainty module. Current and model-selected lineups remain separate scenarios.

**2. Decision Lab — What should I consider doing?** Side-by-side eligible lineup comparison, expected PPR gap, uncertainty bands, matchup drivers, availability/lock checks, and a clear review action. Advisory state must never imply Sleeper roster changes.

**3. Scout — What does the model know?** Fast player search, position filters, readable index rows, trend indicators, model projection/actual split, and a player dossier. Search should not re-render its focused input.

**4. League — What is my context?** Roster and bench, standings, team needs, scores and long-horizon outlook; dynasty/trade/draft tools appear as contextual workspaces. **Connect** is a reliable onboarding/league-switching flow, not a permanent main tab in the production information architecture. The design prototype uses Connect as a demonstration navigation destination.

### Interaction and motion

- Persistent navigation and stable input nodes. Changing live scoring data must not destroy focused fields, the current scroll position, comparison selections, or a player dossier.
- iPhone-sized touch targets >=44 px; input text >=16 px to avoid unintended Safari zoom. Safe-area aware navigation and sheets.
- Navigation transitions 160–240 ms; dossier 250–320 ms with opacity/transform only; reduced-motion alternative; no theatrical loading sequences.
- Back behavior, deep links, and scroll restoration are part of the system design rather than later polish.
- Implement explicit loading, refreshing, stale, disconnected, error, empty league and final-game states. Freshness for **Sleeper league data**, **actual scoring**, and **Powers–Newlon model projections** must be separate.

### Data and model contract

Preserve the existing R/nflverse-powered Powers–Newlon Model as the projection source. Sleeper supplies rosters, league metadata and actual scores, never its own projections. A canonical typed matchup selector must power all screens to prevent the previous conflicting totals. Missing fields remain missing, not zero. Locked pregame projections must remain available for benchmarking; no silent rebuilding of completed weeks. Optimizer suggestions must include eligibility, gain basis, cost and uncertainty.

### Implementation sequence and release standard

1. **Separate frontend foundation:** New `app-next/` React + TypeScript app, typed model/Sleeper adapters, validated schemas, persistent shell, and unit tests for view-model selectors. Do not add more CSS patches to `web/src/main.js`.
2. **Real Connect:** Username/year entry, real Sleeper discovery, league selection, retry and cache status. Automated mobile Safari + Chromium typing and switching tests, plus an actual iPhone check.
3. **Briefing and Decision Lab:** Build the same layout as this selected design with real, provenance-labeled model outputs. Shared matchup totals must reconcile across screens.
4. **Scout and League:** Search, model dossier, starters/bench, standings, trades, dynasty/draft, scores and saved profiles, feature-by-feature parity with legacy functionality.
5. **Preview and signoff:** Run against a real connected league, inspect all critical views on phone/tablet/desktop, pass accessibility and performance gates, and compare all model outputs to the existing site.
6. **Controlled cutover:** Only then promote a separately deployed app to production with a tested rollback path.

### What is approved, and what is not

**Chosen direction:** Atlas's structure and interaction density with Monograph's color palette and warmth.

**Not yet approved or implemented:** final branding assets, production connected-league behavior, live reforecasting, real transactions, or replacement of the existing production website. The accompanying HTML is fixture-backed and demonstrates visual and interaction intent only.