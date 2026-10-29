# Powers–Newlon Fantasy Manager — Atlas × Monograph Next

This is the **new, independent** Powers–Newlon frontend. The existing legacy site remains the default homepage; the new application lives at `/next/` after a successful Pages build.

## What is implemented

- Atlas information architecture and numerical hierarchy with Monograph's warm ivory / forest / copper design system.
- Persistent responsive navigation: **Briefing / Decisions / Scout / League**.
- Read-only Sleeper username + season connection and league picker. No login/password needed. Private-browsing storage failures are handled.
- League rosters, standings, owned starters/bench, matchup actuals, and week switching.
- Powers–Newlon Model snapshot adapter and independent weekly projections, floors/ceilings where available, pregame figures, dynasty values, and actuals in a player dossier.
- Shared starting-lineup calculation across screens. Missing values remain missing, not zero.
- Advisory Start/Sit, lineup optimization, waiver simulations, and dynasty trade package valuation; futures use **round-level** model pick baselines.
- Data health, last update, no probability claims without a calibrated probability model.
- Read-only integration. Transactions **never** write to Sleeper.

## Development

From the repository root:

```sh
cd app-next
npm install
npm run dev
```

The frontend reads `/data/model_snapshot.json` and `/data/snapshot_version.json` from the legacy web static assets. For local dev, run the legacy Vite host on port 5173, or proxy the snapshot endpoints to a local fixture server. API calls to Sleeper use its public read-only endpoints.

```sh
cd app-next
npm run build
```

The build writes to `web/public/next/`. The legacy `web` production build invokes this build before copying static files to `web/dist/next/`, so **the existing homepage remains untouched**.

## What is still necessary before replacing the legacy homepage

1. Real connected-league QA on the user's actual dynasty league, not only mocked fixtures. Check player ID coverage and snapshot freshness.
2. Restore feature parity for any specialized tools not yet migrated: full NFL live scores/game box scores, dynasty draft copilot, model accuracy/benchmark reports, player/news availability enhancements, and league scoring customizations.
3. Validate weekly historical data and matchup state labels with completed and currently-playing games.
4. Confirm exact fantasy scoring configuration. Current model projections are PPR; other league scoring formats are not converted automatically.
5. End-to-end iPhone Safari, keyboard/screen-reader, performance and offline scenarios, then approve cutover separately.
6. Use the legacy version as rollback until parity and accuracy are demonstrated.

## Tests

`.github/workflows/validate-next-app.yml` builds TypeScript and runs `web/scripts/smoke-next.mjs` against intercepted Sleeper/model fixtures in Chromium at 320/390/768/1440 px and Safari WebKit at 390 px. Tests verify real typing, league selection, meaningful score calculations, navigation taps, player dossier, Start/Sit, and trade interactions.

Fixtures are **only** in browser tests, not in the production app. In production, errors or missing data are shown as errors or missing values.

## Data provenance

- `Sleeper API`: league metadata, user ownership, rosters, matchups, points, future pick transfers.
- `Powers–Newlon Model snapshot`: projections, ranges, model-based weekly and dynasty values, historical model data.
- `NFL state`: current week/season, from Sleeper's NFL state endpoint.
- No Sleeper projections enter the forecast adapter. D/ST and K projections are not populated with provider projections.

## Implementation note

The new application intentionally does **not** import any old `web/src/main.js` or legacy `styles.css`. The interface components own their input and navigation state, while the data layer is cached and refreshed independently. The old model's forecasting pipeline is **not** modified.
