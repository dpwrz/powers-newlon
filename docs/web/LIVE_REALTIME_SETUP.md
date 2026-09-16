# Realtime setup — existing GitHub + Cloudflare Pages deployment

The website works without a realtime provider. In that mode Sleeper still supplies league settings and matchup totals, while the R/GitHub automation handles postgame projection refreshes.

For true in-game player-stat updates, Web 0.7 includes an optional Sportradar adapter. The key stays in Cloudflare and is never shipped to the browser.

## 1. Push Web 0.7 to the existing web repository
Replace/merge the Web 0.7 files into the repository already connected to Cloudflare Pages, then commit and push:

```powershell
git add .
git commit -m "Web 0.7 league scoring and live game state"
git push
```

Cloudflare's existing Git integration will build/deploy it automatically.

## 2. Add Cloudflare variables
Cloudflare dashboard → Workers & Pages → your Fantasy Model Pages project → Settings → Variables and Secrets.

Optional realtime variables:

- `SPORTRADAR_API_KEY` — encrypted secret
- `SPORTRADAR_ACCESS_LEVEL` — `trial` or `production`

Keep the OpenAI variables you already have if AI GM is enabled.

Redeploy once after changing environment variables.

## 3. No provider key yet
Do nothing. The Live tab runs in `SCORE ONLY` mode. It uses the connected Sleeper league's scoring/matchup state but does not fabricate live player stat lines.

## 4. Local testing
Create `web/.dev.vars` (never commit it):

```text
SPORTRADAR_API_KEY=YOUR_KEY
SPORTRADAR_ACCESS_LEVEL=trial
OPENAI_API_KEY=YOUR_OPENAI_KEY_IF_USED
```

Then:

```powershell
npm run build
npm run dev:full
```

Open the Wrangler URL (normally http://localhost:8788), connect Sleeper, then open **Live**.

## 5. R/model repository
Overlay the companion Model 2.5 Live patch into the R repository and keep the existing `fantasy-live-refresh.yml` workflow. No change to the postgame automation design is required.

The realtime path and official/postgame path intentionally differ:

```text
During games: Browser → Cloudflare → realtime provider → temporary live overlay
After games: GitHub Action → R Model 2.5 → all future weeks + ROS → model_snapshot.json → Cloudflare deploy
```

This prevents a single play from becoming a permanent model-state change before the game is finalized.
