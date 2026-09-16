# Upgrade the existing GitHub + Cloudflare deployment to Model 2.5 Live / Web 0.7

This guide assumes the setup already used by Fantasy Model:

- a **private R/model GitHub repository** (the production engine), and
- a separate **web GitHub repository** already connected to **Cloudflare Pages**.

Do not create a new Cloudflare project. Upgrade the repositories in place.

## Architecture after the upgrade

```text
During games
Browser -> Cloudflare Pages Function -> Sleeper league state/scoring
                                \-> optional Sportradar realtime stats
      -> Web 0.7 live score + rest-of-game overlay

After games / public data refresh
GitHub Actions -> R Model 2.5 Live -> W1-W18 + ROS + accuracy
               -> web/public/data/model_snapshot.json
               -> existing web repo
               -> existing Cloudflare deployment
```

Sleeper is used for league rules, rosters, matchup state, drafts/picks and identity. Sleeper projections, rankings, ADP and player values are not used.

## A. Upgrade the R/model repository

Overlay the combined patch into the local R project, then from the project root run:

```r
source("tests/TEST_2_5_LIVE_AUTOMATION.R")
source("tests/TEST_2_5_LIVE_SCORING.R")
Sys.setenv(FM_FORCE_REFRESH = "true")
source("runners/RUN_2_5_AUTO_REFRESH.R")
```

Commit/push the model repository, including `.github/workflows/fantasy-live-refresh.yml`.

### Two-repository publishing secrets

In the **model repository** -> Settings -> Secrets and variables -> Actions -> Repository secrets:

- `WEB_REPO` = `YOUR_USERNAME/fantasy-model-web`
- `WEB_REPO_TOKEN` = a fine-grained token restricted to that web repository with **Contents: Read and write**

In Settings -> Actions -> General, allow **Read and write permissions** for the model repository workflow if your repository policy requires it.

Run Actions -> **Fantasy Model live refresh** -> **Run workflow** -> `force = true` once. The workflow publishes only the compact `public/data/model_snapshot.json` into the web repository and therefore triggers the existing Cloudflare deployment.

## B. Upgrade the existing web repository

Replace/merge the Web 0.7 source into the repository already connected to Cloudflare, then run locally:

```powershell
npm install
npm run build
```

Commit and push:

```powershell
git add .
git commit -m "Upgrade Fantasy Model Web to 0.7 live scoring"
git push
```

Your existing Cloudflare Pages Git integration should redeploy automatically. Keep the existing Vite build settings (`npm run build`, output `dist`).

## C. Cloudflare configuration

### League-scored projections + Sleeper matchup state

No new secret is required. Web 0.7 reads the connected league's `scoring_settings` and rescales the Fantasy Model structured stat line while preserving the validated model residual/calibration component.

### True realtime player statistics (optional)

Cloudflare Pages -> your existing project -> Settings -> Variables and Secrets:

- encrypted secret: `SPORTRADAR_API_KEY`
- variable: `SPORTRADAR_ACCESS_LEVEL` = `trial` or `production`

Redeploy after adding/changing variables.

Do **not** put the provider key in GitHub, `.env`, React code, or the browser. The Cloudflare Function owns the secret.

Without a realtime provider key, the Live page remains in **SCORE ONLY** mode: it shows Sleeper league matchup totals and league-specific Fantasy Model projections, while the R/GitHub job performs permanent postgame W1-W18/ROS updates.

With the provider configured, the Live page polls every ~5 seconds while games are in progress (30 seconds when no game is live). Cloudflare caches the upstream realtime feed to limit duplicate calls.

## D. What becomes permanent vs temporary

**During a game** the live game-state model is a temporary overlay. A big play immediately changes current league points and may change the expected final game score, but its future-week effect is bounded and decays quickly.

**After the game** finalized/current public stats enter the normal Model 2.5 Live R pipeline. That is when the official future Week 1-18 and ROS projections are regenerated and published permanently.

## E. Existing tabs

Web 0.7 keeps:

- Rankings
- Weekly
- Season Outlook
- Player
- Start / Sit
- Dynasty
- Quality
- Performance
- My Team / League
- Draft Room / Trade Center / AI GM

and adds **Live**.
