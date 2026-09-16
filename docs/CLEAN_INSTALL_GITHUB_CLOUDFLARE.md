# Fantasy Model 3.0 + Web 1.0 — clean GitHub / Cloudflare install

The release is one complete local project. For production hosting, the recommended layout is still **two GitHub repositories**: a private model-engine repository and the small Web 1.0 repository connected to Cloudflare Pages. This keeps the large R/data/model history away from Cloudflare builds. A one-repository monorepo also works and is documented at the end.

## A. Local first-time setup

1. Install current R/RStudio and Node.js.
2. Extract the release to a normal local folder.
3. Open RStudio in the project root and run:

```r
source("RUN_FIRST_TIME_SETUP.R")
```

The setup installs R dependencies; reuses the bundled historical 2.x models/data; trains/validates the production 2.5 direct challengers if their final artifacts are absent; validates the final-error feedback layer; runs the forward-only production refresh; and exports `web/public/data/model_snapshot.json`.

4. Build Web 1.0 locally:

```powershell
cd web
npm install
npm run build
```

Do not deploy until `npm run build` succeeds.

## B. Recommended production layout: two repositories

### Repository 1 — model engine

Create a private repository such as `fantasy-model-engine`. From the project root:

```powershell
git init
git add .
git commit -m "Fantasy Model 3.0 production"
git branch -M main
git remote add origin https://github.com/YOUR_USER/fantasy-model-engine.git
git push -u origin main
```

If you already have an engine repository, keep its `.git` folder, replace the working-tree files with this release, then commit/push.

### Repository 2 — Web 1.0

Use your existing Cloudflare-connected web repository or create a new repository such as `fantasy-model-web`. Copy the **contents of the local `web/` folder** into the repository root, then:

```powershell
git add .
git commit -m "Fantasy Model Web 1.0"
git push
```

The web repository should have `package.json`, `src/`, `functions/`, and `public/` at its root.

### Let the engine publish snapshots to the web repository

In the **model-engine repository**, create GitHub Actions secrets:

- `WEB_REPO` = `YOUR_USER/fantasy-model-web`
- `WEB_REPO_TOKEN` = a fine-grained GitHub token with **Contents: Read and write** for the web repository
- `WEB_BRANCH` = optional; defaults to `main`

In **Settings -> Actions -> General -> Workflow permissions**, enable **Read and write permissions** for the engine repository's `GITHUB_TOKEN`.

Then open **Actions -> Fantasy Model 3.0 live refresh -> Run workflow**, choose `force=true`, and run it once. The action refreshes the model state, commits persistent engine state, and copies only `model_snapshot.json` into the web repository.

## C. Cloudflare Pages clean setup

Connect the **web repository** to Cloudflare Pages.

Use:

- Production branch: `main`
- Root directory: leave blank because `package.json` is at the web-repository root
- Build command: `npm run build`
- Build output directory: `dist`

Cloudflare will automatically rebuild whenever GitHub receives a new web commit, including the snapshot commits produced by the model-engine workflow.

The `functions/` folder is deliberately at the Pages project root. It provides the Sleeper, AI GM and live-game server routes.

## D. Cloudflare variables and secrets

In the Pages project, go to **Settings -> Variables and Secrets**.

Optional realtime individual-player stats:

- `SPORTRADAR_API_KEY` — encrypted secret
- `SPORTRADAR_ACCESS_LEVEL` — `trial` or `production`

Optional AI GM:

- `OPENAI_API_KEY` — encrypted secret
- `OPENAI_MODEL` — optional model identifier

Never commit real secrets to Git. Local development uses `web/.dev.vars` or `web/.env`; both are ignored by Git.

Without a realtime provider, Web 1.0 still provides Sleeper league-specific scoring, league/roster state, production projections, Start/Sit, W1–W18 outlook, postgame automatic updates, ROS and weekly model performance.

## E. Normal operation

The two update loops are intentionally separate:

**Cloudflare/browser live loop:** refreshes Sleeper/live-provider game state while games are active. It can show current league points and rest-of-game overlays, but does not permanently rewrite the trained projection history every play.

**R/GitHub production loop:** when new football state is available, it scores newly completed games against the frozen pregame prediction, adds actual usage and prior final-model error to historical state, freezes completed player-weeks, reprojects **only unplayed player-weeks**, rebuilds ROS and performance reports, and publishes a new web snapshot.

## F. One-repository alternative

You may instead push the entire release to one GitHub repository and connect Cloudflare Pages to that repository with:

- Root directory: `web`
- Build command: `npm run build`
- Build output directory: `dist`

In that mode, do not set `WEB_REPO` or `WEB_REPO_TOKEN`; the engine workflow commits the snapshot into the same repository and Cloudflare deploys it from `web/`. The two-repository layout is recommended because the R project contains large historical datasets and trained model artifacts that the website does not need to clone or build.
