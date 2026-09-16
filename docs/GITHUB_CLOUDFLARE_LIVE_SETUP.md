# GitHub + Cloudflare production setup — Model 2.5 Live / Web 0.7

## What is automatic now

The scheduled R job does **not retrain** Model 2.5. It polls nflverse current-season player-week stats and schedule state, detects whether public state changed, refreshes current defense PBP after completed games, then re-scores every remaining week and ROS using the already validated position champions. It also maintains a frozen final-pregame forecast archive and scores completed weeks.

The R/GitHub path remains the automatic postgame/current-state refresh using free nflverse data. Web 0.7 adds a separate live-game state layer in Cloudflare: Sleeper provides exact league scoring/matchup state, and an optional realtime provider supplies cumulative in-game player statistics. The live overlay is temporary; finalized games still enter the R pipeline before future-week/ROS changes become permanent.

## Recommended repository layout

Use one private GitHub repository whose root is the Fantasy Model R project and whose `web/` subfolder is the website:

```
Powers-Newlon Model/
  config.R
  R/
  pipeline/
  runners/
  models/
  output/
  data/
  .github/workflows/fantasy-live-refresh.yml
  web/
    package.json
    src/
    functions/
    public/
```

In Cloudflare Pages keep Git connected to this repository, then set:

- Root directory: `web`
- Build command: `npm run build`
- Build output directory: `dist`

The GitHub Action updates `web/public/data/model_snapshot.json` and pushes a bot commit. Cloudflare sees the commit and redeploys automatically.

## One-time runtime requirements

The repository must contain the trained production runtime artifacts from the successful 2.5 tournament, including:

- `models/weekly_2_5_direct_QB.json` + `_meta.rds`
- corresponding RB/WR/TE files
- `output/weekly_2_5_champion_manifest.csv`
- `output/weekly_2_5_residual_pool.csv`
- existing 2.1–2.4 production model/calibration artifacts used transitively by `pipeline/23_project_weekly_2026_2_5.R`
- `data/processed/weekly_model_table_2_3.csv`
- `data/processed/model_table.csv`
- `data/raw/weekly_defense_context_raw.csv`

Before pushing, run locally from the project root:

```r
source("tests/TEST_2_5_LIVE_AUTOMATION.R")
Sys.setenv(FM_FORCE_REFRESH = "true")
source("runners/RUN_2_5_AUTO_REFRESH.R")
```

The forced run creates/updates the compact live state, final-pregame archive, weekly scorecard and web snapshot.

## Turn scheduled automation on

1. Commit this patch and all required production runtime artifacts.
2. Push the repository to GitHub.
3. Repository Settings -> Actions -> General -> Workflow permissions -> allow **Read and write permissions** if required by repository policy.
4. Open Actions -> **Fantasy Model live refresh** -> Run workflow -> Force = true.
5. Confirm the action succeeds and creates a `fantasy-model-bot` commit.
6. Confirm Cloudflare creates a deployment from that commit.
7. Leave the cron schedule enabled.

The workflow keeps these compact state files between runs, including the final-pregame archive. The 2.3.2 feedback controller now uses that compact frozen archive for its live PID/error state, so the large projection-history file does not need to be committed after every refresh.

## Existing web-only GitHub repository

If Cloudflare is already connected to a repository containing only the website (the setup used earlier in this project), **you do not have to break that deployment**. Create a second private model repository and add the two secrets described in `docs/TWO_REPO_GITHUB_SETUP.md`. The supplied workflow will publish only `model_snapshot.json` to the existing Cloudflare repo after each successful R refresh.

Long term, a single repo with Cloudflare Root directory = `web` is simpler, but the two-repo mode is the lower-risk migration from the site you already have online.
