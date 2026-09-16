# Fantasy Model 3.0 + Web 1.0

A consolidated NFL fantasy projection and dynasty decision system with a validated position-specific weekly projection stack, forward-only live seasonal state, bounded final-forecast error feedback, Sleeper league scoring, Start/Sit, weekly model-performance tracking, dynasty/GM tools, and a Cloudflare-hosted web product.

## Start here

### First install

```r
source("RUN_FIRST_TIME_SETUP.R")
```

### Normal model refresh

```r
source("RUN_AUTO_REFRESH.R")
```

### Web build

```powershell
cd web
npm install
npm run build
```

### Full local web including Pages Functions

```powershell
npm run dev:full
```

## Production rules

- Completed player-weeks are frozen and never re-predicted.
- Only unplayed player-weeks are refreshed.
- ROS = actual points already earned + projections for remaining player-weeks.
- The last pregame forecast is frozen for honest weekly accuracy measurement.
- Prior final-model errors can influence later forecasts only through the bounded, chronologically validated feedback layer.
- Sleeper provides league state/scoring, not Fantasy Model rankings or projections.

## Deployment

See `docs/CLEAN_INSTALL_GITHUB_CLOUDFLARE.md` for the clean one-repository GitHub + Cloudflare Pages setup.

## Important internal-version note

The product release is 3.0.0, but validated internal components keep their historical 2.x filenames and artifacts. Do not rename those files manually; the supported 3.0 production wrappers call them in the validated order.
