# Start Here — Fantasy Model 3.0 + Web 1.0

## First run

Open the project root in RStudio and run:

```r
source("RUN_FIRST_TIME_SETUP.R")
```

This is the only setup command you need. It installs dependencies, builds any missing validated production artifacts, verifies the forward-only/error-feedback/live layers, runs the first production refresh, and exports the website snapshot.

## Normal future refresh

```r
source("RUN_AUTO_REFRESH.R")
```

or simply:

```r
source("run.R")
```

`run.R` automatically chooses first-time setup only if the required production champion files are missing.

## Website

```powershell
cd web
npm install
npm run build
npm run dev:full
```

For GitHub and Cloudflare, read `docs/CLEAN_INSTALL_GITHUB_CLOUDFLARE.md`.
