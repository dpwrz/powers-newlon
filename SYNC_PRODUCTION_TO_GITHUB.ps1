param(
    [string]$Branch = "main",
    [switch]$Commit,
    [switch]$Push,
    [switch]$OverwriteRemote
)

$ErrorActionPreference = "Stop"

function Fail($msg) {
    Write-Host ""
    Write-Host "ERROR: $msg" -ForegroundColor Red
    exit 1
}

function Run-Git {
    param(
        [Parameter(Mandatory = $true)]
        [string[]]$GitArgs,
        [string]$ErrorMessage = "git command failed"
    )

    & git @GitArgs
    if ($LASTEXITCODE -ne 0) {
        Fail $ErrorMessage
    }
}

Write-Host "Fantasy Model 3.0 two-repo production sync" -ForegroundColor Cyan
Write-Host "Project root: $((Get-Location).Path)"

if (-not (Test-Path ".git")) {
    Fail "Run this script from the Fantasy Model ENGINE Git repository root."
}

# -----------------------------------------------------------------------------
# Keep /web as its own Git repository.
# The ENGINE repository must not track /web as files, a submodule, or a gitlink.
# -----------------------------------------------------------------------------
$trackedWeb = @(& git ls-files -- web 2>$null)
if ($trackedWeb.Count -gt 0) {
    Write-Host "[SYNC] Removing web from the ENGINE index; local web files are preserved." -ForegroundColor Yellow
    & git rm -r --cached -f --ignore-unmatch -- web
    if ($LASTEXITCODE -ne 0) {
        Fail "Could not remove web from the ENGINE index."
    }
}

# Remove a formal .gitmodules entry pointing at web if one exists.
if (Test-Path ".gitmodules") {
    $entries = @(& git config -f .gitmodules --get-regexp '^submodule\..*\.path$' 2>$null)
    foreach ($entry in $entries) {
        if ([string]::IsNullOrWhiteSpace($entry)) { continue }

        $parts = $entry -split '\s+', 2
        if ($parts.Count -lt 2) { continue }

        $key = $parts[0]
        $path = $parts[1].Trim()

        if ($path -eq "web" -or $path -eq "./web") {
            $section = $key -replace '\.path$',''
            Write-Host "[SYNC] Removing stale .gitmodules section $section" -ForegroundColor Yellow
            & git config -f .gitmodules --remove-section $section
            if ($LASTEXITCODE -ne 0) {
                Fail "Could not remove stale web entry from .gitmodules."
            }
        }
    }
}

# -----------------------------------------------------------------------------
# ENGINE ignore rules.
# /web stays separate. Secrets/local state must never be committed.
# -----------------------------------------------------------------------------
$ignoreLines = @(
    "/web/",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    ".Rproj.user/",
    ".env",
    ".env.*",
    "!.env.example",
    ".dev.vars",
    ".dev.vars.*",
    ".Renviron",
    "node_modules/",
    "dist/",
    "*.zip",
    "*.tar",
    "*.tar.gz",
    "*.tar.zst",
    "logs/*.log",
    "tmp/",
    "temp/",
    ".DS_Store",
    "Thumbs.db"
)

$existingIgnore = ""
if (Test-Path ".gitignore") {
    $existingIgnore = Get-Content ".gitignore" -Raw
}

foreach ($line in $ignoreLines) {
    $escaped = [regex]::Escape($line)
    if ($existingIgnore -notmatch "(?m)^$escaped$") {
        Add-Content ".gitignore" $line
        if ([string]::IsNullOrEmpty($existingIgnore)) {
            $existingIgnore = $line
        }
        else {
            $existingIgnore += "`n$line"
        }
    }
}

# -----------------------------------------------------------------------------
# Production sanity check.
# -----------------------------------------------------------------------------
$required = @(
    "config.R",
    "RUN_AUTO_REFRESH.R",
    "runners/RUN_3_0_AUTO_REFRESH.R",
    "runners/RUN_2_5_AUTO_REFRESH.R",
    "R/weekly_engine_25.R",
    "R/live_refresh_engine_25.R",
    "R/live_error_feedback_engine_25.R",
    "R/league_scoring_engine_25.R",
    "R/live_game_state_engine_25.R",
    "pipeline/23_project_weekly_2026_2_5.R",
    "pipeline/24_score_live_accuracy_2_5.R",
    "scripts/export_model_snapshot.R",
    "tests/TEST_3_0_PRODUCTION.R",
    ".github/workflows/fantasy-live-refresh.yml"
)

$missing = @($required | Where-Object { -not (Test-Path $_) })
if ($missing.Count -gt 0) {
    Write-Host ""
    Write-Host "Missing local production files:" -ForegroundColor Red
    $missing | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
    Fail "Apply the Fantasy Model 3.0 production files before syncing."
}

# -----------------------------------------------------------------------------
# Stage the COMPLETE ENGINE repository.
# This fixes the prior script's selective staging, which left legitimate files
# such as project.Rproj, requirements.txt, research/, run.R, settings/, www/,
# and any future engine files untracked.
# /web remains excluded by .gitignore.
# -----------------------------------------------------------------------------
Write-Host "[SYNC] Staging complete ENGINE repository..." -ForegroundColor Cyan
Run-Git -GitArgs @("add", "-A", "--", ".") -ErrorMessage "git add -A failed"

# Production artifacts/state required by GitHub Actions can be ignored by broad
# project rules, so explicitly force-add the approved runtime artifacts.
$forcePatterns = @(
    "models/weekly_2_5_direct_*.json",
    "models/weekly_2_5_direct_*_meta.rds",
    "models/weekly_2_5_error_feedback_*.rds",
    "output/weekly_2_5_champion_manifest.csv",
    "output/weekly_2_5_residual_pool.csv",
    "output/weekly_2_5_feedback_promotion.csv",
    "output/weekly_2_5_feedback_metrics.csv",
    "output/model_snapshot.json",
    "output/pregame_projection_archive_2026.csv",
    "output/live_accuracy_player_weeks_2026.csv",
    "output/live_accuracy_weekly_summary_2026.csv",
    "output/live_accuracy_weekly_position_2026.csv",
    "output/live_accuracy_cumulative_position_2026.csv",
    "output/live_accuracy_biggest_misses_2026.csv",
    "output/live_refresh_log_2026.csv",
    "data/state/live_refresh_state_2026.json"
)

foreach ($pat in $forcePatterns) {
    $matches = @(Get-ChildItem -Path $pat -File -ErrorAction SilentlyContinue)
    foreach ($m in $matches) {
        & git add -f -- $m.FullName
        if ($LASTEXITCODE -ne 0) {
            Fail "git add -f failed for $($m.FullName)"
        }
    }
}

# -----------------------------------------------------------------------------
# Reject staged files GitHub regular Git cannot accept (>= 100 MB).
# Check only staged files, not ignored/local files such as the separate web repo.
# -----------------------------------------------------------------------------
$stagedPaths = @(& git diff --cached --name-only --diff-filter=ACMR)
$large = @()

foreach ($rel in $stagedPaths) {
    if ([string]::IsNullOrWhiteSpace($rel)) { continue }
    if (-not (Test-Path -LiteralPath $rel -PathType Leaf)) { continue }

    $item = Get-Item -LiteralPath $rel
    if ($item.Length -ge 100MB) {
        $large += [PSCustomObject]@{
            File = $rel
            MB   = [math]::Round($item.Length / 1MB, 1)
        }
    }
}

if ($large.Count -gt 0) {
    Write-Host ""
    Write-Host "Staged ENGINE files >= 100 MB detected:" -ForegroundColor Red
    $large | Format-Table -AutoSize
    Fail "Regular GitHub Git will reject those files. Use Git LFS/object storage or remove them."
}

Write-Host ""
Write-Host "Staged ENGINE runtime:" -ForegroundColor Green
& git status --short

Write-Host ""
Write-Host "The local web folder is intentionally NOT staged; it remains its own Git repository." -ForegroundColor Cyan

# -----------------------------------------------------------------------------
# Commit only when there are staged changes.
# -----------------------------------------------------------------------------
if ($Commit) {
    & git diff --cached --quiet
    $hasNoStagedChanges = ($LASTEXITCODE -eq 0)

    if ($hasNoStagedChanges) {
        Write-Host "[SYNC] No staged changes to commit." -ForegroundColor Yellow
    }
    else {
        Run-Git -GitArgs @("commit", "-m", "Sync Fantasy Model 3.0 two-repo production runtime") `
            -ErrorMessage "git commit failed"
    }
}

# -----------------------------------------------------------------------------
# Push behavior:
#   -Push                  = normal safe push for routine future syncs
#   -Push -OverwriteRemote = replace GitHub Branch with current local HEAD
#                            using force-with-lease after refreshing origin.
# -----------------------------------------------------------------------------
if ($Push) {
    if ($OverwriteRemote) {
        Write-Host ""
        Write-Host "[SYNC] OVERWRITE MODE: refreshing origin, then replacing origin/$Branch with local HEAD." -ForegroundColor Yellow

        Run-Git -GitArgs @("fetch", "origin") -ErrorMessage "git fetch origin failed"
        Run-Git -GitArgs @("push", "--force-with-lease", "-u", "origin", "HEAD:$Branch") `
            -ErrorMessage "forced GitHub overwrite failed"
    }
    else {
        Run-Git -GitArgs @("push", "-u", "origin", "HEAD:$Branch") `
            -ErrorMessage "git push failed. If the remote must be replaced, rerun with -OverwriteRemote."
    }
}

Write-Host ""
Write-Host "Sync complete." -ForegroundColor Green
Write-Host ""
Write-Host "Next:" -ForegroundColor Cyan
Write-Host "  1. Rerun GitHub Actions -> Fantasy Model 3.0 live refresh -> force=true"
Write-Host "  2. The workflow will publish output/model_snapshot.json into the separate web repo."
Write-Host ""
Write-Host "Usage:" -ForegroundColor Cyan
Write-Host "  First overwrite / reset GitHub main:"
Write-Host "    .\\SYNC_PRODUCTION_TO_GITHUB.ps1 -Commit -Push -OverwriteRemote"
Write-Host ""
Write-Host "  Normal future syncs:"
Write-Host "    .\\SYNC_PRODUCTION_TO_GITHUB.ps1 -Commit -Push"
