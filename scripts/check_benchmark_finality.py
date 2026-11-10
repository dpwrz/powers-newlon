#!/usr/bin/env python3
"""Guard weekly benchmark reports against pre-final actuals and absent frozen evidence.

Uses Python stdlib. Exits nonzero on any failed invariant. Never fabricates projections.
"""
import argparse
import csv
import datetime as dt
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "output"

def load(name):
    with (OUT / name).open(encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))

def timestamp(value):
    value = (value or "").strip().replace("Z", "+00:00")
    if not value:
        return None
    try:
        d = dt.datetime.fromisoformat(value)
        return d.replace(tzinfo=dt.timezone.utc) if d.tzinfo is None else d.astimezone(dt.timezone.utc)
    except ValueError:
        return None

def check(week, season, last_kickoff, min_hours):
    issues = []
    snapshots = [r for r in load(f"benchmark_pregame_archive_{season}.csv")
                 if r.get("season") == str(season) and r.get("week") == str(week)]
    providers = {}
    for r in snapshots:
        provider = r.get("provider", "")
        providers[provider] = providers.get(provider, 0) + 1
        captured = timestamp(r.get("captured_at_utc"))
        kickoff = timestamp(r.get("kickoff_utc"))
        if not captured or not kickoff or captured >= kickoff:
            issues.append(f"invalid pregame timestamp: {provider}/{r.get('player_id')}")
    if not providers.get("fantasy_model"):
        issues.append("no frozen Fantasy Model rows")
    if not providers.get("sleeper"):
        issues.append("no frozen Sleeper rows (report as unavailable if provider genuinely absent)")
    actuals = [r for r in load(f"live_accuracy_player_weeks_{season}.csv")
               if r.get("week") == str(week)]
    # The 'captured_at' column in accuracy rows is the FROZEN FORECAST time,
    # not the timestamp at which actual points were scored. Do not compare it
    # against the final game kickoff.
    schedule_path = ROOT / "data" / "raw" / f"schedules_live_{season}.csv"
    if not schedule_path.exists():
        issues.append("finalized schedule data missing")
    else:
        with schedule_path.open(encoding="utf-8-sig", newline="") as f:
            schedule = [r for r in csv.DictReader(f) if r.get("week") == str(week)]
        if not schedule:
            issues.append("no schedule rows for requested week")
        if any(not (r.get("result") or "").strip() or
               (r.get("result") or "").strip().upper() in ("NA", "NAN")
               for r in schedule):
            issues.append("at least one game lacks an explicit final result")
        finished_teams = set()
        for game in schedule:
            if (game.get("result") or "").strip() not in ("", "NA", "NaN"):
                finished_teams.update((game.get("home_team"), game.get("away_team")))
        if any(r.get("team_actual") not in finished_teams for r in actuals):
            issues.append("actual rows contain teams without a final-result schedule entry")
    cutoff = timestamp(last_kickoff)
    if cutoff is None:
        issues.append("last kickoff timestamp missing or invalid")
    elif dt.datetime.now(dt.timezone.utc) < cutoff + dt.timedelta(hours=min_hours):
        issues.append("minimum post-kickoff safety window has not elapsed")
    if not actuals:
        issues.append("no actual player rows")
    report = {"season": season, "week": week, "frozen_provider_rows": providers,
              "actual_rows": len(actuals), "last_game_cutoff_utc": cutoff.isoformat() if cutoff else None,
              "valid": not issues, "issues_count": len(issues), "issues": issues[:25]}
    return report

def main():
    p = argparse.ArgumentParser()
    p.add_argument("--season", type=int, default=2026)
    p.add_argument("--week", type=int, required=True)
    p.add_argument("--last-kickoff-utc", required=True,
                   help="Verified final game kickoff, e.g. 2026-11-10T01:15:00Z")
    p.add_argument("--min-hours-after-kickoff", type=float, default=4.0)
    args = p.parse_args()
    report = check(args.week, args.season, args.last_kickoff_utc, args.min_hours_after_kickoff)
    print(json.dumps(report, indent=2))
    raise SystemExit(0 if report["valid"] else 1)

if __name__ == "__main__":
    main()
