#!/usr/bin/env python3
"""Fail closed on missing/corrupt benchmark evidence. Stdlib only."""
import argparse
import csv
import io
import json
import math
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "output"

def read_csv(name):
    path = OUT / name
    if not path.exists() or path.stat().st_size == 0:
        raise ValueError(f"{name}: missing or empty")
    with path.open(newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        if not reader.fieldnames:
            raise ValueError(f"{name}: no header")
        return list(reader)

def number(value):
    try:
        v = float(value)
        return v if math.isfinite(v) else None
    except (TypeError, ValueError):
        return None

def audit():
    errors, warnings = [], []
    try:
        manifest = read_csv("benchmark_snapshot_manifest_2026.csv")
    except ValueError as e:
        return {"ok": False, "errors": [str(e)], "warnings": []}
    successful = [r for r in manifest if r.get("status") == "ok" and
                  (number(r.get("rows_captured")) or 0) > 0]
    try:
        archive = read_csv("benchmark_pregame_archive_2026.csv")
    except ValueError as e:
        archive = []
        if successful:
            errors.append(f"{e}; manifest claims {len(successful)} successful nonempty captures")
        else:
            warnings.append(str(e))
    if archive:
        required = {"season", "week", "provider"}
        missing = required - set(archive[0])
        if missing:
            errors.append(f"benchmark archive missing columns: {sorted(missing)}")
        else:
            for r in successful:
                key = (r.get("season"), r.get("week"), r.get("provider"))
                if not any((a.get("season"), a.get("week"), a.get("provider")) == key for a in archive):
                    errors.append(f"manifest success without persisted archive rows: {key}")
            if len(errors) > 30:
                errors = errors[:30] + ["additional mismatches omitted"]
    try:
        projections = read_csv("pregame_projection_archive_2026.csv")
        for field in ("week", "player_id", "projection", "captured_at"):
            if field not in projections[0]:
                errors.append(f"projection archive missing {field}")
    except ValueError as e:
        errors.append(str(e))
    try:
        actuals = read_csv("live_accuracy_player_weeks_2026.csv")
        for field in ("week", "player_id", "actual_fppg", "team_actual"):
            if field not in actuals[0]:
                errors.append(f"actuals missing {field}")
        bad = sum(number(r.get("actual_fppg")) is None for r in actuals)
        if bad:
            warnings.append(f"{bad} actuals rows have missing/non-numeric fantasy points")
    except ValueError as e:
        errors.append(str(e))
    return {"ok": not errors, "errors": errors, "warnings": warnings,
            "manifest_success_rows": len(successful), "archive_rows": len(archive)}

def main():
    p = argparse.ArgumentParser()
    p.add_argument("--json", default="")
    args = p.parse_args()
    report = audit()
    print(json.dumps(report, indent=2))
    if args.json:
        Path(args.json).write_text(json.dumps(report, indent=2) + "\n")
    raise SystemExit(0 if report["ok"] else 1)

if __name__ == "__main__":
    main()
