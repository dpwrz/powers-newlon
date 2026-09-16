# Hotfix 3.0.3 — Sleeper Identity Bridge

- Removes remaining non-standard-evaluation references to `gsis_id` from the dynasty identity/market bridge.
- Guarantees canonical `sleeper_id` and `gsis_id` columns even when Sleeper has no GSIS ID.
- Uses name + position (+ team where possible) as the fallback identity path.
- Adds a Reset Sleeper Cache button and a recovery runner.
- Adds an app-build marker to Setup so the installed patch can be verified.
