# Fantasy Model 3.0.7 — Session Stability

- Removed automatic cached-league analysis from Shiny startup.
- League analysis now begins only after Connect / Refresh League.
- Connect builds a complete local snapshot before publishing reactive values.
- Added garbage collection around connected analysis.
- Added a clean launcher that clears stale model-build objects from `.GlobalEnv` before Shiny starts; generated files on disk are preserved.
- Replaced Shiny's ambiguous disconnected grey overlay with an explicit disconnect diagnostic banner.
- Retained cached/on-demand Draft and Trade architecture from 3.0.6.
