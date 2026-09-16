# Fantasy Model 3.0.5 — Team production schema hotfix

Fixes a live Dynasty application failure in `R/team_strategy_engine.R` where `fm3_choose_lineup()` referenced `projected_fppg` even when the attached roster-value table did not carry that optional column.

Changes:
- carries `projected_fppg` through `fm3_attach_roster_values()` when available;
- normalizes required roster/value columns before team analysis;
- uses explicit, length-safe production fallbacks instead of data-mask references to optional columns;
- falls back from `year1_fppg` to `projected_fppg` and finally zero;
- increments the app build to 3.0.5.

No projection model, role model, dynasty valuation formula, or Sleeper identity logic is changed.
