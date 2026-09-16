# Fantasy Model 3.0.9 - Reactive UI isolation

## Root cause
Sleeper league refresh completed successfully, then the Trade Center observer fired
before `trade_partner` had settled. `setNames()` received a zero-length value vector
and a non-zero label vector at `app.R` line 386, raising an unhandled observer error.

## Fix
- Adds `fm3_named_choices()` for length-safe Shiny select choices.
- Trade partner/player/pick observers are wrapped in `tryCatch()`.
- Empty or transient trade-partner states now produce empty controls rather than errors.
- Optional Trade Center UI can no longer terminate the core league session.
- Sleeper remains roster/league/draft state only; Fantasy Model projections and values remain authoritative.

No projection or role-model rebuild is required.
