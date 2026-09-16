# Fantasy Model 2.2.6 Hotfix

## Fix
`11_project_weekly_2026.R` could fail before Week 1 (or whenever no usable current-season team state existed) with a `vctrs` incompatible-size error while assigning `expected_team_plays`.

The cause was a missing `roll3_team_pass_attempts` / `roll3_team_carries` column after joining an intentionally empty current-season team-state table. Accessing a missing tibble column returned `NULL`, which became a zero-length numeric vector and could not be assigned to a non-empty scheduled player-week grid.

## 2.2.6 behavior
- Keeps current-season rolling team pass/carry volume when available.
- Falls back to the validated Model 2.0 2026 team-volume projections before Week 1 or for teams without current-state rows.
- Uses conservative league defaults only if the 2.0 team-volume table cannot supply a mapped team.
- Adds `RESUME_2_2_WEEKLY_FROM_STEP4.R` so completed 2.2 validation/training is not repeated.

No statistical validation result is changed by this hotfix. It repairs the live/future projection path only.
