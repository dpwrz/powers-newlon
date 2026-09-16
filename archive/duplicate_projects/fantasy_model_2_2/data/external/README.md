# Optional receiver alignment data

Fantasy Model 1.1 uses public nflverse route, target-depth, pass-location, team-volume, QB-tendency and Next Gen Stats context automatically.

The public participation data does not provide a complete player-level slot/outside alignment share, so true alignment is optional rather than estimated.

To add alignment data, create:

`data/external/receiver_alignment.csv`

with columns:

- `season`
- `player_id` (GSIS ID)
- `slot_rate` (0-1)
- `wide_rate` (0-1)
- `inline_rate` (0-1; mainly useful for TE)

Use completed-season alignment only. A 2025 row becomes a preseason feature for the 2026 projection. Historical rows are also required if you want the model to learn how alignment relates to next-season fantasy output.
