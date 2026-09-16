# Fantasy Model 2.2 App Design

The 2.2 app is redesigned around fantasy decisions rather than raw model outputs.

## Improvements already included

### Home
- league-aware top redraft player
- upside leader
- dynasty leader
- active league summary
- top-10 league values
- concise “what changed in 2.2” explanation

### Rankings
- position/team/player filters
- league-adjusted FPPG and value
- mobile card layout instead of forcing a wide table

### Weekly
- week/position/search/sort controls
- league-adjusted weekly projection
- floor / median / ceiling
- boom and Top-N probabilities
- explicit matchup grade and point delta
- role trend and implied team total
- mobile cards

### Start / Sit
- compare 2–4 players
- projection, floor, ceiling, matchup, role trend and probabilities
- model pick
- decision-edge confidence
- explanation of why the model prefers one player

### Rest of Season
- sums matchup-specific remaining weekly forecasts
- ROS points/FPPG/range
- favorable and difficult schedule counts

### Player
- season value and decision probabilities
- next weekly projection/range
- matchup delta
- weekly structured stat line
- recent role/snap context
- 2.2 stack weights
- existing team/QB context, comps and season components

### Quality
- 2.0 season validation
- 2.2 weekly metrics
- architecture ablation
- selected stack weights
- fantasy-relevant cohort accuracy
- matchup calibration

## UX issues observed in the current mobile app

The screen recording showed a functional app, but several areas were too technical or required excess scrolling:

1. The Quality page exposed a long raw text report before the important decisions.
2. Wide validation tables required horizontal scrolling on a phone.
3. Weekly/player decisions were separated from floor/ceiling and matchup explanation.
4. There was no dedicated Start/Sit workflow.
5. Rest-of-season value was not a first-class screen.
6. Search/filter controls consumed significant vertical space.
7. The mobile bottom navigation was useful but the number of destinations is growing.

2.2 addresses the first five directly and keeps card-based mobile rankings. The remaining navigation/filter refinements are the next app-only iteration.

## Highest-priority app improvements after 2.2

### 1. Roster-aware league hub
Connect a roster/import source so the app knows:
- my team
- opponent roster
- available free agents
- bench/start slots

Then the app can surface “best start”, “best waiver add”, and “trade upgrade” without manual searching.

### 2. Live data freshness/status
Show timestamps and status for:
- weekly stats
- PBP defense context
- snaps
- market lines
- availability/injury source

Weekly fantasy decisions are only as good as the freshness of the inputs.

### 3. Projection-change timeline
For every player, chart how the projection changed from preseason through each week and identify the cause:
- role
- snaps
- opponent
- team environment
- availability

### 4. Better mobile navigation
The app now has more destinations than the original five-tab bottom bar. The next pure-UX iteration should keep 4–5 primary actions visible (`Weekly`, `Start/Sit`, `Player`, `ROS`, `More`) and place Rankings/Dynasty/League/Quality under `More`.

### 5. Interactive matchup explanation
Translate model features into concise fantasy language, e.g.:
- `+1.3`: high implied points
- `+0.8`: opponent weak against deep passing
- `-0.6`: high pressure matchup
- `-0.4`: declining snap share

This should be explanation of model inputs/outputs, not fabricated causality.

### 6. Actual-vs-projected audit
After games, the Weekly screen should automatically show:
- projected
- actual
- error
- rank error
- which component missed (volume, efficiency, TD, matchup)

This will turn the app into the main diagnostic interface for future model development.

### 7. Watchlist + alerts
Once a dependable live availability source is connected, add alerts for meaningful projection changes, role changes, or newly favorable starts.
