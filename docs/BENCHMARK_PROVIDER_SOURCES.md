# Fantasy Model 3.1 — External Benchmark Provider Plan

## Benchmark contract

External providers are evaluation-only. For a valid head-to-head comparison:

1. Capture the provider projection/ranking before that player's kickoff.
2. Never overwrite a frozen player-week/provider row after kickoff.
3. Preserve provider, scoring format, capture time, source endpoint, and provider rank.
4. Compare only identical player cohorts and identical scoring where point projections are used.
5. Keep ranking-only sources separate from point-projection sources.
6. Do not ingest a provider when its terms or license do not permit automated benchmarking, model evaluation, or competitive/public use.

## Provider status

### Sleeper — IMPLEMENTED

Use the current app projection route first:

- `https://api.sleeper.app/v1/projections/nfl/regular/{season}/{week}`

Fallbacks:

- `https://api.sleeper.app/projections/nfl/{season}/{week}?season_type=regular`
- `https://api.sleeper.com/projections/nfl/{season}/{week}?season_type=regular`

The collector requests `position[]=QB|RB|WR|TE` and `order_by=pts_half_ppr`, then normalizes the returned stat line to Half-PPR. These projection routes are app feeds rather than the core v1 league API contract, so the collector must keep endpoint fallbacks and provider-status logging.

Sleeper's official API documentation states that its API is free for non-commercial use; commercial use requires contacting Sleeper for licensing. Re-check this before monetizing Fantasy Model.

### FantasyPros — OFFICIAL API EXISTS, LICENSE REVIEW REQUIRED

FantasyPros now exposes official NFL weekly projections and consensus rankings through its API. This is technically an ideal benchmark source because it can provide both numeric projections and rankings.

Do not add the API to the production/public benchmark without permission. Current FantasyPros API terms include a non-compete restriction and distinguish free prototype, premium personal production, and commercial access. If Fantasy Model remains an internal personal benchmark, request an API key and confirm that benchmarking a competing projection model is permitted. If Fantasy Model is public or monetized, get written commercial permission first.

Until then, public pregame FantasyPros rankings can be used for one-off, cited research comparisons, but should not be scraped into a persistent production dataset.

### ESPN — EXPERIMENTAL ONLY

ESPN's fantasy web application exposes JSON endpoints used by its own UI, including weekly projected fantasy points in player stat objects. These endpoints are not presented as a supported public developer API.

If added, treat ESPN as an experimental provider:

- capture only pregame;
- retain endpoint/version metadata;
- fail independently without affecting Fantasy Model capture;
- do not depend on ESPN for production correctness;
- review ESPN/Disney terms before persistent automated collection or publication.

### Yahoo — OFFICIAL OAUTH API, PROJECTION COVERAGE TO VERIFY

Yahoo maintains an official Fantasy Sports API using OAuth. It is a good candidate for league/team/player metadata. Before implementing a projection benchmark adapter, verify that the current API exposes Yahoo's own weekly projected fantasy points/rankings in a supported endpoint and that the applicable license permits the intended benchmark use.

### PFF — DO NOT AUTOMATE WITHOUT WRITTEN LICENSE

Current PFF terms expressly restrict automated/manual extraction, public distribution, competing products, and use of PFF data to benchmark/evaluate predictive or machine-learning systems. Do not ingest PFF projections or rankings into Fantasy Model's automated benchmark unless PFF provides a separate written license that permits it.

Public PFF articles may still be cited as editorial context when appropriate, but should not be reconstructed into a persistent benchmark dataset.

### RotoWire — DO NOT AUTOMATE WITHOUT WRITTEN CONSENT

RotoWire publishes weekly projections, but current terms prohibit automated extraction and specifically restrict benchmarking/competitive analysis. Do not build a scraper or automated archive against RotoWire without express written consent or an authorized data license.

### WalterPicks — NO SUPPORTED PROJECTION API IDENTIFIED

No supported public weekly projection API has been identified. Use only public/citable material for one-off comparisons unless WalterPicks provides an authorized data feed or grants permission for benchmark capture.

### FantasyData — LICENSABLE CANDIDATE

FantasyData publishes weekly fantasy projections and offers data/API products. This is a strong candidate for a licensed numeric-projection benchmark if its subscription/license permits internal competitive benchmarking and, if desired, publication of derived comparison metrics. Confirm the exact license before integration.

## Recommended benchmark stack

### Automated numeric projection sources
- Fantasy Model
- Sleeper
- FantasyData (after license confirmation)
- FantasyPros (only after explicit permission for this use)

### Automated ranking sources
- Same providers above when their licenses permit ranking capture
- Yahoo if supported projection/ranking fields are verified

### Research-only / citation-only
- PFF unless separately licensed
- RotoWire unless separately licensed
- WalterPicks unless an authorized feed is provided
- Public FantasyPros pages when API use is not licensed for the benchmark

## Next implementation order

1. Stabilize Sleeper and verify a Week 3 pregame capture.
2. Add a provider-neutral archive schema and scoring report that can compare any provider already present in the archive.
3. Investigate ESPN as an experimental adapter without making it a required provider.
4. Add FantasyData if its license permits benchmarking.
5. Add FantasyPros only after obtaining permission appropriate to Fantasy Model's public/competitive use.
