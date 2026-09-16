# Fantasy Model 3.0 — Dynasty Intelligence Architecture

## Core principle

The language model is an explanation/reasoning layer, not the valuation source.

```text
Fantasy Model 2.4.3 projections
          |
          v
Dynasty Value Engine <--- optional external market values
          |
Sleeper -> League State -> Team Strategy Engine
          |                    |
          |                    +--> Trade Engine
          +--> Draft State ----+--> Draft Engine <--- optional dynasty ADP
                               |
                               v
                        Candidate actions
                               |
                               v
                         AI GM Copilot
```

## Sleeper integration

`R/sleeper_api.R` imports:

- user and league metadata
- league settings / scoring / roster positions
- rosters and league users
- drafts and live picks
- traded future picks
- NFL player identity and current injury metadata

The full player map is cached for 24 hours.

## Player identity

`R/player_identity.R` maps Sleeper players to Fantasy Model players in this order:

1. GSIS ID exact match when Sleeper supplies it.
2. normalized name + position + team.
3. normalized name + position fallback.

The resulting mapping is written to `data/processed/sleeper_player_identity_3_0.csv`.

## Dynasty values

`R/dynasty_value_engine.R` starts from the model's existing discounted multi-year dynasty production value, rescales it to a user-facing 0–10,000 scale, and applies league-format premiums for scarce QB/Superflex and TE-premium formats.

The model value and market value are kept separate.

Market signal priority:

1. `data/external/dynasty_market_values.csv` when supplied.
2. Sleeper search-rank proxy.
3. Model value itself when no market signal is available.

## Team strategy

`R/team_strategy_engine.R` builds optimal lineups using league roster settings, ranks teams on production and long-term value, measures positional need, tracks future draft capital and classifies the competitive window.

## Draft engine

`R/draft_engine.R` resolves:

- current draft
- picks already made
- current pick owner
- all future pick ownership within the draft, including traded picks
- user's next pick
- rookie-vs-startup pool
- remaining model-matched players

Candidate draft score combines:

- model dynasty value
- roster need
- competitive-window fit
- positional scarcity
- risk that the player will not survive to the user's next pick

ADP priority:

1. optional `data/external/dynasty_adp.csv`
2. Sleeper search-rank proxy

## Trade engine

`R/trade_engine.R` compares positional surplus/need and competitive windows across every roster, generates one-for-one frameworks, and scores mutual plausibility, fairness and strategy utility.

The custom evaluator compares multi-player and future-pick packages against one selected trade partner. Future picks are first-class assets, valued from the model's rookie pick curve, years-out discount and projected strength of the original roster. The evaluator reports both sides' strategy utility plus a mutual-fit score.

## AI GM

`R/ai_gm.R` receives structured output from the deterministic engines. It is explicitly instructed not to invent values, picks, injuries, probabilities or roster facts.

Without `OPENAI_API_KEY`, the application remains functional and returns a deterministic GM summary.
