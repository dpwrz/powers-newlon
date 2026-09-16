# ============================================================
# FANTASY MODEL 3.0 - LIVE DYNASTY DRAFT ENGINE
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "tibble", "purrr", "readr"))
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

fm3_select_draft <- function(state, draft_id = NULL) {
  drafts <- state$drafts
  if (!is.null(draft_id) && nzchar(draft_id)) return(as.character(draft_id))
  if (is.null(drafts) || !nrow(drafts)) return(NA_character_)
  active <- drafts |> dplyr::filter(status %in% c("drafting", "pre_draft"))
  if (nrow(active)) return(as.character(active$draft_id[1]))
  current <- drafts |> dplyr::filter(season == CURRENT_SEASON)
  if (nrow(current)) return(as.character(current$draft_id[1]))
  as.character(drafts$draft_id[1])
}

fm3_draft_pick_ownership <- function(draft_id) {
  draft <- sleeper_get_draft(draft_id)
  teams <- suppressWarnings(as.integer(draft$settings$teams %||% length(draft$slot_to_roster_id)))
  rounds <- suppressWarnings(as.integer(draft$settings$rounds %||% 0))
  if (!is.finite(teams) || teams <= 0 || !is.finite(rounds) || rounds <= 0) return(tibble::tibble())
  dtype <- tolower(as.character(draft$type %||% "snake"))
  slot_map <- unlist(draft$slot_to_roster_id %||% list(), use.names = TRUE)
  if (!length(slot_map)) return(tibble::tibble())
  slot_to_roster <- suppressWarnings(as.integer(slot_map))
  names(slot_to_roster) <- names(slot_map)

  grid <- purrr::map_dfr(seq_len(rounds), function(r) {
    purrr::map_dfr(seq_len(teams), function(pir) {
      draft_slot <- if (dtype == "snake" && r %% 2 == 0) teams - pir + 1 else pir
      original <- slot_to_roster[[as.character(draft_slot)]] %||% NA_integer_
      tibble::tibble(
        round = r, pick_in_round = pir, pick_no = (r - 1) * teams + pir,
        draft_slot = draft_slot, original_roster_id = as.integer(original), owner_roster_id = as.integer(original)
      )
    })
  })

  traded_raw <- sleeper_get_draft_traded_picks(draft_id)
  if (length(traded_raw)) {
    traded <- purrr::map_dfr(traded_raw, function(z) tibble::tibble(
      round = suppressWarnings(as.integer(z$round %||% NA)),
      original_roster_id = suppressWarnings(as.integer(z$roster_id %||% NA)),
      owner_roster_id_new = suppressWarnings(as.integer(z$owner_id %||% NA))
    ))
    key <- paste(traded$round, traded$original_roster_id, sep = "|")
    gkey <- paste(grid$round, grid$original_roster_id, sep = "|")
    m <- match(gkey, key); hit <- !is.na(m)
    grid$owner_roster_id[hit] <- traded$owner_roster_id_new[m[hit]]
  }
  grid
}

fm3_infer_rookie_draft <- function(state, draft_id, picks = NULL) {
  draft <- sleeper_get_draft(draft_id)
  rounds <- suppressWarnings(as.integer(draft$settings$rounds %||% 99))
  meta <- paste(unlist(draft$metadata %||% list()), collapse = " ")
  if (grepl("rookie", tolower(meta))) return(TRUE)
  if (!is.finite(rounds)) rounds <- 99
  if (rounds <= DYNASTY_ROOKIE_DRAFT_MAX_ROUNDS) return(TRUE)
  FALSE
}

fm3_draft_pool <- function(state, dynasty_values, draft_id, picks = sleeper_draft_picks_frame(draft_id)) {
  drafted <- unique(as.character(picks$sleeper_id))
  rookie_only <- fm3_infer_rookie_draft(state, draft_id, picks)
  d <- dynasty_values |>
    dplyr::filter(position %in% c("QB","RB","WR","TE"), !sleeper_id %in% drafted)
  if (rookie_only) {
    d <- d |> dplyr::filter((is_rookie %in% 1) | (!is.na(years_exp) & years_exp <= 0))
  }
  if (!nrow(d)) return(d)
  # Prefer an optional user-supplied dynasty ADP file. Otherwise the draft
  # order proxy is derived from Fantasy Model dynasty rank only. Sleeper
  # search rank/projections are intentionally not used in 3.0.8.
  adp_path <- "data/external/dynasty_adp.csv"
  d$.external_adp <- NA_real_
  d$draft_market_source <- "Fantasy Model dynasty rank"
  if (file.exists(adp_path)) {
    adp <- tryCatch(readr::read_csv(adp_path, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(adp) && nrow(adp) && all(c("sleeper_id", "adp") %in% names(adp))) {
      adp <- adp |> dplyr::transmute(sleeper_id = as.character(sleeper_id), .external_adp = suppressWarnings(as.numeric(adp)),
                                     .adp_source = if ("source" %in% names(adp)) as.character(source) else "external ADP") |>
        dplyr::filter(nzchar(sleeper_id), is.finite(.external_adp)) |> dplyr::distinct(sleeper_id, .keep_all = TRUE)
      d <- d |> dplyr::select(-.external_adp) |> dplyr::left_join(adp, by = "sleeper_id") |>
        dplyr::mutate(draft_market_source = dplyr::if_else(is.finite(.external_adp), .adp_source, draft_market_source))
    }
  }
  d <- d |>
    dplyr::mutate(.market_sort = dplyr::case_when(
      is.finite(.external_adp) ~ .external_adp,
      is.finite(as.numeric(model_dynasty_rank)) ~ as.numeric(model_dynasty_rank),
      TRUE ~ 1e9
    )) |>
    dplyr::arrange(.market_sort, dplyr::desc(model_dynasty_value)) |>
    dplyr::mutate(market_draft_rank = dplyr::row_number()) |>
    dplyr::arrange(dplyr::desc(model_dynasty_value))
  d
}

fm3_user_next_pick <- function(ownership, roster_id, current_pick_no) {
  x <- ownership |> dplyr::filter(owner_roster_id == roster_id, pick_no > current_pick_no) |> dplyr::arrange(pick_no)
  if (!nrow(x)) return(NA_integer_)
  as.integer(x$pick_no[1])
}

fm3_position_scarcity <- function(pool, pos, value) {
  x <- pool |> dplyr::filter(position == pos) |> dplyr::arrange(dplyr::desc(model_dynasty_value))
  if (!nrow(x) || !is.finite(value) || value <= 0) return(0)
  idx <- which.min(abs(x$model_dynasty_value - value))
  next_idx <- min(nrow(x), idx + 3)
  drop <- value - x$model_dynasty_value[next_idx]
  pmin(1, pmax(0, drop / value * 4))
}

fm3_pick_survival_probability <- function(market_rank, next_pick_no, current_pick_no, demand_pressure = 0) {
  if (!is.finite(next_pick_no) || next_pick_no <= current_pick_no) return(0)
  picks_until_next <- max(1, next_pick_no - current_pick_no)
  span <- max(2.0, picks_until_next / 3)
  # market_rank is ranked among players still available, so compare it with
  # the number of selections that will occur before the user's next pick.
  base <- stats::plogis((market_rank - picks_until_next) / span)
  pmin(0.99, pmax(0.01, base * (1 - 0.45 * pmin(1, pmax(0, demand_pressure)))))
}

fm3_draft_recommendations <- function(state, dynasty_values, draft_id = NULL, top_n = 12) {
  draft_id <- fm3_select_draft(state, draft_id)
  if (is.na(draft_id) || !nzchar(draft_id)) return(list(error = "No Sleeper draft found for this league."))
  picks <- sleeper_draft_picks_frame(draft_id)
  ownership <- fm3_draft_pick_ownership(draft_id)
  if (!nrow(ownership)) return(list(error = "Draft ownership/order could not be resolved."))
  roster_id <- state$user_roster_id
  if (!is.finite(roster_id)) return(list(error = "Your Sleeper roster could not be identified."))
  current_pick <- if (nrow(picks)) max(picks$pick_no, na.rm = TRUE) + 1L else 1L
  if (!is.finite(current_pick)) current_pick <- 1L
  current_owner <- ownership$owner_roster_id[match(current_pick, ownership$pick_no)]
  next_pick <- fm3_user_next_pick(ownership, roster_id, current_pick)
  settings <- fm3_sleeper_league_settings(state)
  power <- fm3_team_power_table(state, dynasty_values, settings)
  needs <- fm3_team_needs(power, roster_id)
  strategy <- power$strategy[match(roster_id, power$roster_id)] %||% "RETOOL"
  pool <- fm3_draft_pool(state, dynasty_values, draft_id, picks)
  if (!nrow(pool)) return(list(error = "No model-matched draft-eligible players remain."))

  intervening <- ownership |> dplyr::filter(pick_no > current_pick, is.finite(next_pick), pick_no < next_pick)
  demand <- purrr::map_dfr(c("QB","RB","WR","TE"), function(pos) {
    ids <- unique(intervening$owner_roster_id)
    vals <- vapply(ids, function(id) {
      z <- fm3_team_needs(power, id)
      v <- z$need_score[z$position == pos]
      if (length(v)) v[1] else 0.5
    }, numeric(1))
    tibble::tibble(position = pos, demand_pressure = if (length(vals)) mean(vals, na.rm = TRUE) else 0.5)
  })

  need_map <- stats::setNames(needs$need_score, needs$position)
  demand_map <- stats::setNames(demand$demand_pressure, demand$position)
  maxv <- max(pool$model_dynasty_value, na.rm = TRUE); if (!is.finite(maxv) || maxv <= 0) maxv <- 1
  prod01 <- fm3_rescale01(pool$year1_fppg)
  long01 <- fm3_rescale01(pool$model_dynasty_value)
  window_weight <- if (strategy %in% c("CONTENDER", "FRINGE CONTENDER")) 0.75 * prod01 + 0.25 * long01 else 0.20 * prod01 + 0.80 * long01

  rec <- pool |>
    dplyr::mutate(
      need_score = vapply(position, function(p) need_map[[p]] %||% 0.5, numeric(1)),
      demand_pressure = vapply(position, function(p) demand_map[[p]] %||% 0.5, numeric(1)),
      scarcity_score = mapply(function(p, v) fm3_position_scarcity(pool, p, v), position, model_dynasty_value),
      survival_to_next_pick = mapply(function(mr, dp) fm3_pick_survival_probability(mr, next_pick, current_pick, dp), market_draft_rank, demand_pressure),
      loss_risk = 1 - survival_to_next_pick,
      model_score = model_dynasty_value / maxv,
      draft_score = 100 * (0.52 * model_score + 0.14 * need_score + 0.12 * window_weight + 0.10 * scarcity_score + 0.12 * loss_risk),
      recommendation_reason = paste0(
        "Model value ", round(model_dynasty_value), "; ", position, " need ", round(100 * need_score),
        "/100; estimated survival to next pick ", ifelse(is.finite(next_pick), paste0(round(100 * survival_to_next_pick), "%"), "N/A"), "."
      )
    ) |>
    dplyr::arrange(dplyr::desc(draft_score), dplyr::desc(model_dynasty_value)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::mutate(recommendation_rank = dplyr::row_number())

  list(
    draft_id = draft_id,
    picks = picks,
    ownership = ownership,
    current_pick = current_pick,
    current_owner_roster_id = current_owner,
    user_on_clock = isTRUE(current_owner == roster_id),
    user_roster_id = roster_id,
    next_user_pick = next_pick,
    rookie_draft = fm3_infer_rookie_draft(state, draft_id, picks),
    strategy = strategy,
    recommendations = rec,
    available_pool = pool,
    team_power = power,
    needs = needs
  )
}

fm3_trade_down_ideas <- function(draft_result, dynasty_values, max_ideas = 5) {
  if (!isTRUE(draft_result$rookie_draft) || !isTRUE(draft_result$user_on_clock)) return(tibble::tibble())
  if (is.null(draft_result$ownership) || !nrow(draft_result$ownership)) return(tibble::tibble())
  cur <- draft_result$current_pick
  own <- draft_result$ownership
  current_row <- own |> dplyr::filter(pick_no == cur)
  if (!nrow(current_row)) return(tibble::tibble())
  teams <- max(own$pick_in_round, na.rm = TRUE)
  same_round <- own |>
    dplyr::filter(round == current_row$round[1], pick_no > cur, pick_no <= cur + min(6, teams - 1)) |>
    dplyr::distinct(owner_roster_id, pick_no, pick_in_round)
  if (!nrow(same_round)) return(tibble::tibble())
  curve <- fm3_rookie_pick_curve(dynasty_values, teams = teams)
  cur_slot <- current_row$pick_in_round[1]
  cur_value <- curve$pick_value[curve$round == current_row$round[1] & curve$pick_in_round == cur_slot][1]
  same_round |>
    dplyr::rowwise() |>
    dplyr::mutate(
      later_value = curve$pick_value[curve$round == current_row$round[1] & curve$pick_in_round == pick_in_round][1],
      value_gap = pmax(0, cur_value - later_value),
      suggested_extra = dplyr::case_when(
        value_gap >= 2200 ~ "Ask for a future 1st-level add or multiple premium assets",
        value_gap >= 1000 ~ "Ask for a future 2nd plus a smaller add",
        value_gap >= 500 ~ "Ask for a future 2nd/3rd equivalent",
        TRUE ~ "A small future pick or bench asset can close the gap"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(value_gap)) |>
    dplyr::slice_head(n = max_ideas)
}
