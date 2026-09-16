# ============================================================
# FANTASY MODEL 3.0 - DYNASTY TRADE FINDER / EVALUATOR
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "tibble", "purrr"))
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

fm3_strategy_utility <- function(strategy, production_delta, dynasty_delta, age_delta = 0) {
  if (strategy == "CONTENDER") return(0.62 * production_delta + 0.38 * dynasty_delta / 500)
  if (strategy == "FRINGE CONTENDER") return(0.52 * production_delta + 0.48 * dynasty_delta / 500)
  if (strategy %in% c("REBUILD", "DEEP REBUILD")) return(0.20 * production_delta + 0.80 * dynasty_delta / 500 - 0.10 * age_delta)
  0.38 * production_delta + 0.62 * dynasty_delta / 500 - 0.05 * age_delta
}

fm3_pick_asset_table <- function(state, team_power, dynasty_values) {
  picks <- fm3_all_future_picks(state)
  if (!nrow(picks)) return(tibble::tibble())
  teams <- nrow(team_power)
  curve <- fm3_rookie_pick_curve(dynasty_values, teams = teams, rounds = max(picks$round, na.rm = TRUE))
  strength <- stats::setNames(team_power$production_percentile, team_power$roster_id)
  picks |>
    dplyr::rowwise() |>
    dplyr::mutate(
      owner_strength = strength[[as.character(original_roster_id)]] %||% 0.5,
      expected_slot = pmax(1, pmin(teams, round(1 + owner_strength * (teams - 1)))),
      pick_value = fm3_future_pick_value(curve, season, round, expected_slot),
      asset_id = paste(season, round, original_roster_id, owner_roster_id, sep = "|"),
      asset_label = paste0(season, " R", round, " (orig roster ", original_roster_id, ")")
    ) |>
    dplyr::ungroup()
}

fm3_evaluate_trade <- function(state, dynasty_values, send_sleeper_ids = character(), receive_sleeper_ids = character(),
                               send_pick_rows = NULL, receive_pick_rows = NULL, partner_roster_id = NA_integer_,
                               team_power = NULL) {
  settings <- fm3_sleeper_league_settings(state)
  power <- if (!is.null(team_power) && is.data.frame(team_power) && nrow(team_power)) team_power else fm3_team_power_table(state, dynasty_values, settings)
  rid <- state$user_roster_id
  strategy <- power$strategy[match(rid, power$roster_id)] %||% "RETOOL"
  vals <- dynasty_values
  send <- vals |> dplyr::filter(sleeper_id %in% send_sleeper_ids)
  rec <- vals |> dplyr::filter(sleeper_id %in% receive_sleeper_ids)
  send_pick_value <- if (!is.null(send_pick_rows) && nrow(send_pick_rows)) sum(send_pick_rows$pick_value, na.rm = TRUE) else 0
  rec_pick_value <- if (!is.null(receive_pick_rows) && nrow(receive_pick_rows)) sum(receive_pick_rows$pick_value, na.rm = TRUE) else 0
  send_value <- sum(send$model_dynasty_value, na.rm = TRUE) + send_pick_value
  rec_value <- sum(rec$model_dynasty_value, na.rm = TRUE) + rec_pick_value
  send_prod <- sum(send$year1_fppg, na.rm = TRUE)
  rec_prod <- sum(rec$year1_fppg, na.rm = TRUE)
  age_send <- if (nrow(send)) stats::weighted.mean(send$age, pmax(send$model_dynasty_value, 1), na.rm = TRUE) else 0
  age_rec <- if (nrow(rec)) stats::weighted.mean(rec$age, pmax(rec$model_dynasty_value, 1), na.rm = TRUE) else 0
  fairness <- if (max(send_value, rec_value) > 0) 1 - abs(send_value - rec_value) / max(send_value, rec_value) else 1
  utility <- fm3_strategy_utility(strategy, rec_prod - send_prod, rec_value - send_value, age_rec - age_send)
  partner_strategy <- if (is.finite(suppressWarnings(as.numeric(partner_roster_id)))) {
    power$strategy[match(as.integer(partner_roster_id), power$roster_id)] %||% "RETOOL"
  } else NA_character_
  partner_utility <- if (!is.na(partner_strategy)) {
    fm3_strategy_utility(partner_strategy, send_prod - rec_prod, send_value - rec_value, age_send - age_rec)
  } else NA_real_
  list(
    strategy = strategy,
    partner_strategy = partner_strategy,
    send_value = send_value, receive_value = rec_value,
    dynasty_delta = rec_value - send_value,
    production_delta = rec_prod - send_prod,
    fairness_score = 100 * pmax(0, pmin(1, fairness)),
    strategy_utility = utility,
    partner_strategy_utility = partner_utility,
    mutual_fit_score = if (is.finite(partner_utility)) 100 * pmax(0, pmin(1, 0.60 * fairness + 0.20 * stats::plogis(utility) + 0.20 * stats::plogis(partner_utility))) else 100 * pmax(0, pmin(1, fairness)),
    verdict = dplyr::case_when(
      utility >= 1.5 && fairness >= 0.80 ~ "STRONG ACCEPT",
      utility >= 0.4 && fairness >= 0.75 ~ "ACCEPT",
      utility >= -0.4 && fairness >= 0.85 ~ "FAIR / TEAM-DEPENDENT",
      utility < -1.5 ~ "DECLINE",
      TRUE ~ "NEGOTIATE"
    )
  )
}

fm3_trade_finder <- function(state, dynasty_values, max_ideas = 20,
                             team_power = NULL, roster_values = NULL, pick_assets = NULL) {
  rid <- state$user_roster_id
  if (!is.finite(rid)) return(tibble::tibble())
  settings <- fm3_sleeper_league_settings(state)
  power <- if (!is.null(team_power) && is.data.frame(team_power) && nrow(team_power)) team_power else fm3_team_power_table(state, dynasty_values, settings)
  rp <- if (!is.null(roster_values) && is.data.frame(roster_values) && nrow(roster_values)) roster_values else fm3_attach_roster_values(state, dynasty_values)
  mine <- rp |> dplyr::filter(roster_id == rid, model_dynasty_value >= 600, position %in% c("QB","RB","WR","TE"))
  if (!nrow(mine)) return(tibble::tibble())
  my_needs <- fm3_team_needs(power, rid)
  my_strategy <- power$strategy[match(rid, power$roster_id)] %||% "RETOOL"
  partner_ids <- setdiff(unique(rp$roster_id), rid)

  ideas <- purrr::map_dfr(partner_ids, function(pid) {
    theirs <- rp |> dplyr::filter(roster_id == pid, model_dynasty_value >= 600, position %in% c("QB","RB","WR","TE"))
    if (!nrow(theirs)) return(tibble::tibble())
    partner_needs <- fm3_team_needs(power, pid)
    partner_strategy <- power$strategy[match(pid, power$roster_id)] %||% "RETOOL"
    my_need_map <- stats::setNames(my_needs$need_score, my_needs$position)
    p_need_map <- stats::setNames(partner_needs$need_score, partner_needs$position)

    cand <- merge(
      mine |> dplyr::transmute(send_id = sleeper_id, send_name = player_display_name, send_pos = position,
                               send_value = model_dynasty_value, send_prod = year1_fppg, send_age = age),
      theirs |> dplyr::transmute(receive_id = sleeper_id, receive_name = player_display_name, receive_pos = position,
                                 receive_value = model_dynasty_value, receive_prod = year1_fppg, receive_age = age),
      by = NULL
    ) |> tibble::as_tibble()
    cand |>
      dplyr::mutate(
        value_ratio = receive_value / pmax(send_value, 1),
        fairness = 1 - abs(receive_value - send_value) / pmax(receive_value, send_value, 1),
        your_need_fit = vapply(receive_pos, function(p) my_need_map[[p]] %||% 0.5, numeric(1)),
        their_need_fit = vapply(send_pos, function(p) p_need_map[[p]] %||% 0.5, numeric(1)),
        your_utility = mapply(function(pd, dd, ad) fm3_strategy_utility(my_strategy, pd, dd, ad),
                              receive_prod - send_prod, receive_value - send_value, receive_age - send_age),
        their_utility = mapply(function(pd, dd, ad) fm3_strategy_utility(partner_strategy, pd, dd, ad),
                               send_prod - receive_prod, send_value - receive_value, send_age - receive_age),
        mutual_fit = 0.38 * pmax(0, fairness) + 0.20 * your_need_fit + 0.20 * their_need_fit +
          0.11 * stats::plogis(your_utility) + 0.11 * stats::plogis(their_utility),
        partner_roster_id = pid,
        partner_name = power$team_name[match(pid, power$roster_id)],
        partner_strategy = partner_strategy,
        your_strategy = my_strategy
      ) |>
      dplyr::filter(value_ratio >= 0.55, value_ratio <= 1.80, mutual_fit >= 0.48)
  })

  if (!nrow(ideas)) return(ideas)
  picks <- if (!is.null(pick_assets) && is.data.frame(pick_assets)) pick_assets else fm3_pick_asset_table(state, power, dynasty_values)
  pick_for_gap <- function(owner, gap) {
    if (!nrow(picks) || !is.finite(gap) || gap < 450) return(list(label = "", value = 0))
    z <- picks |> dplyr::filter(owner_roster_id == owner)
    if (!nrow(z)) return(list(label = "", value = 0))
    z <- z |> dplyr::mutate(distance = abs(pick_value - gap)) |> dplyr::arrange(distance, dplyr::desc(pick_value))
    list(label = as.character(z$asset_label[1]), value = as.numeric(z$pick_value[1]))
  }

  out <- ideas |>
    dplyr::arrange(dplyr::desc(mutual_fit), dplyr::desc(your_utility)) |>
    dplyr::distinct(send_id, receive_id, .keep_all = TRUE) |>
    dplyr::slice_head(n = max_ideas)

  packages <- purrr::pmap_dfr(out, function(...) {
    r <- list(...)
    gap <- as.numeric(r$receive_value) - as.numeric(r$send_value)
    if (abs(gap) < 450) {
      return(tibble::tibble(balance_pick = "", balance_pick_value = 0,
                            package_text = paste0(r$send_name, " for ", r$receive_name)))
    }
    if (gap > 0) {
      pk <- pick_for_gap(rid, gap)
      txt <- if (nzchar(pk$label)) paste0(r$send_name, " + ", pk$label, " for ", r$receive_name) else paste0(r$send_name, " + secondary asset for ", r$receive_name)
    } else {
      pk <- pick_for_gap(as.integer(r$partner_roster_id), abs(gap))
      txt <- if (nzchar(pk$label)) paste0(r$send_name, " for ", r$receive_name, " + ", pk$label) else paste0(r$send_name, " for ", r$receive_name, " + secondary asset")
    }
    tibble::tibble(balance_pick = pk$label, balance_pick_value = pk$value, package_text = txt)
  })

  dplyr::bind_cols(out, packages) |>
    dplyr::mutate(
      package_send_value = send_value + dplyr::if_else(receive_value > send_value, balance_pick_value, 0),
      package_receive_value = receive_value + dplyr::if_else(send_value > receive_value, balance_pick_value, 0),
      package_fairness = 1 - abs(package_receive_value - package_send_value) / pmax(package_receive_value, package_send_value, 1),
      package_your_utility = mapply(function(pd, dd, ad) fm3_strategy_utility(my_strategy, pd, dd, ad),
                                    receive_prod - send_prod, package_receive_value - package_send_value, receive_age - send_age),
      package_their_utility = mapply(function(pd, dd, ad, ps) fm3_strategy_utility(ps, pd, dd, ad),
                                     send_prod - receive_prod, package_send_value - package_receive_value, send_age - receive_age, partner_strategy),
      package_mutual_fit = 0.45 * pmax(0, package_fairness) + 0.18 * your_need_fit + 0.18 * their_need_fit +
        0.095 * stats::plogis(package_your_utility) + 0.095 * stats::plogis(package_their_utility),
      fit_score = round(100 * package_mutual_fit),
      your_utility = package_your_utility,
      their_utility = package_their_utility,
      balance_hint = dplyr::case_when(
        balance_pick_value > 0 & receive_value > send_value ~ "Package adds the closest future pick you own to reduce the model-value gap.",
        balance_pick_value > 0 & send_value > receive_value ~ "Package asks for the closest future pick they own to reduce the model-value gap.",
        receive_value > send_value * 1.18 ~ "Your side still needs additional value; no owned future pick was close enough to resolve automatically.",
        send_value > receive_value * 1.18 ~ "Their side still needs additional value; no owned future pick was close enough to resolve automatically.",
        TRUE ~ "Core values are close enough for a direct framework."
      )
    ) |>
    dplyr::arrange(dplyr::desc(package_mutual_fit), dplyr::desc(your_utility))
}
