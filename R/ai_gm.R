# ============================================================
# FANTASY MODEL 3.0 - AI GM EXPLANATION LAYER
# ============================================================
# The AI does not calculate player values. It receives structured outputs from
# the deterministic dynasty/draft/trade engines and explains the best actions.

source("config.R")
ensure_packages(c("httr2", "jsonlite", "dplyr"))
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

fm3_ai_available <- function() nzchar(Sys.getenv("OPENAI_API_KEY", ""))

fm3_ai_model <- function() {
  x <- Sys.getenv("OPENAI_MODEL", "")
  if (nzchar(x)) x else OPENAI_MODEL_DEFAULT
}

fm3_ai_extract_text <- function(body) {
  if (!is.null(body$output_text) && length(body$output_text)) return(as.character(body$output_text[[1]]))
  out <- body$output
  if (is.null(out) || !length(out)) return("")
  pieces <- character()
  for (item in out) {
    content <- item$content
    if (is.null(content) || !length(content)) next
    for (part in content) {
      txt <- part$text
      if (!is.null(txt) && length(txt)) pieces <- c(pieces, as.character(txt[[1]] %||% txt))
    }
  }
  paste(pieces[nzchar(pieces)], collapse = "\n")
}

fm3_ai_compact_table <- function(d, cols, n = 8) {
  if (is.null(d) || !is.data.frame(d) || !nrow(d)) return("None available")
  cols <- intersect(cols, names(d))
  if (!length(cols)) return("None available")
  x <- utils::head(d[, cols, drop = FALSE], n)
  paste(capture.output(print(x, row.names = FALSE)), collapse = "\n")
}

fm3_build_gm_context <- function(state, dynasty_values, team_power, draft_result = NULL, trade_ideas = NULL, role_forecasts = NULL) {
  rid <- state$user_roster_id
  my_power <- if (!is.null(team_power) && nrow(team_power)) team_power |> dplyr::filter(roster_id == rid) else data.frame()
  my_roster <- fm3_attach_roster_values(state, dynasty_values) |> dplyr::filter(roster_id == rid) |>
    dplyr::arrange(dplyr::desc(model_dynasty_value))
  needs <- if (!is.null(team_power) && nrow(team_power)) fm3_team_needs(team_power, rid) else data.frame()

  draft_text <- "No active draft context loaded."
  if (is.list(draft_result) && is.null(draft_result$error) && !is.null(draft_result$recommendations)) {
    draft_text <- paste0(
      "Current pick: ", draft_result$current_pick, "; next user pick: ", draft_result$next_user_pick,
      "; user on clock: ", draft_result$user_on_clock, "; rookie draft: ", draft_result$rookie_draft, "\n",
      fm3_ai_compact_table(draft_result$recommendations,
        c("recommendation_rank","player_display_name","position","draft_score","model_dynasty_value","need_score","survival_to_next_pick","recommendation_reason"), 10)
    )
  }

  role_text <- "No role-forecast table loaded."
  if (!is.null(role_forecasts) && nrow(role_forecasts)) {
    role_text <- fm3_ai_compact_table(role_forecasts,
      c("player_display_name","position","projected_weekly_fppg_24","projected_targets_30","projected_carries_30","projected_snap_share_30","role_regime_probability_30","role_uncertainty_30"), 10)
  }

  paste0(
    "LEAGUE: ", as.character(state$league$name %||% "Sleeper League"), "\n",
    "USER ROSTER ID: ", rid, "\n\n",
    "TEAM OUTLOOK\n", fm3_ai_compact_table(my_power,
      c("team_name","strategy","contender_index","starter_fppg","roster_dynasty_value","weighted_core_age","future_first_count","title_equity_proxy"), 1), "\n\n",
    "TEAM NEEDS\n", fm3_ai_compact_table(needs, c("position","league_percentile","need_score"), 4), "\n\n",
    "TOP ROSTER ASSETS / CURRENT FANTASY MODEL OUTLOOK\n", fm3_ai_compact_table(my_roster,
      c("player_display_name","position","age","projected_weekly_fppg_24","weekly_floor","weekly_ceiling","projection_confidence","opponent","year1_fppg","year3_fppg","model_dynasty_value","market_value_proxy","model_market_gap_pct","value_signal"), 16), "\n\n",
    "DRAFT STATE\n", draft_text, "\n\n",
    "TRADE IDEAS\n", fm3_ai_compact_table(trade_ideas,
      c("package_text","partner_name","fit_score","your_utility","their_utility","balance_hint"), 10), "\n\n",
    "ROLE / REGIME SIGNALS\n", role_text
  )
}

fm3_ai_fallback <- function(question, state, dynasty_values, team_power, draft_result = NULL, trade_ideas = NULL) {
  rid <- state$user_roster_id
  row <- team_power |> dplyr::filter(roster_id == rid)
  strategy <- if (nrow(row)) as.character(row$strategy[1]) else "RETOOL"
  needs <- fm3_team_needs(team_power, rid)
  need_text <- if (nrow(needs)) paste0(needs$position[1], " is the largest model-identified need") else "No positional need was resolved"
  draft_text <- ""
  if (is.list(draft_result) && is.null(draft_result$error) && !is.null(draft_result$recommendations) && nrow(draft_result$recommendations)) {
    x <- draft_result$recommendations[1, ]
    draft_text <- paste0(" In the current draft, the top computed option is ", x$player_display_name, " (", x$position, ") with a draft score of ", round(x$draft_score, 1), ".")
  }
  trade_text <- ""
  if (!is.null(trade_ideas) && nrow(trade_ideas)) {
    x <- trade_ideas[1, ]
    trade_text <- paste0(" The best current trade framework is sending ", x$send_name, " for ", x$receive_name, " with ", x$partner_name, ".")
  }
  paste0("AI API is not configured, so this is the deterministic GM summary. Your team is classified as ", strategy, ". ", need_text, ".", draft_text, trade_text,
         " Set OPENAI_API_KEY to enable natural-language comparison and follow-up questions.")
}

fm3_ai_answer <- function(question, state, dynasty_values, team_power, draft_result = NULL, trade_ideas = NULL, role_forecasts = NULL) {
  if (!fm3_ai_available()) return(fm3_ai_fallback(question, state, dynasty_values, team_power, draft_result, trade_ideas))
  context <- fm3_build_gm_context(state, dynasty_values, team_power, draft_result, trade_ideas, role_forecasts)
  instructions <- paste(
    "You are the Fantasy Model Dynasty GM Copilot.",
    "Use ONLY the quantitative league context supplied by the application for player values, rankings, draft scores, roster needs, trade fit, and team strategy.",
    "Never invent an asset, roster, pick, probability, injury, or numeric valuation that is not in the context.",
    "If a requested fact is missing, say it is not available in the current model context.",
    "Sleeper supplies only league state, roster ownership, player IDs, drafts, picks, and transactions. Never use or imply Sleeper projections, rankings, or player values.",
    "All football projections and dynasty values in context come from Fantasy Model 2.4.3 / 3.0. If external market_value is present, label it external market data; otherwise the recommendation is model-only.",
    "Treat title_equity_proxy as a relative strength proxy, not a calibrated championship probability.",
    "For draft questions, consider current pick, next pick, survival probability, positional need, scarcity, and competitive window.",
    "For trade questions, favor mutually plausible structures and explain what each side gains.",
    "Be concise but decisive. End with a recommended next move when the context supports one."
  )
  prompt <- paste0("QUESTION\n", question, "\n\nSTRUCTURED LEAGUE CONTEXT\n", context)
  key <- Sys.getenv("OPENAI_API_KEY")
  req <- httr2::request(OPENAI_API_URL) |>
    httr2::req_headers(Authorization = paste("Bearer", key)) |>
    httr2::req_body_json(list(
      model = fm3_ai_model(),
      instructions = instructions,
      input = prompt,
      max_output_tokens = 1200
    ), auto_unbox = TRUE) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 2)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) return(paste0("AI request failed: ", conditionMessage(resp)))
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  txt <- fm3_ai_extract_text(body)
  if (!nzchar(txt)) paste0("AI returned no text. Model: ", fm3_ai_model()) else txt
}
