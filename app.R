# ============================================================
# FANTASY MODEL 3.0 - DYNASTY INTELLIGENCE APPLICATION
# ============================================================
source("config.R")
ensure_packages(c("shiny", "dplyr", "readr", "httr2", "jsonlite", "purrr", "tibble", "stringr"))

library(shiny)
library(dplyr)
library(readr)

source("R/league_engine.R")
source("R/sleeper_api.R")
source("R/player_identity.R")
source("R/dynasty_value_engine.R")
source("R/team_strategy_engine.R")
source("R/draft_engine.R")
source("R/trade_engine.R")
source("R/ai_gm.R")
source("R/data_quality_engine.R")
source("R/app_data_bridge.R")

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

saved_cfg <- sleeper_read_config()
default_username <- if (!is.null(saved_cfg) && nrow(saved_cfg)) as.character(saved_cfg$username[1]) else ""
default_user_id <- if (!is.null(saved_cfg) && nrow(saved_cfg)) as.character(saved_cfg$user_id[1]) else ""
default_league_id <- if (!is.null(saved_cfg) && nrow(saved_cfg)) as.character(saved_cfg$league_id[1]) else ""
default_league_name <- if (!is.null(saved_cfg) && nrow(saved_cfg)) as.character(saved_cfg$league_name[1]) else "Saved League"
initial_state <- NULL  # 3.0.7: never rebuild cached league analysis automatically at app startup

fm3_existing <- function(d, cols) {
  if (is.null(d) || !is.data.frame(d)) return(data.frame())
  cols <- intersect(cols, names(d))
  d[, cols, drop = FALSE]
}

fm3_pct <- function(x, digits = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(x), paste0(round(100 * x, digits), "%"), "—")
}

fm3_clean_error <- function(e) {
  msg <- conditionMessage(e)
  # rlang/vctrs can include ANSI styling in condition text.  Browsers display
  # those escape bytes as boxes, so strip them before showing Shiny status.
  gsub("\033\\[[0-9;]*m", "", msg, perl = TRUE)
}

# Build Shiny select choices defensively. Reactive UI controls can briefly have
# zero rows while a league snapshot and its dependent controls settle. `setNames()`
# throws when value/label lengths differ, which should never be allowed to terminate
# the entire app session.
fm3_named_choices <- function(values = character(), labels = NULL) {
  values <- as.character(values %||% character())
  if (!length(values)) return(stats::setNames(character(), character()))

  labels <- as.character(labels %||% character())
  if (!length(labels)) labels <- values
  if (length(labels) == 1L && length(values) > 1L) labels <- rep(labels, length(values))

  n <- min(length(values), length(labels))
  if (n < 1L) return(stats::setNames(character(), character()))
  values <- values[seq_len(n)]
  labels <- labels[seq_len(n)]
  labels[is.na(labels) | !nzchar(labels)] <- values[is.na(labels) | !nzchar(labels)]
  keep <- !is.na(values) & nzchar(values)
  stats::setNames(values[keep], labels[keep])
}

metric_card <- function(title, value, subtitle = NULL, accent = FALSE) {
  div(class = paste("gm-card", if (accent) "gm-card-accent" else ""),
      div(class = "gm-card-title", title),
      div(class = "gm-card-value", value),
      if (!is.null(subtitle)) div(class = "gm-card-sub", subtitle))
}

connection_banner <- function() {
  div(class = "gm-banner",
      strong("Dynasty GM Copilot"),
      span(" · Sleeper roster/draft state only + Fantasy Model 2.4.3/3.0 projections, values, role signals + optional AI explanation"))
}

ui <- navbarPage(
  title = paste0("Fantasy Model ", APP_VERSION),
  id = "main_nav",
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "dynasty.css"),
      tags$script(HTML("
        $(document).on('shiny:disconnected', function() {
          $('#fm-disconnected-banner').show();
        });
        $(document).on('shiny:connected', function() {
          $('#fm-disconnected-banner').hide();
        });
      "))
    ),
    connection_banner(),
    tags$div(id = "fm-disconnected-banner", class = "fm-disconnected-banner",
             tags$b("App server disconnected."),
             tags$span(" Restart the R session and launch with runners/RUN_DYNASTY_APP_CLEAN.R. This usually means the Posit session ran out of memory or the Shiny process stopped.")),
    tags$div(id = "fm-busy-indicator", class = "fm-busy-indicator",
             tags$span(class = "fm-busy-dot"), tags$span("Working…"))
  ),

  tabPanel("GM Dashboard",
    fluidPage(
      br(),
      uiOutput("connection_status"),
      fluidRow(
        column(3, uiOutput("strategy_card")),
        column(3, uiOutput("contender_card")),
        column(3, uiOutput("age_card")),
        column(3, uiOutput("picks_card"))
      ),
      fluidRow(
        column(7,
          h3("My roster"),
          p(class = "gm-muted", "Roster ownership comes from Sleeper. Weekly points, uncertainty, season/career outlook and dynasty value come from Fantasy Model. External market value appears only if you provide a market dataset."),
          tableOutput("my_roster_table")
        ),
        column(5,
          h3("Team needs"),
          tableOutput("team_needs_table"),
          h3("League power"),
          tableOutput("league_power_table")
        )
      )
    )
  ),

  tabPanel("Draft Room",
    fluidPage(
      br(),
      fluidRow(
        column(8,
          h2("Live Draft Room"),
          p(class = "gm-muted", "Refresh after picks are made. Recommendations use Fantasy Model dynasty value, roster need, scarcity, team window, and model/optional external-ADP survival estimates. Sleeper supplies only the live draft state.")),
        column(4, actionButton("refresh_draft", "Refresh Draft", class = "btn-primary"))
      ),
      uiOutput("draft_summary"),
      h3("Recommended picks"),
      tableOutput("draft_recommendations"),
      fluidRow(
        column(6, h3("Recent selections"), tableOutput("recent_draft_picks")),
        column(6, h3("Trade-down frameworks"), tableOutput("trade_down_table"))
      )
    )
  ),

  tabPanel("Trade Center",
    fluidPage(
      br(),
      h2("Trade Center"),
      p(class = "gm-muted", "The finder prioritizes mutually plausible structures: your roster fit, the other team's need, both competitive windows, and value fairness."),
      actionButton("refresh_trades", "Refresh Trade Ideas", class = "btn-primary"),
      h3("Best current frameworks"),
      tableOutput("trade_ideas_table"),
      hr(),
      h3("Evaluate a custom package"),
      selectInput("trade_partner", "Trade partner", choices = character()),
      fluidRow(
        column(6, selectizeInput("trade_send", "Players you send", choices = NULL, multiple = TRUE)),
        column(6, selectizeInput("trade_receive", "Players you receive", choices = NULL, multiple = TRUE))
      ),
      fluidRow(
        column(6, selectizeInput("trade_send_picks", "Picks you send", choices = NULL, multiple = TRUE)),
        column(6, selectizeInput("trade_receive_picks", "Picks you receive", choices = NULL, multiple = TRUE))
      ),
      actionButton("evaluate_trade", "Evaluate Package", class = "btn-primary"),
      uiOutput("trade_evaluation")
    )
  ),

  tabPanel("GM Assistant",
    fluidPage(
      br(),
      h2("AI GM Assistant"),
      p(class = "gm-muted", "The AI receives the outputs of the value, team, draft, trade, and role engines. It is instructed not to invent player values or league facts."),
      textAreaInput("gm_question", "Ask about your next move", rows = 4, width = "100%",
                    value = "What should I do next to improve this dynasty team?"),
      actionButton("ask_gm", "Ask GM", class = "btn-primary"),
      span(class = "gm-muted", textOutput("ai_status", inline = TRUE)),
      hr(),
      verbatimTextOutput("gm_answer")
    )
  ),

  tabPanel("Weekly + Role Lab",
    fluidPage(
      br(),
      h2("Weekly projection + 3.0 role challenger"),
      p(class = "gm-muted", "2.4.3 remains the production point forecast. The new role engine stays in shadow mode unless each role target beats its historical baseline."),
      tableOutput("role_roster_table"),
      h3("Role-model promotion results"),
      tableOutput("role_validation_table")
    )
  ),

  tabPanel("Data Health",
    fluidPage(
      br(),
      h2("Data & Model Health"),
      actionButton("refresh_health", "Refresh Health"),
      tableOutput("data_health_table"),
      h3("Sleeper identity coverage"),
      uiOutput("identity_coverage")
    )
  ),

  tabPanel("Setup",
    fluidPage(
      br(),
      h2("Sleeper Dynasty Integration"),
      p("Sleeper is used only for league settings, roster ownership, player IDs, drafts and pick ownership. Player projections, role forecasts and dynasty value come from Fantasy Model outputs; Sleeper projections/rankings are not used."),
      fluidRow(
        column(4, textInput("sleeper_username", "Sleeper username", value = default_username)),
        column(2, numericInput("sleeper_season", "League season", value = CURRENT_SEASON, min = 2017, max = CURRENT_SEASON + 1, step = 1)),
        column(2, br(), actionButton("load_leagues", "Load Leagues")),
        column(4, selectInput("sleeper_league", "League", choices = if (nzchar(default_league_id)) fm3_named_choices(default_league_id, default_league_name) else character()))
      ),
      actionButton("connect_league", "Connect / Refresh League", class = "btn-primary"),
      actionButton("clear_sleeper_cache", "Reset League Cache", class = "btn-default"),
      hr(),
      h3("Optional AI configuration"),
      p("Set OPENAI_API_KEY in your environment to enable natural-language GM answers. The deterministic draft/trade/team engines work without it."),
      pre('Sys.setenv(OPENAI_API_KEY = "YOUR_KEY")\n# Optional: Sys.setenv(OPENAI_MODEL = "gpt-5.6-luna")'),
      p(class = "gm-muted", "For a persistent local key, put the same variable in your user .Renviron file. Never commit the key to this project."),
      h3("Current status"),
      verbatimTextOutput("setup_status")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    user_id = default_user_id,
    leagues = data.frame(),
    state = initial_state,
    dynasty = NULL,
    team_power = NULL,
    roster_values = NULL,
    pick_assets = NULL,
    trades = NULL,
    draft = NULL,
    trade_down = NULL,
    gm_answer = "Connect a Sleeper league, then ask a question.",
    status = if (nzchar(default_league_id)) "Saved Sleeper league found. Click Connect / Refresh League to activate it in this clean app session." else "No league connected.",
    health_tick = 0
  )

  role_forecasts <- reactive({
    # Compact model-only snapshot built before Shiny starts. This avoids parsing
    # the 10k-row role/weekly files inside a connected mobile session.
    fm3_load_current_week_snapshot()
  })

  rebuild_analysis <- function(state, refresh_draft = FALSE, refresh_trades = FALSE) {
    # Base league analysis is intentionally separated from connected feature
    # refreshes.  A league connect should build the reusable local snapshot once;
    # Draft Room / Trade Center then operate from that snapshot on demand.
    state$players <- fm3_identity_sleeper_schema(state$players)
    if (!is.null(state$roster_players) && is.data.frame(state$roster_players)) {
      rp <- tibble::as_tibble(state$roster_players)
      if (!"sleeper_id" %in% names(rp)) rp[["sleeper_id"]] <- rep("", nrow(rp))
      state$roster_players <- rp
    }

    settings <- fm3_sleeper_league_settings(state)
    dynasty <- fm3_build_dynasty_values(settings, state$players)
    roster_values <- fm3_attach_roster_values(state, dynasty)
    power <- fm3_team_power_table(state, dynasty, settings)
    pick_assets <- tryCatch(
      fm3_pick_asset_table(state, power, dynasty),
      error = function(e) tibble::tibble()
    )

    # Network-/search-heavy feature engines are opt-in so simply connecting a
    # league or changing tabs does not lock the mobile Shiny session.
    trades <- if (isTRUE(refresh_trades)) {
      tryCatch(
        fm3_trade_finder(state, dynasty, team_power = power,
                         roster_values = roster_values, pick_assets = pick_assets),
        error = function(e) data.frame()
      )
    } else NULL
    draft <- if (isTRUE(refresh_draft)) {
      tryCatch(fm3_draft_recommendations(state, dynasty),
               error = function(e) list(error = fm3_clean_error(e)))
    } else NULL
    td <- if (is.list(draft) && is.null(draft$error)) {
      tryCatch(fm3_trade_down_ideas(draft, dynasty), error = function(e) data.frame())
    } else data.frame()

    rv$state <- state
    rv$dynasty <- dynasty
    rv$team_power <- power
    rv$roster_values <- roster_values
    rv$pick_assets <- pick_assets
    rv$trades <- trades
    rv$draft <- draft
    rv$trade_down <- td
    invisible(TRUE)
  }

  observeEvent(input$load_leagues, {
    req(nzchar(trimws(input$sleeper_username)))
    rv$status <- "Loading Sleeper leagues..."
    tryCatch({
      u <- sleeper_get_user(trimws(input$sleeper_username))
      uid <- as.character(u$user_id %||% "")
      if (!nzchar(uid)) stop("Sleeper user was not found.")
      lg <- sleeper_get_user_leagues(uid, as.integer(input$sleeper_season))
      if (!nrow(lg)) stop("No NFL leagues found for that Sleeper user and season.")
      rv$user_id <- uid
      rv$leagues <- lg
      updateSelectInput(session, "sleeper_league", choices = fm3_named_choices(lg$league_id, paste0(lg$league_name, " · ", lg$status)))
      rv$status <- paste("Loaded", nrow(lg), "league(s). Select one and connect.")
    }, error = function(e) {
      rv$status <- paste("Sleeper load failed:", fm3_clean_error(e))
      showNotification(rv$status, type = "error")
    })
  })

  observeEvent(input$clear_sleeper_cache, {
    fm3_clear_sleeper_cache(as.character(input$sleeper_league %||% ""), clear_players = FALSE)
    rv$state <- NULL; rv$dynasty <- NULL; rv$team_power <- NULL; rv$roster_values <- NULL; rv$pick_assets <- NULL; rv$trades <- NULL; rv$draft <- NULL
    rv$status <- "League-state cache reset. The compact Sleeper player identity cache was kept. Click Connect / Refresh League."
    showNotification("League-state cache reset.", type = "message")
  })

  observeEvent(input$connect_league, {
    req(nzchar(as.character(input$sleeper_league)))
    uid <- rv$user_id
    if (!nzchar(uid) && nzchar(default_user_id)) uid <- default_user_id
    if (!nzchar(uid)) {
      showNotification("Load your Sleeper leagues first so the app can identify your roster.", type = "error")
      return()
    }
    rv$status <- "Refreshing lightweight Sleeper roster/draft state, then applying Fantasy Model projections..."
    tryCatch({
      gc(verbose = FALSE)
      state <- sleeper_build_league_state(as.character(input$sleeper_league), uid, force_players = FALSE)
      name <- as.character(state$league$name %||% "Sleeper League")
      sleeper_save_config(input$sleeper_username, uid, state$league_id, name)
      rebuild_analysis(state, refresh_draft = FALSE, refresh_trades = FALSE)
      gc(verbose = FALSE)
      rv$status <- paste0("Connected to ", name, " at ", format(Sys.time(), "%I:%M:%S %p"), ". Sleeper = roster/draft state only; Fantasy Model = projections, role and dynasty intelligence.")
      rv$health_tick <- rv$health_tick + 1
      showNotification("Sleeper league refreshed.", type = "message")
    }, error = function(e) {
      rv$status <- paste("League refresh failed:", fm3_clean_error(e))
      showNotification(rv$status, type = "error", duration = 8)
    })
  })

  observeEvent(input$refresh_draft, {
    req(!is.null(rv$state), !is.null(rv$dynasty))
    rv$status <- "Refreshing live draft state…"
    tryCatch({
      rv$draft <- fm3_draft_recommendations(rv$state, rv$dynasty)
      rv$trade_down <- if (is.null(rv$draft$error)) fm3_trade_down_ideas(rv$draft, rv$dynasty) else data.frame()
      rv$status <- paste("Draft refreshed", format(Sys.time(), "%I:%M:%S %p"))
    }, error = function(e) {
      rv$status <- paste("Draft refresh failed:", fm3_clean_error(e))
      showNotification(fm3_clean_error(e), type = "error")
    })
  })

  observeEvent(input$refresh_trades, {
    req(!is.null(rv$state), !is.null(rv$dynasty), !is.null(rv$team_power))
    rv$status <- "Building trade ideas from the cached league snapshot…"
    tryCatch({
      rv$trades <- fm3_trade_finder(
        rv$state, rv$dynasty,
        team_power = rv$team_power,
        roster_values = rv$roster_values,
        pick_assets = rv$pick_assets
      )
      rv$status <- paste("Trade ideas refreshed", format(Sys.time(), "%I:%M:%S %p"))
    }, error = function(e) {
      rv$trades <- data.frame()
      rv$status <- paste("Trade refresh failed:", fm3_clean_error(e))
      showNotification(fm3_clean_error(e), type = "error")
    })
  })

  # Trade controls are downstream conveniences, not part of league connection.
  # Keep them isolated from the core session so an empty/transient partner table can
  # never disconnect the app after Sleeper has already loaded successfully.
  observe({
    if (is.null(rv$state) || is.null(rv$dynasty) || is.null(rv$team_power)) return()
    tryCatch({
      rid <- rv$state$user_roster_id
      partners <- rv$team_power |> dplyr::filter(roster_id != rid) |> dplyr::arrange(team_name)
      current <- isolate(input$trade_partner)
      choices <- fm3_named_choices(partners$roster_id, partners$team_name)
      selected <- if (length(current) == 1L && current %in% unname(choices)) {
        current
      } else if (length(choices)) {
        unname(choices[[1]])
      } else {
        character()
      }
      updateSelectInput(session, "trade_partner", choices = choices, selected = selected)
    }, error = function(e) {
      updateSelectInput(session, "trade_partner", choices = character(), selected = character())
      message("[3.0.9 UI] Trade-partner control skipped safely: ", fm3_clean_error(e))
    })
  })

  observe({
    if (is.null(rv$state) || is.null(rv$dynasty) || is.null(rv$team_power)) return()
    tryCatch({
      rid <- rv$state$user_roster_id
      pid_raw <- input$trade_partner %||% NA_character_
      pid <- suppressWarnings(as.integer(pid_raw[1]))
      valid_pid <- length(pid) == 1L && is.finite(pid)

      rp <- rv$roster_values
      if (is.null(rp) || !is.data.frame(rp)) rp <- fm3_attach_roster_values(rv$state, rv$dynasty)
      rp <- tibble::as_tibble(rp)
      required_player_cols <- c("roster_id", "sleeper_id", "player_display_name", "position", "model_dynasty_value")
      if (!all(required_player_cols %in% names(rp))) {
        missing <- setdiff(required_player_cols, names(rp))
        stop("Roster value table is missing: ", paste(missing, collapse = ", "))
      }
      rp <- rp |> dplyr::filter(is.finite(model_dynasty_value), model_dynasty_value > 0)
      mine <- rp |> dplyr::filter(roster_id == rid) |> dplyr::arrange(dplyr::desc(model_dynasty_value))
      theirs <- if (valid_pid) {
        rp |> dplyr::filter(roster_id == pid) |> dplyr::arrange(dplyr::desc(model_dynasty_value))
      } else {
        rp[0, , drop = FALSE]
      }

      mine_labels <- if (nrow(mine)) paste0(mine$player_display_name, " · ", mine$position, " · ", round(mine$model_dynasty_value)) else character()
      their_labels <- if (nrow(theirs)) paste0(theirs$player_display_name, " · ", theirs$position, " · ", round(theirs$model_dynasty_value)) else character()
      updateSelectizeInput(session, "trade_send", choices = fm3_named_choices(mine$sleeper_id, mine_labels), server = TRUE)
      updateSelectizeInput(session, "trade_receive", choices = fm3_named_choices(theirs$sleeper_id, their_labels), server = TRUE)

      picks <- rv$pick_assets
      if (is.null(picks) || !is.data.frame(picks)) {
        picks <- tryCatch(fm3_pick_asset_table(rv$state, rv$team_power, rv$dynasty), error = function(e) tibble::tibble())
      }
      picks <- tibble::as_tibble(picks)
      required_pick_cols <- c("owner_roster_id", "asset_id", "asset_label", "pick_value", "season", "round", "original_roster_id")
      if (nrow(picks) && all(required_pick_cols %in% names(picks))) {
        my_picks <- picks |> dplyr::filter(owner_roster_id == rid) |> dplyr::arrange(season, round, original_roster_id)
        their_picks <- if (valid_pid) picks |> dplyr::filter(owner_roster_id == pid) |> dplyr::arrange(season, round, original_roster_id) else picks[0, , drop = FALSE]
      } else {
        my_picks <- tibble::tibble()
        their_picks <- tibble::tibble()
      }
      my_pick_values <- if (nrow(my_picks)) my_picks$asset_id else character()
      my_pick_labels <- if (nrow(my_picks)) paste0(my_picks$asset_label, " · ", round(my_picks$pick_value)) else character()
      their_pick_values <- if (nrow(their_picks)) their_picks$asset_id else character()
      their_pick_labels <- if (nrow(their_picks)) paste0(their_picks$asset_label, " · ", round(their_picks$pick_value)) else character()
      updateSelectizeInput(session, "trade_send_picks", choices = fm3_named_choices(my_pick_values, my_pick_labels), server = TRUE)
      updateSelectizeInput(session, "trade_receive_picks", choices = fm3_named_choices(their_pick_values, their_pick_labels), server = TRUE)
    }, error = function(e) {
      updateSelectizeInput(session, "trade_send", choices = character(), server = TRUE)
      updateSelectizeInput(session, "trade_receive", choices = character(), server = TRUE)
      updateSelectizeInput(session, "trade_send_picks", choices = character(), server = TRUE)
      updateSelectizeInput(session, "trade_receive_picks", choices = character(), server = TRUE)
      message("[3.0.9 UI] Trade asset controls skipped safely: ", fm3_clean_error(e))
    })
  })

  trade_eval <- eventReactive(input$evaluate_trade, {
    req(!is.null(rv$state), !is.null(rv$dynasty), !is.null(rv$team_power))
    pid <- suppressWarnings(as.integer(input$trade_partner))
    picks <- rv$pick_assets %||% fm3_pick_asset_table(rv$state, rv$team_power, rv$dynasty)
    send_pick_rows <- picks |> dplyr::filter(asset_id %in% (input$trade_send_picks %||% character()))
    receive_pick_rows <- picks |> dplyr::filter(asset_id %in% (input$trade_receive_picks %||% character()))
    send_ids <- input$trade_send %||% character()
    receive_ids <- input$trade_receive %||% character()
    if (!length(send_ids) && !nrow(send_pick_rows)) return(NULL)
    if (!length(receive_ids) && !nrow(receive_pick_rows)) return(NULL)
    fm3_evaluate_trade(rv$state, rv$dynasty, send_ids, receive_ids, send_pick_rows, receive_pick_rows, pid, team_power = rv$team_power)
  }, ignoreInit = TRUE)

  observeEvent(input$ask_gm, {
    req(!is.null(rv$state), !is.null(rv$dynasty), !is.null(rv$team_power))
    rv$gm_answer <- "Thinking from the current quantitative league state..."
    tryCatch({
      role <- role_forecasts()
      if (nrow(role)) {
        rp <- (rv$roster_values %||% fm3_attach_roster_values(rv$state, rv$dynasty)) |> dplyr::filter(roster_id == rv$state$user_roster_id)
        ids <- unique(rp$gsis_id[nzchar(rp$gsis_id)])
        role <- role |> dplyr::filter(player_id %in% ids)
      }
      rv$gm_answer <- fm3_ai_answer(input$gm_question, rv$state, rv$dynasty, rv$team_power, rv$draft, rv$trades, role)
    }, error = function(e) rv$gm_answer <- paste("GM Assistant error:", fm3_clean_error(e)))
  })

  observeEvent(input$refresh_health, { rv$health_tick <- rv$health_tick + 1 })

  output$connection_status <- renderUI({
    div(class = "gm-status", rv$status)
  })

  my_power <- reactive({
    req(!is.null(rv$state), !is.null(rv$team_power))
    rv$team_power |> dplyr::filter(roster_id == rv$state$user_roster_id)
  })

  output$strategy_card <- renderUI({
    x <- my_power(); req(nrow(x))
    metric_card("Team strategy", x$strategy[1], fm3_team_strategy_text(x$strategy[1]), TRUE)
  })
  output$contender_card <- renderUI({
    x <- my_power(); req(nrow(x))
    rank <- match(x$roster_id[1], rv$team_power$roster_id)
    metric_card("Contender index", paste0(round(x$contender_index[1]), "/100"), paste0("League power rank #", rank, " · title equity proxy ", fm3_pct(x$title_equity_proxy[1])))
  })
  output$age_card <- renderUI({
    x <- my_power(); req(nrow(x))
    metric_card("Core age", sprintf("%.1f", x$weighted_core_age[1]), paste0("Dynasty percentile ", fm3_pct(x$dynasty_percentile[1])))
  })
  output$picks_card <- renderUI({
    x <- my_power(); req(nrow(x))
    metric_card("Future 1sts", x$future_first_count[1], paste0(x$future_pick_count[1], " future picks tracked"))
  })

  output$my_roster_table <- renderTable({
    req(!is.null(rv$state), !is.null(rv$dynasty))
    d <- (rv$roster_values %||% fm3_attach_roster_values(rv$state, rv$dynasty)) |>
      dplyr::filter(roster_id == rv$state$user_roster_id) |>
      dplyr::arrange(dplyr::desc(model_dynasty_value)) |>
      dplyr::transmute(
        Player = player_display_name, Pos = position, Age = round(age, 1),
        Week = ifelse(is.finite(week), as.integer(week), NA_integer_),
        Opp = dplyr::if_else(nzchar(opponent), opponent, "—"),
        `2.4.3 Week Pts` = ifelse(is.finite(projected_weekly_fppg_24), round(projected_weekly_fppg_24, 1), NA_real_),
        `Floor-Ceiling` = ifelse(is.finite(weekly_floor) & is.finite(weekly_ceiling), paste0(round(weekly_floor,1), "-", round(weekly_ceiling,1)), "—"),
        `Expected Error` = ifelse(is.finite(expected_abs_error), paste0("±", round(expected_abs_error,1)), "—"),
        `2026 FPPG` = round(year1_fppg, 1), `Year 3 FPPG` = round(year3_fppg, 1),
        `Model Value` = round(model_dynasty_value),
        `External Market` = ifelse(is.finite(market_value_proxy), round(market_value_proxy), NA_real_),
        Signal = value_signal,
        Injury = dplyr::if_else(nzchar(injury_status), injury_status, "—")
      )
    utils::head(d, 24)
  }, striped = TRUE, hover = TRUE, spacing = "s")

  output$team_needs_table <- renderTable({
    req(!is.null(rv$state), !is.null(rv$team_power))
    fm3_team_needs(rv$team_power, rv$state$user_roster_id) |>
      dplyr::transmute(Position = position, `League Percentile` = paste0(round(100 * league_percentile), "%"), `Need Score` = round(100 * need_score))
  }, striped = TRUE, spacing = "s")

  output$league_power_table <- renderTable({
    req(!is.null(rv$team_power))
    rv$team_power |>
      dplyr::transmute(Team = team_name, Strategy = strategy, `Contender Index` = round(contender_index), `Starter FPPG` = round(starter_fppg, 1), `Core Age` = round(weighted_core_age, 1)) |>
      utils::head(12)
  }, striped = TRUE, spacing = "s")

  output$draft_summary <- renderUI({
    if (is.null(rv$draft)) return(div(class = "gm-status", "Draft data is not loaded yet. Tap Refresh Draft when you want live draft recommendations."))
    if (!is.null(rv$draft$error)) return(div(class = "gm-warning", rv$draft$error))
    owner_name <- rv$team_power$team_name[match(rv$draft$current_owner_roster_id, rv$team_power$roster_id)] %||% "Unknown"
    fluidRow(
      column(3, metric_card("Current pick", rv$draft$current_pick, if (rv$draft$user_on_clock) "YOU ARE ON THE CLOCK" else paste("Owned by", owner_name), rv$draft$user_on_clock)),
      column(3, metric_card("Your next pick", ifelse(is.finite(rv$draft$next_user_pick), rv$draft$next_user_pick, "—"), if (rv$draft$rookie_draft) "Rookie draft inferred" else "Startup / full-player pool")),
      column(3, metric_card("Team window", rv$draft$strategy, "Draft scoring changes with your competitive window")),
      column(3, metric_card("Players selected", nrow(rv$draft$picks), paste0(nrow(rv$draft$available_pool), " model-matched players available")))
    )
  })

  output$draft_recommendations <- renderTable({
    if (is.null(rv$draft)) return(data.frame(Status = "Tap Refresh Draft to load current selections and recommendations."))
    if (!is.null(rv$draft$error) || is.null(rv$draft$recommendations)) return(data.frame(Status = rv$draft$error %||% "Draft recommendations unavailable."))
    rv$draft$recommendations |>
      dplyr::transmute(
        Rank = recommendation_rank, Player = player_display_name, Pos = position,
        Score = round(draft_score, 1), `Model Value` = round(model_dynasty_value),
        Need = round(100 * need_score), Scarcity = round(100 * scarcity_score),
        `Survives to Next` = ifelse(is.finite(rv$draft$next_user_pick), paste0(round(100 * survival_to_next_pick), "%"), "N/A"),
        Reason = recommendation_reason
      )
  }, striped = TRUE, hover = TRUE, spacing = "s")

  output$recent_draft_picks <- renderTable({
    if (is.null(rv$draft)) return(data.frame(Status = "Draft not refreshed yet."))
    if (!is.null(rv$draft$error)) return(data.frame(Status = rv$draft$error))
    p <- rv$draft$picks
    if (!nrow(p)) return(data.frame(Status = "No picks yet"))
    utils::tail(p, 12) |>
      dplyr::transmute(Pick = pick_no, Player = player_name, Pos = position, Team = team, `Roster ID` = roster_id)
  }, striped = TRUE, spacing = "s")

  output$trade_down_table <- renderTable({
    if (is.null(rv$draft)) return(data.frame(Status = "Refresh Draft to calculate trade-down opportunities."))
    if (is.null(rv$trade_down) || !nrow(rv$trade_down)) return(data.frame(Status = "No trade-down framework resolved for the current pick."))
    rv$trade_down |>
      dplyr::transmute(`Partner Roster` = owner_roster_id, `Later Pick` = pick_no, `Estimated Value Gap` = round(value_gap), `Suggested Ask` = suggested_extra)
  }, striped = TRUE, spacing = "s")

  output$trade_ideas_table <- renderTable({
    if (is.null(rv$trades)) return(data.frame(Status = "Tap Refresh Trade Ideas to search the league. No trade search runs in the background."))
    if (!nrow(rv$trades)) return(data.frame(Status = "No mutually strong one-for-one frameworks found. Try a custom package."))
    rv$trades |>
      dplyr::transmute(
        Package = package_text, Partner = partner_name, Fit = fit_score, `Your Utility` = round(your_utility, 2),
        `Their Utility` = round(their_utility, 2), Balance = balance_hint
      ) |>
      utils::head(20)
  }, striped = TRUE, hover = TRUE, spacing = "s")

  output$trade_evaluation <- renderUI({
    x <- trade_eval(); req(!is.null(x))
    fluidRow(
      column(3, metric_card("Verdict", x$verdict, paste("Strategy:", x$strategy), TRUE)),
      column(3, metric_card("Value delta", sprintf("%+.0f", x$dynasty_delta), paste0("Receive ", round(x$receive_value), " vs send ", round(x$send_value)))),
      column(3, metric_card("Production delta", sprintf("%+.1f", x$production_delta), "Combined Year-1 FPPG in the selected player assets")),
      column(3, metric_card("Mutual fit", paste0(round(x$mutual_fit_score), "/100"), paste0("You ", round(x$strategy_utility, 2), " · partner ", ifelse(is.finite(x$partner_strategy_utility), round(x$partner_strategy_utility, 2), "—"))))
    )
  })

  output$ai_status <- renderText({ if (fm3_ai_available()) paste0(" · AI enabled (", fm3_ai_model(), ")") else " · deterministic mode — OPENAI_API_KEY not set" })
  output$gm_answer <- renderText({ rv$gm_answer })

  output$role_roster_table <- renderTable({
    req(!is.null(rv$state), !is.null(rv$dynasty))
    role <- role_forecasts()
    if (!nrow(role)) return(data.frame(Status = "Run source(\"runners/RUN_3_0_DATA_LAB.R\") to build role forecasts."))
    rp <- (rv$roster_values %||% fm3_attach_roster_values(rv$state, rv$dynasty)) |> dplyr::filter(roster_id == rv$state$user_roster_id)
    ids <- unique(rp$gsis_id[nzchar(rp$gsis_id)])
    role |> dplyr::filter(player_id %in% ids) |>
      dplyr::arrange(dplyr::desc(projected_weekly_fppg_24)) |>
      dplyr::transmute(
        Player = player_display_name, Pos = position, Opp = opponent,
        `2.4.3 Pts` = round(projected_weekly_fppg_24, 1),
        Targets = round(projected_targets_30, 1), Carries = round(projected_carries_30, 1),
        `Snap Share` = paste0(round(100 * projected_snap_share_30), "%"),
        `Regime Prob` = paste0(round(100 * role_regime_probability_30), "%"),
        `Role Uncertainty` = paste0(round(100 * role_uncertainty_30), "%"),
        Confidence = projection_confidence
      )
  }, striped = TRUE, hover = TRUE, spacing = "s")

  output$role_validation_table <- renderTable({
    d <- fm3_role_health()
    if (!nrow(d)) return(data.frame(Status = "Role validation has not been run."))
    d |> dplyr::transmute(Position = position, Target = target, N = n, `Baseline MAE` = round(baseline_MAE, 3), `Model MAE` = round(model_MAE, 3), `MAE Gain %` = mae_gain_pct_display, Status = status)
  }, striped = TRUE, spacing = "s")

  output$data_health_table <- renderTable({
    rv$health_tick
    lid <- if (!is.null(rv$state)) rv$state$league_id else default_league_id
    fm3_data_health(lid)
  }, striped = TRUE, spacing = "s")

  output$identity_coverage <- renderUI({
    if (is.null(rv$state) || is.null(rv$dynasty)) return(div(class = "gm-muted", "Connect a Sleeper league to calculate identity coverage."))
    rostered <- rv$state$roster_players |> dplyr::filter(position %in% c("QB","RB","WR","TE"))
    matched <- rv$dynasty |> dplyr::filter(!is.na(sleeper_id), sleeper_id %in% rostered$sleeper_id)
    n_all <- length(unique(rostered$sleeper_id)); n_match <- length(unique(matched$sleeper_id))
    metric_card("Rostered skill-player match rate", if (n_all > 0) paste0(round(100 * n_match / n_all, 1), "%") else "—", paste0(n_match, " of ", n_all, " rostered QB/RB/WR/TE players linked to the model"), n_all > 0 && n_match / n_all >= 0.95)
  })

  output$setup_status <- renderText({
    paste0(rv$status,
           "\nApp mode: model-first / lightweight Sleeper state",
           "\nSleeper usage: league + rosters + player IDs + drafts/picks only",
           "\nSleeper projections/rankings: DISABLED",
           "\nAI: ", if (fm3_ai_available()) paste0("enabled with ", fm3_ai_model()) else "not configured",
           "\nProjection engine: ", PROJECTION_ENGINE_VERSION,
           "\nDynasty engine: ", DYNASTY_ENGINE_VERSION,
           "\nRole engine: ", ROLE_ENGINE_VERSION)
  })
}

shinyApp(ui, server)
