# ============================================================
# FANTASY MODEL 2.2 - SEASON + WEEKLY DECISION APPLICATION
# ============================================================
source("config.R")
ensure_packages(c("shiny", "dplyr", "readr"))

library(shiny)
library(dplyr)
library(readr)
source("R/league_engine.R")

rank_path <- paste0("output/final_", CURRENT_SEASON, "_rankings.csv")
dynasty_path <- "output/final_dynasty_rankings.csv"
comp_path <- paste0("output/player_comps_", CURRENT_SEASON, ".csv")
quality_path <- "output/model_quality_report.txt"
validation_path <- "output/validation_metrics.csv"
probability_path <- "output/validation_probability_metrics.csv"
version_compare_path <- "output/model_version_comparison.csv"
weekly_path <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
weekly_quality_path <- "output/weekly_model_quality_report.txt"
weekly_validation_path <- "output/weekly_validation_metrics.csv"
weekly_ros_path <- paste0("output/rest_of_season_", CURRENT_SEASON, ".csv")
weekly_arch_path <- "output/weekly_2_2_architecture_comparison.csv"
weekly_cohort_path <- "output/weekly_2_2_cohort_metrics.csv"
weekly_matchup_path <- "output/weekly_2_2_matchup_calibration.csv"
weekly_stack_path <- "output/weekly_2_2_selected_stack.csv"
weekly_residual_path <- "output/weekly_2_2_residual_pool.csv"

required_paths <- c(rank_path, dynasty_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop("Run source(\"MOBILE_RUN.R\") before the dashboard. Missing: ", paste(missing_paths, collapse = ", "))
}

rankings_base <- readr::read_csv(rank_path, show_col_types = FALSE)
dynasty_base <- readr::read_csv(dynasty_path, show_col_types = FALSE)
comps_base <- if (file.exists(comp_path)) readr::read_csv(comp_path, show_col_types = FALSE) else data.frame()
validation_base <- if (file.exists(validation_path)) readr::read_csv(validation_path, show_col_types = FALSE) else data.frame()
probability_base <- if (file.exists(probability_path)) readr::read_csv(probability_path, show_col_types = FALSE) else data.frame()
version_compare <- if (file.exists(version_compare_path)) readr::read_csv(version_compare_path, show_col_types = FALSE) else data.frame()
quality_report <- if (file.exists(quality_path)) readLines(quality_path, warn = FALSE) else "Model quality report not found."
weekly_base <- if (file.exists(weekly_path)) readr::read_csv(weekly_path, show_col_types = FALSE) else data.frame()
weekly_validation_base <- if (file.exists(weekly_validation_path)) readr::read_csv(weekly_validation_path, show_col_types = FALSE) else data.frame()
weekly_ros_base <- if (file.exists(weekly_ros_path)) readr::read_csv(weekly_ros_path, show_col_types = FALSE) else data.frame()
weekly_arch_base <- if (file.exists(weekly_arch_path)) readr::read_csv(weekly_arch_path, show_col_types = FALSE) else data.frame()
weekly_cohort_base <- if (file.exists(weekly_cohort_path)) readr::read_csv(weekly_cohort_path, show_col_types = FALSE) else data.frame()
weekly_matchup_base <- if (file.exists(weekly_matchup_path)) readr::read_csv(weekly_matchup_path, show_col_types = FALSE) else data.frame()
weekly_stack_base <- if (file.exists(weekly_stack_path)) readr::read_csv(weekly_stack_path, show_col_types = FALSE) else data.frame()
weekly_residual_base <- if (file.exists(weekly_residual_path)) readr::read_csv(weekly_residual_path, show_col_types = FALSE) else data.frame()
weekly_quality_report <- if (file.exists(weekly_quality_path)) readLines(weekly_quality_path, warn = FALSE) else "Run source(\"MOBILE_RUN_WEEKLY_FAST.R\") to generate 2.2 weekly validation and projections."
current_week_value <- if (file.exists("output/current_week.txt")) suppressWarnings(as.integer(readLines("output/current_week.txt", warn = FALSE)[1])) else 1L
if (!is.finite(current_week_value)) current_week_value <- 1L
weekly_refresh_label <- if (file.exists(weekly_path)) format(file.info(weekly_path)$mtime, "%b %d %I:%M %p") else "not generated"
profiles_initial <- read_league_profiles()
initial_profile_name <- as.character(profiles_initial$league_name[1])
initial_settings <- read_league_profile(initial_profile_name)

pct <- function(x, digits = 0) {
  ifelse(is.finite(x), paste0(format(round(100 * x, digits), nsmall = digits), "%"), "—")
}
num1 <- function(x) ifelse(is.finite(x), sprintf("%.1f", x), "—")
num2 <- function(x) ifelse(is.finite(x), sprintf("%.2f", x), "—")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
field <- function(d, nm, default = 0) {
  if (!nm %in% names(d) || length(d[[nm]]) == 0) return(default)
  x <- d[[nm]][1]
  if (is.numeric(default)) {
    x <- suppressWarnings(as.numeric(x))
    if (!is.finite(x)) default else x
  } else {
    if (is.na(x) || !nzchar(as.character(x))) default else as.character(x)
  }
}

vcol <- function(d, nm, default = 0) {
  n <- nrow(d)
  def <- if (length(default) == n) as.numeric(default) else rep(as.numeric(default)[1], n)
  if (!nm %in% names(d)) return(def)
  x <- suppressWarnings(as.numeric(d[[nm]]))
  bad <- !is.finite(x)
  if (any(bad)) x[bad] <- def[bad]
  x
}

kpi <- function(label, value, detail = NULL, accent = FALSE) {
  div(
    class = "fm-kpi",
    div(class = "label", label),
    div(class = if (accent) "value fm-accent" else "value", value),
    if (!is.null(detail)) div(class = "fm-muted", detail)
  )
}

route_name <- function(d, prefix) {
  choices <- c(
    Vertical = field(d, paste0(prefix, "vertical"), 0),
    `In-breaking` = field(d, paste0(prefix, "in_break"), 0),
    `Out-breaking` = field(d, paste0(prefix, "out_break"), 0),
    Screen = field(d, paste0(prefix, "screen"), 0),
    Hitch = field(d, paste0(prefix, "hitch"), 0)
  )
  if (max(choices, na.rm = TRUE) <= 0) return("No route chart")
  names(choices)[which.max(choices)]
}

player_mobile_card <- function(d) {
  div(
    class = "fm-player-row",
    div(class = "fm-rank-badge", paste0("#", d$league_overall_rank)),
    div(class = "fm-player-main",
        div(class = "fm-player-name", d$player_display_name),
        div(class = "fm-player-meta", paste0(d$position, d$league_position_rank, " · ", d$current_team, " · ", d$fantasy_outlook))),
    div(class = "fm-player-score",
        strong(num1(d$league_projected_fppg)),
        span(" FPPG"),
        tags$small(paste0("Elite ", pct(d$elite_probability))))
  )
}

weekly_mobile_card <- function(d) {
  delta <- field(d, "matchup_delta_22", field(d, "matchup_delta_vs_prior", 0))
  grade <- field(d, "matchup_grade", "Neutral")
  boom <- field(d, "boom_probability", NA_real_)
  topn <- field(d, "topN_probability", NA_real_)
  proj <- field(d, "league_projected_weekly_fppg", field(d, "projected_weekly_fppg", 0))
  div(
    class = "fm-player-row",
    div(class = "fm-rank-badge", paste0("#", d$weekly_position_rank)),
    div(class = "fm-player-main",
        div(class = "fm-player-name", d$player_display_name),
        div(class = "fm-player-meta", paste0(d$position, " · ", d$team, " vs ", d$opponent, " · Matchup ", grade))),
    div(class = "fm-player-score",
        strong(num1(proj)),
        span(" pts"),
        tags$small(paste0("Top tier ", pct(topn), " · Boom ", pct(boom))))
  )
}

ui <- navbarPage(
  title = paste0("Fantasy Model ", PROJECT_VERSION),
  id = "main_nav",
  selected = "Weekly",
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1, viewport-fit=cover"),
    tags$meta(name = "theme-color", content = "#0b1220"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  tabPanel(
    "Home",
    div(class = "fm-hero",
        h2("Fantasy Model 2.2"),
        p(class = "fm-muted", "Validated season prior + decomposed weekly role/opportunity/matchup stack + probability-driven decisions.")),
    fluidRow(
      column(3, uiOutput("home_top_player")),
      column(3, uiOutput("home_upside_player")),
      column(3, uiOutput("home_dynasty_player")),
      column(3, uiOutput("home_league"))
    ),
    fluidRow(
      column(7, div(class = "fm-card",
                    div(class = "fm-section-head", h4("Top 10 for this league"), span(class = "fm-pill", "Live league value")),
                    div(class = "fm-desktop-only fm-table-wrap", tableOutput("home_top10")),
                    div(class = "fm-mobile-only", uiOutput("home_top10_cards")))),
      column(5, div(class = "fm-card",
                    h4("What changed in 2.2"),
                    tags$ul(
                      tags$li("Separates neutral player role from the weekly matchup adjustment so recent FPPG can no longer drown out defense signal."),
                      tags$li("Projects opportunity first (attempts/carries/targets), then reconstructs a shrunk weekly stat line."),
                      tags$li("Adds live lagged Next Gen Stats when available for QB, receiver and rushing traits."),
                      tags$li("Uses a dedicated opponent/PBP matchup-delta learner trained on cross-fitted residuals."),
                      tags$li("Stacks the 2.0 prior, neutral model, structured+matchup model and full direct challenger with chronological guardrails."),
                      tags$li("Adds starter-cohort validation plus empirical floor/ceiling, boom/bust and Top-N probabilities." )
                    ),
                    p(class = "fm-muted", "The app now emphasizes weekly decisions: projection distribution, role trend, matchup grade, league-adjusted scoring and start/sit comparison.")))
    )
  ),

  tabPanel(
    "Rankings",
    div(class = "fm-hero", h2("League Rankings"), p(class = "fm-muted", "League-adjusted redraft value with projection, upside and context.")),
    fluidRow(
      column(3, selectInput("rank_position", "Position", c("ALL", "QB", "RB", "WR", "TE"))),
      column(3, selectInput("rank_team", "Team", c("ALL", sort(unique(stats::na.omit(rankings_base$current_team)))))),
      column(3, selectInput("rank_sort", "Sort", c("League Rank", "Projected FPPG", "Upside", "Elite Probability", "Breakout Probability"))),
      column(3, selectInput("rank_count", "Show", c("25", "50", "100", "All"), selected = "50"))
    ),
    fluidRow(column(12, textInput("rank_search", "Player search", placeholder = "Type a player name..."))),
    div(class = "fm-card fm-desktop-only", div(class = "fm-table-wrap", tableOutput("redraft_table"))),
    div(class = "fm-mobile-only", uiOutput("redraft_cards"))
  ),

  tabPanel(
    "Weekly",
    div(class = "fm-hero", h2("Weekly Projections"), p(class = "fm-muted", "2.2 separates neutral role, expected opportunity, opponent matchup and direct-model signal before stacking the final forecast.")),
    fluidRow(
      column(2, selectInput("weekly_week", "Week", choices = as.character(1:18), selected = as.character(current_week_value))),
      column(2, selectInput("weekly_position", "Position", c("ALL", "QB", "RB", "WR", "TE"))),
      column(3, selectInput("weekly_sort", "Sort", c("Projection", "Floor", "Ceiling", "Top-N Probability", "Boom Probability", "Matchup Boost"), selected = "Projection")),
      column(5, textInput("weekly_search", "Player search", placeholder = "Type a player name..."))
    ),
    uiOutput("weekly_status"),
    fluidRow(
      column(3, uiOutput("weekly_top_player")),
      column(3, uiOutput("weekly_best_matchup")),
      column(3, uiOutput("weekly_safest_player")),
      column(3, uiOutput("weekly_week_summary"))
    ),
    div(class = "fm-card",
        div(class = "fm-section-head", h4("Week rankings"), span(class = "fm-pill", "League-adjusted projection")),
        p(class = "fm-muted", "Floor/ceiling and probabilities come from the historical out-of-sample residual distribution. Matchup grade is the explicit 2.2 opponent adjustment."),
        div(class = "fm-desktop-only fm-table-wrap", tableOutput("weekly_table")),
        div(class = "fm-mobile-only", uiOutput("weekly_cards")))
  ),

  tabPanel(
    "Start/Sit",
    div(class = "fm-hero", h2("Start / Sit"), p(class = "fm-muted", "Compare up to four players using league-adjusted projection, range, Top-N probability, matchup and role trend.")),
    fluidRow(
      column(3, selectInput("compare_week", "Week", choices = as.character(1:18), selected = as.character(current_week_value))),
      column(9, selectizeInput("compare_players", "Players", choices = sort(unique(weekly_base$player_display_name)), multiple = TRUE,
                               options = list(maxItems = 4, placeholder = "Select 2-4 players...")))
    ),
    fluidRow(
      column(4, uiOutput("compare_pick")),
      column(4, uiOutput("compare_confidence")),
      column(4, uiOutput("compare_risk"))
    ),
    div(class = "fm-card", h4("Side-by-side decision"), div(class = "fm-table-wrap", tableOutput("compare_table"))),
    div(class = "fm-card", h4("Why the model prefers its pick"), uiOutput("compare_reason"))
  ),

  tabPanel(
    "ROS",
    div(class = "fm-hero", h2("Rest of Season"), p(class = "fm-muted", "Sum the remaining 2.2 matchup-specific weekly forecasts instead of multiplying one generic FPPG by games left.")),
    fluidRow(
      column(3, selectInput("ros_position", "Position", c("ALL", "QB", "RB", "WR", "TE"))),
      column(3, selectInput("ros_sort", "Sort", c("ROS Points", "ROS FPPG", "Floor", "Ceiling", "Favorable Games"), selected = "ROS Points")),
      column(6, textInput("ros_search", "Player search", placeholder = "Type a player name..."))
    ),
    fluidRow(
      column(4, uiOutput("ros_top_player")),
      column(4, uiOutput("ros_best_schedule")),
      column(4, uiOutput("ros_games_left"))
    ),
    div(class = "fm-card",
        div(class = "fm-section-head", h4("Remaining schedule value"), span(class = "fm-pill", "League-adjusted future scoring")),
        p(class = "fm-muted", "ROS points are the sum of each remaining weekly projection. Floor/ceiling are summed weekly empirical ranges; favorable games come from the explicit 2.2 matchup-delta model."),
        div(class = "fm-table-wrap", tableOutput("ros_table")))
  ),

  tabPanel(
    "Player",
    div(class = "fm-hero", h2("Player Detail"), p(class = "fm-muted", "Projection, range, probabilities, team fit and historical comparables.")),
    fluidRow(column(12, selectizeInput(
      "player_select", "Player",
      choices = sort(unique(rankings_base$player_display_name)),
      selected = rankings_base$player_display_name[1],
      options = list(placeholder = "Search player...", maxOptions = 700)
    ))),
    uiOutput("player_header"),
    fluidRow(
      column(3, uiOutput("player_projection")),
      column(3, uiOutput("player_elite")),
      column(3, uiOutput("player_breakout")),
      column(3, uiOutput("player_value"))
    ),
    fluidRow(
      column(3, uiOutput("player_week_projection")),
      column(3, uiOutput("player_week_matchup")),
      column(3, uiOutput("player_week_topn")),
      column(3, uiOutput("player_week_boom"))
    ),
    fluidRow(
      column(6, div(class = "fm-card", h4("Current weekly role + matchup"), uiOutput("player_week_context"))),
      column(6, div(class = "fm-card", h4("2.2 weekly stat line"), tableOutput("player_week_statline")))
    ),
    div(class = "fm-card", h4("Season projection range"), uiOutput("player_range_bar")),
    div(class = "fm-card", h4("2.0 projected stat line"), tableOutput("player_component_table")),
    fluidRow(
      column(6, div(class = "fm-card", h4("Team + QB environment"), uiOutput("player_team_context"))),
      column(6, div(class = "fm-card", h4("Player usage profile"), uiOutput("player_receiver_context")))
    ),
    fluidRow(
      column(6, div(class = "fm-card", h4("Projection details"), tableOutput("player_projection_table"))),
      column(6, div(class = "fm-card", h4("Historical comps"), tableOutput("player_comps_table")))
    )
  ),

  tabPanel(
    "Dynasty",
    div(class = "fm-hero", h2("Dynasty Rankings"), p(class = "fm-muted", "Current multi-year dynasty value translated to your scoring format.")),
    fluidRow(
      column(4, selectInput("dyn_position", "Position", c("ALL", "QB", "RB", "WR", "TE"))),
      column(4, textInput("dyn_search", "Player search", placeholder = "Type a player name...")),
      column(4, selectInput("dyn_count", "Show", c("25", "50", "100", "All"), selected = "50"))
    ),
    div(class = "fm-card", div(class = "fm-table-wrap", tableOutput("dynasty_table")))
  ),

  tabPanel(
    "League",
    div(class = "fm-hero", h2("League Settings"), p(class = "fm-muted", "Save multiple leagues and switch between them without rebuilding the model.")),
    fluidRow(
      column(12, div(class = "fm-card fm-profile-bar",
        selectInput("league_profile", "Saved league", choices = sort(unique(profiles_initial$league_name)), selected = initial_profile_name),
        actionButton("load_profile", "Load", class = "btn-default"),
        actionButton("delete_profile", "Delete", class = "btn-default")
      ))
    ),
    fluidRow(
      column(6, div(class = "fm-card",
        h4("Format"),
        selectInput("league_preset", "Quick preset", c("Custom", "12-team 1QB Half-PPR", "12-team Superflex PPR", "10-team 1QB PPR"), selected = "Custom"),
        textInput("league_name", "League name", value = initial_settings$league_name),
        numericInput("teams", "Teams", min = 8, max = 16, step = 1, value = initial_settings$teams),
        selectInput("scoring_preset", "Reception scoring", c("Standard", "Half PPR", "PPR"), selected = initial_settings$scoring_preset),
        selectInput("pass_td_points", "Passing TD points", c("4" = 4, "6" = 6), selected = as.character(initial_settings$pass_td_points)),
        sliderInput("te_premium", "TE reception premium", min = 0, max = 1.5, step = 0.25, value = initial_settings$te_premium)
      )),
      column(6, div(class = "fm-card",
        h4("Starting lineup"),
        numericInput("qb_starters", "QB", min = 0, max = 2, step = 1, value = initial_settings$qb_starters),
        numericInput("rb_starters", "RB", min = 1, max = 4, step = 1, value = initial_settings$rb_starters),
        numericInput("wr_starters", "WR", min = 1, max = 5, step = 1, value = initial_settings$wr_starters),
        numericInput("te_starters", "TE", min = 0, max = 2, step = 1, value = initial_settings$te_starters),
        numericInput("flex_starters", "FLEX (RB/WR/TE)", min = 0, max = 3, step = 1, value = initial_settings$flex_starters),
        numericInput("superflex_starters", "SUPERFLEX", min = 0, max = 2, step = 1, value = initial_settings$superflex_starters)
      ))
    ),
    fluidRow(
      column(6, div(class = "fm-card", h4("Estimated replacement levels"), tableOutput("replacement_table"))),
      column(6, div(class = "fm-card", h4("Save profile"),
                    actionButton("save_settings", "Save league", class = "btn-primary"),
                    br(), br(), uiOutput("save_status"),
                    p(class = "fm-muted", "Saving a new league name creates a new profile. Saving an existing name updates it.")))
    )
  ),

  tabPanel(
    "Quality",
    div(class = "fm-hero", h2("Model Quality"), p(class = "fm-muted", "Lead with the decisions: honest weekly accuracy, starter cohorts, architecture ablation and matchup calibration. Raw reports are available below for debugging.")),
    fluidRow(column(12, div(class = "fm-card", h4("2.2 weekly walk-forward metrics"), div(class = "fm-table-wrap", tableOutput("weekly_validation_table"))))),
    fluidRow(
      column(6, div(class = "fm-card", h4("Fantasy-relevant cohorts"), div(class = "fm-table-wrap", tableOutput("weekly_cohort_table")))),
      column(6, div(class = "fm-card", h4("Selected 2026 stack"), div(class = "fm-table-wrap", tableOutput("weekly_stack_table"))))
    ),
    fluidRow(
      column(6, div(class = "fm-card", h4("Architecture ablation"), div(class = "fm-table-wrap", tableOutput("weekly_arch_table")))),
      column(6, div(class = "fm-card", h4("Matchup calibration"), div(class = "fm-table-wrap", tableOutput("weekly_matchup_table"))))
    ),
    fluidRow(
      column(6, div(class = "fm-card", h4("2.0 season regression"), div(class = "fm-table-wrap", tableOutput("validation_table")))),
      column(6, div(class = "fm-card", h4("2.0 decision models"), div(class = "fm-table-wrap", tableOutput("probability_table"))))
    ),
    fluidRow(column(12, div(class = "fm-card", h4("2.0 vs 1.2"), div(class = "fm-table-wrap", tableOutput("version_comparison_table"))))),
    fluidRow(column(12, div(class = "fm-card fm-technical",
      tags$details(
        tags$summary("Technical reports (tap to expand)"),
        h4("2.2 weekly quality report"), verbatimTextOutput("weekly_quality_report"),
        h4("2.0 season quality report"), verbatimTextOutput("quality_report")
      )
    )))
  )
)

server <- function(input, output, session) {
  save_message <- reactiveVal("")

  settings <- reactive({
    val <- function(x, default) {
      if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) default else x
    }
    preset <- val(input$scoring_preset, initial_settings$scoring_preset)
    list(
      league_name = val(input$league_name, initial_settings$league_name),
      teams = as.numeric(val(input$teams, initial_settings$teams)),
      scoring_preset = preset,
      reception_points = apply_scoring_preset(preset),
      pass_td_points = as.numeric(val(input$pass_td_points, initial_settings$pass_td_points)),
      te_premium = as.numeric(val(input$te_premium, initial_settings$te_premium)),
      qb_starters = as.numeric(val(input$qb_starters, initial_settings$qb_starters)),
      rb_starters = as.numeric(val(input$rb_starters, initial_settings$rb_starters)),
      wr_starters = as.numeric(val(input$wr_starters, initial_settings$wr_starters)),
      te_starters = as.numeric(val(input$te_starters, initial_settings$te_starters)),
      flex_starters = as.numeric(val(input$flex_starters, initial_settings$flex_starters)),
      superflex_starters = as.numeric(val(input$superflex_starters, initial_settings$superflex_starters))
    )
  })

  apply_profile_to_inputs <- function(s) {
    updateTextInput(session, "league_name", value = s$league_name)
    updateNumericInput(session, "teams", value = s$teams)
    updateSelectInput(session, "scoring_preset", selected = s$scoring_preset)
    updateSelectInput(session, "pass_td_points", selected = as.character(s$pass_td_points))
    updateSliderInput(session, "te_premium", value = s$te_premium)
    updateNumericInput(session, "qb_starters", value = s$qb_starters)
    updateNumericInput(session, "rb_starters", value = s$rb_starters)
    updateNumericInput(session, "wr_starters", value = s$wr_starters)
    updateNumericInput(session, "te_starters", value = s$te_starters)
    updateNumericInput(session, "flex_starters", value = s$flex_starters)
    updateNumericInput(session, "superflex_starters", value = s$superflex_starters)
  }

  observeEvent(input$league_preset, {
    req(input$league_preset)
    if (input$league_preset == "12-team 1QB Half-PPR") {
      updateNumericInput(session, "teams", value = 12); updateSelectInput(session, "scoring_preset", selected = "Half PPR")
      updateNumericInput(session, "qb_starters", value = 1); updateNumericInput(session, "rb_starters", value = 2)
      updateNumericInput(session, "wr_starters", value = 2); updateNumericInput(session, "te_starters", value = 1)
      updateNumericInput(session, "flex_starters", value = 1); updateNumericInput(session, "superflex_starters", value = 0)
    } else if (input$league_preset == "12-team Superflex PPR") {
      updateNumericInput(session, "teams", value = 12); updateSelectInput(session, "scoring_preset", selected = "PPR")
      updateNumericInput(session, "qb_starters", value = 1); updateNumericInput(session, "rb_starters", value = 2)
      updateNumericInput(session, "wr_starters", value = 2); updateNumericInput(session, "te_starters", value = 1)
      updateNumericInput(session, "flex_starters", value = 2); updateNumericInput(session, "superflex_starters", value = 1)
    } else if (input$league_preset == "10-team 1QB PPR") {
      updateNumericInput(session, "teams", value = 10); updateSelectInput(session, "scoring_preset", selected = "PPR")
      updateNumericInput(session, "qb_starters", value = 1); updateNumericInput(session, "rb_starters", value = 2)
      updateNumericInput(session, "wr_starters", value = 3); updateNumericInput(session, "te_starters", value = 1)
      updateNumericInput(session, "flex_starters", value = 1); updateNumericInput(session, "superflex_starters", value = 0)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$load_profile, {
    req(input$league_profile)
    s <- read_league_profile(input$league_profile)
    apply_profile_to_inputs(s)
    save_message(paste0("Loaded ", s$league_name, "."))
  })

  observeEvent(input$save_settings, {
    s <- settings()
    if (!nzchar(trimws(s$league_name))) s$league_name <- "My League"
    write_league_profile(s)
    profiles <- read_league_profiles()
    updateSelectInput(session, "league_profile", choices = sort(unique(profiles$league_name)), selected = s$league_name)
    save_message(paste0("Saved ", s$league_name, "."))
  })

  observeEvent(input$delete_profile, {
    req(input$league_profile)
    ok <- delete_league_profile(input$league_profile)
    profiles <- read_league_profiles()
    selected <- as.character(profiles$league_name[1])
    updateSelectInput(session, "league_profile", choices = sort(unique(profiles$league_name)), selected = selected)
    if (ok) {
      apply_profile_to_inputs(read_league_profile(selected))
      save_message("League deleted.")
    } else save_message("Keep at least one saved league.")
  })

  league_rankings <- reactive({
    req(input$teams, input$scoring_preset, input$pass_td_points)
    rank_for_league(rankings_base, settings())
  })
  league_dynasty <- reactive({ adjust_dynasty_for_league(dynasty_base, league_rankings()) })

  output$save_status <- renderUI({
    if (nzchar(save_message())) div(class = "fm-accent", strong(save_message())) else div(class = "fm-muted", "League profiles persist inside this project.")
  })
  output$replacement_table <- renderTable({
    r <- league_replacement_ranks(settings())
    data.frame(Position = names(r), `Replacement Rank` = as.integer(r), check.names = FALSE)
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$home_top_player <- renderUI({
    d <- league_rankings()[1, ]; kpi("#1 Redraft", d$player_display_name, paste0(d$position, d$league_position_rank, " · ", num1(d$league_projected_fppg), " FPPG"), TRUE)
  })
  output$home_upside_player <- renderUI({
    d <- league_rankings() |> arrange(desc(upside_index)) |> slice(1); kpi("Highest Upside", d$player_display_name, paste0(round(d$upside_index), " upside index"))
  })
  output$home_dynasty_player <- renderUI({
    d <- league_dynasty() |> slice(1); kpi("#1 Dynasty", d$player_display_name, paste0(d$position, " · value ", round(d$league_dynasty_value)))
  })
  output$home_league <- renderUI({
    s <- settings(); format <- paste0(s$teams, " teams · ", s$scoring_preset)
    if (s$te_premium > 0) format <- paste0(format, " · TE+", s$te_premium)
    if (s$superflex_starters > 0) format <- paste0(format, " · SF")
    kpi("Current League", s$league_name, format)
  })

  output$home_top10 <- renderTable({
    league_rankings() |> slice_head(n = 10) |> transmute(
      Rank = league_overall_rank, Player = player_display_name, Pos = paste0(position, league_position_rank), Team = current_team,
      FPPG = round(league_projected_fppg, 1), Elite = pct(elite_probability), Upside = round(upside_index)
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)
  output$home_top10_cards <- renderUI({
    d <- league_rankings() |> slice_head(n = 10)
    tagList(lapply(seq_len(nrow(d)), function(i) player_mobile_card(d[i, ])))
  })

  redraft_filtered <- reactive({
    d <- league_rankings()
    if (!is.null(input$rank_position) && input$rank_position != "ALL") d <- d |> filter(position == input$rank_position)
    if (!is.null(input$rank_team) && input$rank_team != "ALL") d <- d |> filter(current_team == input$rank_team)
    if (!is.null(input$rank_search) && nzchar(trimws(input$rank_search))) d <- d |> filter(grepl(trimws(input$rank_search), player_display_name, ignore.case = TRUE))
    sort_name <- if (is.null(input$rank_sort) || !nzchar(input$rank_sort)) "League Rank" else input$rank_sort
    d <- switch(sort_name,
      "Projected FPPG" = d |> arrange(desc(league_projected_fppg)),
      "Upside" = d |> arrange(desc(upside_index)),
      "Elite Probability" = d |> arrange(desc(elite_probability)),
      "Breakout Probability" = d |> arrange(desc(breakout_probability)),
      d |> arrange(league_overall_rank)
    )
    if (!is.null(input$rank_count) && input$rank_count != "All") d <- d |> slice_head(n = as.integer(input$rank_count))
    d
  })

  output$redraft_table <- renderTable({
    redraft_filtered() |> transmute(
      Rank = league_overall_rank, Player = player_display_name, Pos = paste0(position, league_position_rank), Team = current_team,
      FPPG = round(league_projected_fppg, 1), Floor = round(league_floor_fppg, 1), Ceiling = round(league_ceiling_fppg, 1),
      Elite = pct(elite_probability), Starter = pct(starter_probability), Breakout = pct(breakout_probability),
      Upside = round(upside_index), Outlook = fantasy_outlook
    )
  }, striped = TRUE, bordered = FALSE, hover = TRUE, spacing = "s", rownames = FALSE)
  output$redraft_cards <- renderUI({
    d <- redraft_filtered()
    if (nrow(d) > 100) d <- d |> slice_head(n = 100)
    tagList(lapply(seq_len(nrow(d)), function(i) player_mobile_card(d[i, ])))
  })

  weekly_league <- reactive({
    if (nrow(weekly_base) == 0) return(data.frame())
    d <- weekly_base
    s <- settings()
    rec <- vcol(d, "projected_receptions", 0)
    pass_td <- vcol(d, "projected_pass_tds", 0)
    te_bonus <- ifelse(as.character(d$position) == "TE", s$te_premium, 0)
    score_shift <- rec * ((s$reception_points - SCORING$reception) + te_bonus) + pass_td * (s$pass_td_points - SCORING$pass_td)
    d$league_score_shift <- score_shift
    d$league_projected_weekly_fppg <- pmax(0, vcol(d, "projected_weekly_fppg", 0) + score_shift)
    d$league_weekly_median <- pmax(0, vcol(d, "weekly_median", vcol(d, "projected_weekly_fppg", 0)) + score_shift)
    d$league_weekly_floor <- pmax(0, vcol(d, "weekly_floor", 0) + score_shift)
    d$league_weekly_ceiling <- pmax(0, vcol(d, "weekly_ceiling", 0) + score_shift)
    # Recompute league scoring rank; empirical Top-N probability remains the
    # validated default-scoring distribution and is displayed as such.
    d |> dplyr::group_by(week, position) |>
      dplyr::arrange(dplyr::desc(league_projected_weekly_fppg), .by_group = TRUE) |>
      dplyr::mutate(weekly_position_rank = dplyr::row_number()) |>
      dplyr::ungroup()
  })

  weekly_filtered <- reactive({
    d <- weekly_league(); if (nrow(d) == 0) return(data.frame())
    d <- d |> dplyr::filter(week == as.integer(input$weekly_week %||% current_week_value), is_actual == 0)
    if (!is.null(input$weekly_position) && input$weekly_position != "ALL") d <- d |> dplyr::filter(position == input$weekly_position)
    if (!is.null(input$weekly_search) && nzchar(trimws(input$weekly_search))) d <- d |> dplyr::filter(grepl(trimws(input$weekly_search), player_display_name, ignore.case = TRUE))
    sortby <- input$weekly_sort %||% "Projection"
    if (sortby == "Floor") d <- d |> dplyr::arrange(dplyr::desc(league_weekly_floor))
    else if (sortby == "Ceiling") d <- d |> dplyr::arrange(dplyr::desc(league_weekly_ceiling))
    else if (sortby == "Top-N Probability") d <- d |> dplyr::arrange(dplyr::desc(topN_probability), dplyr::desc(league_projected_weekly_fppg))
    else if (sortby == "Boom Probability") d <- d |> dplyr::arrange(dplyr::desc(boom_probability), dplyr::desc(league_projected_weekly_fppg))
    else if (sortby == "Matchup Boost") d <- d |> dplyr::arrange(dplyr::desc(matchup_delta_22), dplyr::desc(league_projected_weekly_fppg))
    else d <- d |> dplyr::arrange(dplyr::desc(league_projected_weekly_fppg))
    d
  })

  output$weekly_status <- renderUI({
    if (nrow(weekly_base) == 0) return(div(class = "fm-card", strong("Weekly engine not generated yet."), p(class = "fm-muted", "Run source(\"MOBILE_RUN_WEEKLY_FAST.R\") once, then relaunch the app.")))
    NULL
  })
  output$weekly_top_player <- renderUI({
    d <- weekly_filtered(); if (nrow(d) == 0) return(kpi("Top projection", "—", "No projection rows"))
    x <- d |> dplyr::arrange(dplyr::desc(league_projected_weekly_fppg)) |> dplyr::slice(1)
    kpi(paste0("Week ", x$week, " #1"), x$player_display_name, paste0(x$position, " · vs ", x$opponent, " · ", num1(x$league_projected_weekly_fppg), " pts"), TRUE)
  })
  output$weekly_best_matchup <- renderUI({
    d <- weekly_filtered(); if (nrow(d) == 0) return(kpi("Best matchup", "—"))
    x <- d |> dplyr::arrange(dplyr::desc(matchup_delta_22)) |> dplyr::slice(1)
    kpi("Best matchup", x$player_display_name, paste0(field(x, "matchup_grade", "Neutral"), " · ", sprintf("%+.1f", field(x, "matchup_delta_22", 0)), " pts matchup delta"))
  })
  output$weekly_safest_player <- renderUI({
    d <- weekly_filtered(); if (nrow(d) == 0) return(kpi("Highest floor", "—"))
    x <- d |> dplyr::arrange(dplyr::desc(league_weekly_floor)) |> dplyr::slice(1)
    kpi("Highest floor", x$player_display_name, paste0(num1(x$league_weekly_floor), " floor · ", pct(field(x, "bust_probability", NA_real_)), " bust"))
  })
  output$weekly_week_summary <- renderUI({
    d <- weekly_filtered(); wk <- if (!is.null(input$weekly_week)) input$weekly_week else current_week_value
    kpi("Projection week", paste0("Week ", wk), paste0(nrow(d), " ranked rows · ", settings()$scoring_preset, " · updated ", weekly_refresh_label))
  })
  output$weekly_table <- renderTable({
    d <- weekly_filtered(); if (nrow(d) == 0) return(data.frame(Message = "No weekly projections for these filters."))
    d |> dplyr::slice_head(n = 160) |> dplyr::transmute(
      Rank = weekly_position_rank, Player = player_display_name, Pos = position, Team = team, Opp = opponent,
      Proj = round(league_projected_weekly_fppg, 1), Median = round(league_weekly_median, 1), Floor = round(league_weekly_floor, 1), Ceiling = round(league_weekly_ceiling, 1),
      `Top-N` = ifelse(is.finite(topN_probability), paste0(round(100 * topN_probability), "%"), "—"),
      Boom = ifelse(is.finite(boom_probability), paste0(round(100 * boom_probability), "%"), "—"),
      Matchup = ifelse(is.na(matchup_grade), "Neutral", matchup_grade), `Δ` = sprintf("%+.1f", vcol(d, "matchup_delta_22", 0)),
      `Role Δ` = sprintf("%+.1f", vcol(d, "role_fppg_trend", 0)), `Impl Pts` = round(vcol(d, "implied_team_total", 0), 1),
      Injury = ifelse(vcol(d, "injury_risk", 0) > 0, injury_status, "—")
    )
  }, striped = TRUE, bordered = FALSE, hover = TRUE, spacing = "s", rownames = FALSE)
  output$weekly_cards <- renderUI({
    d <- weekly_filtered(); if (nrow(d) == 0) return(div(class = "fm-muted", "No weekly projections for these filters."))
    d <- d |> dplyr::slice_head(n = 100)
    tagList(lapply(seq_len(nrow(d)), function(i) weekly_mobile_card(d[i, ])))
  })
  ros_league <- reactive({
    d <- weekly_league()
    if (nrow(d) == 0) return(data.frame())
    d <- d |> dplyr::filter(is_actual == 0)
    if (nrow(d) == 0) return(data.frame())
    d$matchup_delta_22_safe <- vcol(d, "matchup_delta_22", 0)
    d |>
      dplyr::group_by(player_id, player_display_name, position, team) |>
      dplyr::summarise(
        games_left = dplyr::n(),
        ros_points = sum(league_projected_weekly_fppg, na.rm = TRUE),
        ros_fppg = mean(league_projected_weekly_fppg, na.rm = TRUE),
        ros_floor = sum(league_weekly_floor, na.rm = TRUE),
        ros_ceiling = sum(league_weekly_ceiling, na.rm = TRUE),
        favorable_games = sum(matchup_delta_22_safe >= 0.75, na.rm = TRUE),
        difficult_games = sum(matchup_delta_22_safe <= -0.75, na.rm = TRUE),
        avg_matchup_delta = mean(matchup_delta_22_safe, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::group_by(position) |>
      dplyr::arrange(dplyr::desc(ros_points), .by_group = TRUE) |>
      dplyr::mutate(ros_position_rank = dplyr::row_number()) |>
      dplyr::ungroup()
  })

  ros_filtered <- reactive({
    d <- ros_league(); if (nrow(d) == 0) return(data.frame())
    if (!is.null(input$ros_position) && input$ros_position != "ALL") d <- d |> dplyr::filter(position == input$ros_position)
    if (!is.null(input$ros_search) && nzchar(trimws(input$ros_search))) d <- d |> dplyr::filter(grepl(trimws(input$ros_search), player_display_name, ignore.case = TRUE))
    sortby <- input$ros_sort %||% "ROS Points"
    if (sortby == "ROS FPPG") d <- d |> dplyr::arrange(dplyr::desc(ros_fppg))
    else if (sortby == "Floor") d <- d |> dplyr::arrange(dplyr::desc(ros_floor))
    else if (sortby == "Ceiling") d <- d |> dplyr::arrange(dplyr::desc(ros_ceiling))
    else if (sortby == "Favorable Games") d <- d |> dplyr::arrange(dplyr::desc(favorable_games), dplyr::desc(ros_points))
    else d <- d |> dplyr::arrange(dplyr::desc(ros_points))
    d
  })

  output$ros_top_player <- renderUI({
    d <- ros_filtered(); if (nrow(d) == 0) return(kpi("Top ROS", "—", "Run 2.2 weekly projections"))
    x <- d |> dplyr::slice(1)
    kpi("Top ROS", x$player_display_name, paste0(x$position, " · ", num1(x$ros_points), " pts remaining"), TRUE)
  })
  output$ros_best_schedule <- renderUI({
    d <- ros_filtered(); if (nrow(d) == 0) return(kpi("Best schedule", "—"))
    x <- d |> dplyr::arrange(dplyr::desc(favorable_games), dplyr::desc(avg_matchup_delta)) |> dplyr::slice(1)
    kpi("Best schedule", x$player_display_name, paste0(x$favorable_games, " favorable · ", x$difficult_games, " difficult"))
  })
  output$ros_games_left <- renderUI({
    d <- ros_filtered(); if (nrow(d) == 0) return(kpi("Remaining", "—"))
    kpi("Remaining horizon", paste0(max(d$games_left, na.rm = TRUE), " games"), "Each matchup is projected separately")
  })
  output$ros_table <- renderTable({
    d <- ros_filtered(); if (nrow(d) == 0) return(data.frame(Message = "Run the 2.2 weekly engine to generate rest-of-season projections."))
    d |> dplyr::slice_head(n = 120) |> dplyr::transmute(
      Rank = ros_position_rank, Player = player_display_name, Pos = position, Team = team, Games = games_left,
      `ROS Pts` = round(ros_points, 1), `ROS FPPG` = round(ros_fppg, 1), Floor = round(ros_floor, 1), Ceiling = round(ros_ceiling, 1),
      Favorable = favorable_games, Difficult = difficult_games, `Avg Matchup Δ` = sprintf("%+.1f", avg_matchup_delta)
    )
  }, striped = TRUE, bordered = FALSE, hover = TRUE, spacing = "s", rownames = FALSE)

  compare_data <- reactive({
    d <- weekly_league(); if (nrow(d) == 0) return(data.frame())
    wk <- as.integer(input$compare_week %||% current_week_value)
    d <- d |> dplyr::filter(week == wk, is_actual == 0)
    picks <- input$compare_players
    if (is.null(picks) || length(picks) == 0) return(data.frame())
    d |> dplyr::filter(player_display_name %in% picks) |> dplyr::arrange(dplyr::desc(league_projected_weekly_fppg))
  })
  output$compare_pick <- renderUI({
    d <- compare_data(); if (nrow(d) == 0) return(kpi("Model pick", "—", "Select 2-4 players"))
    x <- d |> dplyr::slice(1); kpi("Model pick", x$player_display_name, paste0(num1(x$league_projected_weekly_fppg), " pts · ", field(x, "matchup_grade", "Neutral")), TRUE)
  })
  output$compare_confidence <- renderUI({
    d <- compare_data(); if (nrow(d) < 2) return(kpi("Decision edge", "—", "Select at least two players"))
    gap <- d$league_projected_weekly_fppg[1] - d$league_projected_weekly_fppg[2]
    span <- max(2, d$league_weekly_ceiling[1] - d$league_weekly_floor[1])
    conf <- if (gap >= 0.30 * span) "Strong" else if (gap >= 0.15 * span) "Moderate" else "Close call"
    kpi("Decision edge", conf, paste0(sprintf("%+.1f", gap), " projected-point gap"))
  })
  output$compare_risk <- renderUI({
    d <- compare_data(); if (nrow(d) == 0) return(kpi("Best floor", "—"))
    x <- d |> dplyr::arrange(dplyr::desc(league_weekly_floor)) |> dplyr::slice(1)
    kpi("Best floor", x$player_display_name, paste0(num1(x$league_weekly_floor), " pts · bust ", pct(field(x, "bust_probability", NA_real_))))
  })
  output$compare_table <- renderTable({
    d <- compare_data(); if (nrow(d) == 0) return(data.frame(Message = "Select players above."))
    d |> dplyr::transmute(
      Player = player_display_name, Pos = position, Opp = opponent, Projection = round(league_projected_weekly_fppg, 1),
      Floor = round(league_weekly_floor, 1), Ceiling = round(league_weekly_ceiling, 1),
      `Top-N` = ifelse(is.finite(topN_probability), paste0(round(100 * topN_probability), "%"), "—"),
      Boom = ifelse(is.finite(boom_probability), paste0(round(100 * boom_probability), "%"), "—"),
      Matchup = matchup_grade, `Matchup Δ` = sprintf("%+.1f", matchup_delta_22),
      `Role Δ` = sprintf("%+.1f", role_fppg_trend), `Impl Pts` = round(implied_team_total, 1)
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)
  output$compare_reason <- renderUI({
    d <- compare_data(); if (nrow(d) < 2) return(p(class = "fm-muted", "Select at least two players to generate a decision explanation."))
    a <- d[1, ]; b <- d[2, ]
    reasons <- c()
    if (a$league_weekly_floor > b$league_weekly_floor + .5) reasons <- c(reasons, paste0("higher floor (+", num1(a$league_weekly_floor - b$league_weekly_floor), ")"))
    if (field(a, "matchup_delta_22", 0) > field(b, "matchup_delta_22", 0) + .5) reasons <- c(reasons, "better opponent-specific matchup")
    if (field(a, "role_fppg_trend", 0) > field(b, "role_fppg_trend", 0) + .5) reasons <- c(reasons, "stronger recent role trend")
    if (field(a, "implied_team_total", 0) > field(b, "implied_team_total", 0) + 2) reasons <- c(reasons, "better implied scoring environment")
    if (field(a, "boom_probability", 0) > field(b, "boom_probability", 0) + .08) reasons <- c(reasons, "higher boom probability")
    if (length(reasons) == 0) reasons <- "the combined 2.2 stack gives a small overall edge; the decision is close"
    tagList(strong(paste0(a$player_display_name, " over ", b$player_display_name)), p(class = "fm-muted", paste(reasons, collapse = "; "), "."))
  })

  selected_player <- reactive({
    req(input$player_select)
    league_rankings() |> filter(player_display_name == input$player_select) |> slice(1)
  })

  output$player_header <- renderUI({
    d <- selected_player()
    div(class = "fm-card fm-player-header",
        div(h3(d$player_display_name), p(class = "fm-muted", paste0("Fantasy outlook: ", d$fantasy_outlook))),
        div(
          span(class = "fm-pill", paste0(d$position, d$league_position_rank)),
          span(class = "fm-pill", d$current_team),
          span(class = "fm-pill", paste0("Overall #", d$league_overall_rank)),
          span(class = "fm-pill", paste0("Confidence: ", d$confidence)),
          if (isTRUE(d$is_rookie == 1)) span(class = "fm-pill", "Rookie") else NULL
        ))
  })
  output$player_projection <- renderUI({ d <- selected_player(); kpi("Projected FPPG", num1(d$league_projected_fppg), paste0(num1(d$league_projected_points), " projected points"), TRUE) })
  output$player_elite <- renderUI({ d <- selected_player(); kpi("Elite Probability", pct(d$elite_probability), paste0("Starter: ", pct(d$starter_probability))) })
  output$player_breakout <- renderUI({ d <- selected_player(); kpi("Breakout Probability", pct(d$breakout_probability), paste0("Upside index: ", round(d$upside_index))) })
  output$player_value <- renderUI({ d <- selected_player(); kpi("Value Over Replacement", num1(d$league_vorp_fppg), "FPPG above league replacement") })

  selected_player_week <- reactive({
    req(input$player_select)
    d <- weekly_league()
    if (nrow(d) == 0) return(data.frame())
    x <- d |> dplyr::filter(player_display_name == input$player_select, is_actual == 0, week >= current_week_value) |> dplyr::arrange(week) |> dplyr::slice(1)
    if (nrow(x) == 0) x <- d |> dplyr::filter(player_display_name == input$player_select, is_actual == 0) |> dplyr::arrange(week) |> dplyr::slice(1)
    x
  })
  output$player_week_projection <- renderUI({
    d <- selected_player_week(); if (nrow(d) == 0) return(kpi("Next week", "—", "Run the 2.2 weekly engine"))
    kpi(paste0("Week ", d$week, " projection"), num1(d$league_projected_weekly_fppg), paste0(d$team, " vs ", d$opponent, " · floor ", num1(d$league_weekly_floor), " / ceiling ", num1(d$league_weekly_ceiling)), TRUE)
  })
  output$player_week_matchup <- renderUI({
    d <- selected_player_week(); if (nrow(d) == 0) return(kpi("Matchup", "—"))
    kpi("Matchup", field(d, "matchup_grade", "Neutral"), paste0(sprintf("%+.1f", field(d, "matchup_delta_22", 0)), " pts vs neutral"))
  })
  output$player_week_topn <- renderUI({
    d <- selected_player_week(); if (nrow(d) == 0) return(kpi("Top-tier chance", "—"))
    kpi("Top-tier chance", pct(field(d, "topN_probability", NA_real_)), paste0("Projected ", d$position, d$weekly_position_rank, " under current league scoring"))
  })
  output$player_week_boom <- renderUI({
    d <- selected_player_week(); if (nrow(d) == 0) return(kpi("Boom / Bust", "—"))
    kpi("Boom / Bust", paste0(pct(field(d, "boom_probability", NA_real_)), " / ", pct(field(d, "bust_probability", NA_real_))), "Empirical OOF residual distribution")
  })
  output$player_week_context <- renderUI({
    d <- selected_player_week(); if (nrow(d) == 0) return(p(class = "fm-muted", "Weekly projection not generated yet."))
    role_label <- if (d$position == "QB") paste0("Pass att (3g) ", num1(field(d, "roll3_pass_attempts", 0)), " · Carries ", num1(field(d, "roll3_carries", 0)))
      else if (d$position == "RB") paste0("Carries (3g) ", num1(field(d, "roll3_carries", 0)), " · Targets ", num1(field(d, "roll3_targets", 0)))
      else paste0("Targets (3g) ", num1(field(d, "roll3_targets", 0)), " · Target share ", pct(field(d, "roll3_target_share", 0)))
    tagList(
      div(class = "fm-context-grid",
          div(strong(paste0("Week ", d$week)), span(paste0(" vs ", d$opponent))),
          div(strong(num1(field(d, "implied_team_total", 0))), span(" Implied team points")),
          div(strong(num1(field(d, "roll3_fppg", 0))), span(" Recent 3-game FPPG")),
          div(strong(pct(field(d, "roll3_offense_pct", 0))), span(" Offensive snap share")),
          div(strong(sprintf("%+.1f", field(d, "role_fppg_trend", 0))), span(" Role/FPPG trend")),
          div(strong(sprintf("%+.1f", field(d, "matchup_delta_22", 0))), span(" Matchup delta"))
      ),
      hr(),
      p(class = "fm-muted", role_label),
      p(class = "fm-muted", paste0("Architecture: prior ", pct(field(d, "stack_prior_weight", 0)), " · neutral ", pct(field(d, "stack_neutral_weight", 0)), " · structured+matchup ", pct(field(d, "stack_structured_matchup_weight", 0)), " · full direct ", pct(field(d, "stack_direct_weight", 0))))
    )
  })
  output$player_week_statline <- renderTable({
    d <- selected_player_week(); if (nrow(d) == 0) return(data.frame(Message = "Weekly stat line not generated."))
    pos <- as.character(d$position[1])
    if (pos == "QB") {
      return(data.frame(
        Metric = c("Pass attempts", "Pass yards", "Pass TD", "Interceptions", "Carries", "Rush yards", "Rush TD", "Neutral structured FPPG", "Matchup delta", "Final projection"),
        Value = c(num1(field(d, "projected_pass_attempts", NA_real_)), num1(field(d, "projected_pass_yards", NA_real_)), num2(field(d, "projected_pass_tds", NA_real_)), num2(field(d, "projected_interceptions", NA_real_)), num1(field(d, "projected_carries", NA_real_)), num1(field(d, "projected_rush_yards", NA_real_)), num2(field(d, "projected_rush_tds", NA_real_)), num1(field(d, "structured_neutral_fppg", NA_real_)), sprintf("%+.1f", field(d, "matchup_delta_22", 0)), num1(field(d, "league_projected_weekly_fppg", NA_real_))), check.names = FALSE
      ))
    }
    metrics <- c("Targets", "Receptions", "Receiving yards", "Receiving TD")
    vals <- c(num1(field(d, "projected_targets", NA_real_)), num1(field(d, "projected_receptions", NA_real_)), num1(field(d, "projected_rec_yards", NA_real_)), num2(field(d, "projected_rec_tds", NA_real_)))
    if (pos %in% c("RB", "WR")) {
      metrics <- c(metrics, "Carries", "Rush yards", "Rush TD")
      vals <- c(vals, num1(field(d, "projected_carries", NA_real_)), num1(field(d, "projected_rush_yards", NA_real_)), num2(field(d, "projected_rush_tds", NA_real_)))
    }
    metrics <- c(metrics, "Neutral structured FPPG", "Matchup delta", "Final projection")
    vals <- c(vals, num1(field(d, "structured_neutral_fppg", NA_real_)), sprintf("%+.1f", field(d, "matchup_delta_22", 0)), num1(field(d, "league_projected_weekly_fppg", NA_real_)))
    data.frame(Metric = metrics, Value = vals, check.names = FALSE)
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$player_range_bar <- renderUI({
    d <- selected_player(); floor <- field(d, "league_floor_fppg", 0); mid <- field(d, "league_projected_fppg", 0); ceil <- field(d, "league_ceiling_fppg", 0)
    maxv <- max(1, ceil * 1.08); left <- 100 * floor / maxv; midp <- 100 * mid / maxv; right <- 100 * ceil / maxv
    div(class = "fm-range-wrap",
        div(class = "fm-range-labels", span(paste0("Floor ", num1(floor))), span(paste0("Projection ", num1(mid))), span(paste0("Ceiling ", num1(ceil)))),
        div(class = "fm-range-track",
            div(class = "fm-range-band", style = paste0("left:", left, "%;width:", max(1, right-left), "%")),
            div(class = "fm-range-dot", style = paste0("left:", midp, "%"))))
  })

  output$player_team_context <- renderUI({
    d <- selected_player()
    qb_name <- field(d, "prior_primary_qb_name", "Prior team primary QB")

    if (d$position == "QB") {
      return(tagList(
        div(class = "fm-context-grid",
            div(strong(pct(field(d, "team_prior_pass_rate", 0))), span(" Team pass rate")),
            div(strong(pct(field(d, "team_prior_neutral_pass_rate", 0))), span(" Neutral pass rate")),
            div(strong(num1(field(d, "team_prior_pass_attempts_pg", 0))), span(" Pass attempts/game")),
            div(strong(num1(field(d, "team_prior_plays_pg", 0))), span(" Plays/game")),
            div(strong(pct(field(d, "team_prior_redzone_pass_rate", 0))), span(" Red-zone pass rate")),
            div(strong(pct(field(d, "team_prior_shotgun_rate", 0))), span(" Shotgun rate"))
        ),
        hr(),
        p(class = "fm-muted", paste0(
          "QB/team continuity: ", ifelse(field(d, "qb_team_continuity", 0) > 0, "returning primary QB", "new/non-primary QB context"),
          " · team deep throw rate ", pct(field(d, "team_prior_deep_throw_rate", 0)),
          " · no-huddle rate ", pct(field(d, "team_prior_no_huddle_rate", 0))
        ))
      ))
    }

    if (d$position == "RB") {
      return(tagList(
        div(class = "fm-context-grid",
            div(strong(pct(field(d, "team_prior_rush_rate", 0))), span(" Team rush rate")),
            div(strong(pct(field(d, "team_prior_neutral_rush_rate", 0))), span(" Neutral rush rate")),
            div(strong(num1(field(d, "team_prior_carries_pg", 0))), span(" Team carries/game")),
            div(strong(num1(field(d, "team_prior_rush_yd_pg", 0))), span(" Team rush yards/game")),
            div(strong(pct(field(d, "team_prior_redzone_rush_rate", 0))), span(" Red-zone rush rate")),
            div(strong(pct(field(d, "qb_prior_rb_target_rate", 0))), span(" QB RB-target rate"))
        ),
        hr(),
        div(class = "fm-fit-row",
            span(paste0("Prior team carry share ", pct(field(d, "rb_prior_team_carry_share", 0)))),
            span(paste0("Receiving fit ", pct(field(d, "rb_receiving_fit", 0)))),
            span(paste0("Touches/game ", num1(field(d, "rb_prior_touch_opportunity", 0)))))
      ))
    }

    if (d$position == "TE") {
      return(tagList(
        div(class = "fm-context-grid",
            div(strong(pct(field(d, "team_prior_pass_rate", 0))), span(" Team pass rate")),
            div(strong(pct(field(d, "team_prior_middle_throw_rate", 0))), span(" Team middle throw rate")),
            div(strong(pct(field(d, "team_prior_redzone_pass_rate", 0))), span(" Red-zone pass rate")),
            div(strong(num1(field(d, "team_prior_plays_pg", 0))), span(" Plays/game")),
            div(strong(pct(field(d, "qb_prior_te_target_rate", 0))), span(" QB TE-target rate")),
            div(strong(pct(field(d, "qb_prior_redzone_throw_rate", 0))), span(" QB red-zone rate"))
        ),
        hr(),
        p(strong(qb_name), class = "fm-context-title"),
        div(class = "fm-fit-row",
            span(paste0("Middle fit ", pct(field(d, "te_middle_fit", 0)))),
            span(paste0("Red-zone fit ", pct(field(d, "te_redzone_fit", 0)))),
            span(paste0("Route fit ", pct(field(d, "te_route_fit", 0)))),
            span(paste0("TE context ", pct(field(d, "te_context_fit_score", 0)))))
      ))
    }

    tagList(
      div(class = "fm-context-grid",
          div(strong(pct(field(d, "team_prior_pass_rate", 0))), span(" Team pass rate")),
          div(strong(pct(field(d, "team_prior_neutral_pass_rate", 0))), span(" Neutral pass rate")),
          div(strong(num1(field(d, "team_prior_pass_attempts_pg", 0))), span(" Pass attempts/game")),
          div(strong(num1(field(d, "team_prior_plays_pg", 0))), span(" Plays/game")),
          div(strong(pct(field(d, "team_prior_redzone_pass_rate", 0))), span(" Red-zone pass rate")),
          div(strong(num1(field(d, "qb_prior_avg_air_yards", 0))), span(" QB average target depth"))
      ),
      hr(),
      p(strong(qb_name), class = "fm-context-title"),
      p(class = "fm-muted", paste0(
        "QB tendencies: deep ", pct(field(d, "qb_prior_deep_throw_rate", 0)),
        " · middle ", pct(field(d, "qb_prior_middle_throw_rate", 0)),
        " · red zone ", pct(field(d, "qb_prior_redzone_throw_rate", 0)),
        " · WR target rate ", pct(field(d, "qb_prior_wr_target_rate", 0)),
        " · most common route family ", route_name(d, "qb_prior_route_")
      )),
      div(class = "fm-fit-row",
          span(paste0("Depth fit ", pct(field(d, "qb_receiver_depth_fit", 0)))),
          span(paste0("Location fit ", pct(field(d, "qb_receiver_location_fit", 0)))),
          span(paste0("Route fit ", pct(field(d, "qb_receiver_route_fit", 0)))),
          span(paste0("WR context ", pct(field(d, "wr_context_fit_score", 0)))))
    )
  })

  output$player_receiver_context <- renderUI({
    d <- selected_player()

    if (d$position == "QB") {
      return(tagList(
        div(class = "fm-context-grid",
            div(strong(num1(field(d, "player_qb_prior_avg_air_yards", 0))), span(" Avg target depth")),
            div(strong(pct(field(d, "player_qb_prior_deep_throw_rate", 0))), span(" Deep throw rate")),
            div(strong(pct(field(d, "player_qb_prior_middle_throw_rate", 0))), span(" Middle throw rate")),
            div(strong(pct(field(d, "player_qb_prior_redzone_throw_rate", 0))), span(" Red-zone dropback rate")),
            div(strong(num1(field(d, "player_qb_prior_ngs_time_to_throw", 0))), span(" NGS time to throw")),
            div(strong(route_name(d, "player_qb_prior_route_")), span(" Top targeted route family"))
        ),
        hr(),
        p(class = "fm-muted", paste0(
          "Target preference WR/TE/RB: ", pct(field(d, "player_qb_prior_wr_target_rate", 0)), "/",
          pct(field(d, "player_qb_prior_te_target_rate", 0)), "/", pct(field(d, "player_qb_prior_rb_target_rate", 0)),
          " · NGS aggressiveness: ", num1(field(d, "player_qb_prior_ngs_aggressiveness", 0)),
          " · CPOE: ", num1(field(d, "player_qb_prior_ngs_cpoe", 0))
        ))
      ))
    }

    alignment_text <- if (field(d, "alignment_available", 0) > 0) {
      paste0("Slot ", pct(field(d, "rec_prior_slot_rate", 0)), " · Wide ", pct(field(d, "rec_prior_wide_rate", 0)), " · Inline ", pct(field(d, "rec_prior_inline_rate", 0)))
    } else "True slot/wide alignment source not connected"

    if (d$position == "RB") {
      return(tagList(
        div(class = "fm-context-grid",
            div(strong(num1(field(d, "prior_carries_pg", 0))), span(" Carries/game")),
            div(strong(num1(field(d, "prior_targets_pg", 0))), span(" Targets/game")),
            div(strong(pct(field(d, "rec_prior_short_target_rate", 0))), span(" Short target rate")),
            div(strong(pct(field(d, "rec_prior_redzone_target_rate", 0))), span(" Red-zone target rate")),
            div(strong(num1(field(d, "prior_yards_per_carry", 0))), span(" Yards/carry")),
            div(strong(pct(field(d, "rec_prior_route_screen", 0))), span(" Screen target share"))
        ),
        hr(),
        p(class = "fm-muted", paste0("Estimated receiving opportunity: ", num1(field(d, "team_target_opportunity", 0)), " targets/game-equivalent from prior share × team pass volume."))
      ))
    }

    if (d$position == "TE") {
      return(tagList(
        div(class = "fm-context-grid",
            div(strong(num1(field(d, "prior_targets_pg", 0))), span(" Targets/game")),
            div(strong(pct(field(d, "rec_prior_middle_target_rate", 0))), span(" Middle target rate")),
            div(strong(pct(field(d, "rec_prior_redzone_target_rate", 0))), span(" Red-zone target rate")),
            div(strong(pct(field(d, "rec_prior_short_target_rate", 0))), span(" Short target rate")),
            div(strong(num1(field(d, "rec_prior_ngs_avg_separation", 0))), span(" NGS separation")),
            div(strong(route_name(d, "rec_prior_route_")), span(" Top route family"))
        ),
        hr(),
        p(class = "fm-muted", alignment_text),
        p(class = "fm-muted", paste0("Estimated team target opportunity: ", num1(field(d, "team_target_opportunity", 0)), " targets/game-equivalent."))
      ))
    }

    tagList(
      div(class = "fm-context-grid",
          div(strong(num1(field(d, "rec_prior_avg_depth_target", 0))), span(" Avg target depth")),
          div(strong(pct(field(d, "rec_prior_deep_target_rate", 0))), span(" Deep target rate")),
          div(strong(pct(field(d, "rec_prior_middle_target_rate", 0))), span(" Middle target rate")),
          div(strong(pct(field(d, "rec_prior_redzone_target_rate", 0))), span(" Red-zone target rate")),
          div(strong(num1(field(d, "rec_prior_ngs_avg_separation", 0))), span(" NGS separation")),
          div(strong(route_name(d, "rec_prior_route_")), span(" Top route family"))
      ),
      hr(),
      p(class = "fm-muted", alignment_text),
      p(class = "fm-muted", paste0("Estimated team target opportunity: ", num1(field(d, "team_target_opportunity", 0)), " targets/game-equivalent."))
    )
  })

  output$player_component_table <- renderTable({
    d <- selected_player()
    pos <- as.character(d$position[1])
    if (pos == "QB") {
      return(data.frame(
        Metric = c("Pass attempts/game", "Pass yards/game", "Pass TD/game", "INT/game", "Rush attempts/game", "Rush yards/game", "Rush TD/game", "Structured FPPG", "Residual correction", "Structured + residual FPPG", "2.0 weight"),
        Value = c(num1(field(d, "projected_pass_attempts_pg", NA_real_)), num1(field(d, "projected_pass_yards_pg", NA_real_)), num2(field(d, "projected_pass_tds_pg", NA_real_)), num2(field(d, "projected_interceptions_pg", NA_real_)), num1(field(d, "projected_carries_pg", NA_real_)), num1(field(d, "projected_rush_yards_pg", NA_real_)), num2(field(d, "projected_rush_tds_pg", NA_real_)), num1(field(d, "opportunity_fppg_calibrated", NA_real_)), num2(field(d, "residual_correction_20", 0)), num1(field(d, "opportunity_residual_fppg", NA_real_)), pct(field(d, "opportunity_weight", field(d, "component_weight", 0))))
      ))
    }
    metrics <- c("Targets/game", "Receptions/game", "Receiving yards/game", "Receiving TD/game")
    vals <- c(num1(field(d, "projected_targets_pg", NA_real_)), num1(field(d, "projected_receptions_pg", NA_real_)), num1(field(d, "projected_rec_yards_pg", NA_real_)), num2(field(d, "projected_rec_tds_pg", NA_real_)))
    if (pos %in% c("RB", "WR")) {
      metrics <- c(metrics, "Carries/game", "Rush yards/game", "Rush TD/game")
      vals <- c(vals, num1(field(d, "projected_carries_pg", NA_real_)), num1(field(d, "projected_rush_yards_pg", NA_real_)), num2(field(d, "projected_rush_tds_pg", NA_real_)))
    }
    metrics <- c(metrics, "Structured FPPG", "Residual correction", "Structured + residual FPPG", "2.0 weight")
    vals <- c(vals, num1(field(d, "opportunity_fppg_calibrated", NA_real_)), num2(field(d, "residual_correction_20", 0)), num1(field(d, "opportunity_residual_fppg", NA_real_)), pct(field(d, "opportunity_weight", field(d, "component_weight", 0))))
    data.frame(Metric = metrics, Value = vals, check.names = FALSE)
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$player_projection_table <- renderTable({
    d <- selected_player()
    data.frame(
      Metric = c("Floor FPPG", "Projection FPPG", "Ceiling FPPG", "Projected Games", "Full 17-game Points", "Age", "Architecture"),
      Value = c(num1(d$league_floor_fppg), num1(d$league_projected_fppg), num1(d$league_ceiling_fppg), num1(d$projected_games), num1(d$league_full_17_points), num1(d$age), field(d, "projection_architecture", "1.2 direct guardrail")),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$player_comps_table <- renderTable({
    d <- selected_player()
    if (nrow(comps_base) == 0) return(data.frame(Message = "Player comps were not generated."))
    x <- comps_base |> filter(player_id == d$player_id) |> arrange(comp_rank)
    if (nrow(x) == 0) return(data.frame(Message = "No historical comps found."))
    x |> transmute(Comp = comp_player, Season = comp_season, Similarity = paste0(round(similarity_score), "%"), `Next FPPG` = round(comp_next_season_fppg, 1))
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  dynasty_filtered <- reactive({
    d <- league_dynasty()
    if (!is.null(input$dyn_position) && input$dyn_position != "ALL") d <- d |> filter(position == input$dyn_position)
    if (!is.null(input$dyn_search) && nzchar(trimws(input$dyn_search))) d <- d |> filter(grepl(trimws(input$dyn_search), player_display_name, ignore.case = TRUE))
    if (!is.null(input$dyn_count) && input$dyn_count != "All") d <- d |> slice_head(n = as.integer(input$dyn_count))
    d
  })
  output$dynasty_table <- renderTable({
    dynasty_filtered() |> transmute(
      Rank = league_dynasty_rank, Player = player_display_name, Pos = position, Team = current_team,
      `Dynasty Value` = round(league_dynasty_value), `Year 1 FPPG` = round(year1_fppg, 1), `Year 3 FPPG` = round(year3_fppg, 1),
      Elite = pct(elite_probability), Breakout = pct(breakout_probability), Confidence = confidence
    )
  }, striped = TRUE, bordered = FALSE, hover = TRUE, spacing = "s", rownames = FALSE)

  output$quality_report <- renderText(paste(quality_report, collapse = "\n"))
  output$validation_table <- renderTable({
    if (nrow(validation_base) == 0) return(data.frame(Message = "Validation metrics not found."))
    validation_base |> transmute(Pos = position, N = n, MAE = round(model_MAE, 2), `Baseline MAE` = round(baseline_MAE, 2), `MAE Gain` = paste0(round(MAE_improvement_pct, 1), "%"), Corr = round(correlation, 3), `Rank Corr` = round(spearman_rank_correlation, 3), Bias = round(bias, 2))
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)
  output$probability_table <- renderTable({
    if (nrow(probability_base) == 0) return(data.frame(Message = "Decision-model metrics not found."))
    probability_base |> transmute(Pos = position, N = n, `Elite AUC` = round(elite_AUC, 3), `Starter AUC` = round(starter_AUC, 3), `Breakout AUC` = round(breakout_AUC, 3), `Elite Brier` = round(elite_Brier, 3), `Starter Brier` = round(starter_Brier, 3), `Breakout Brier` = round(breakout_Brier, 3))
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)
  output$weekly_quality_report <- renderText(paste(weekly_quality_report, collapse = "\n"))
  output$weekly_validation_table <- renderTable({
    if (nrow(weekly_validation_base) == 0) return(data.frame(Message = "Run the 2.2 weekly engine to generate weekly validation."))
    if (all(c("final_MAE", "baseline_MAE", "MAE_improvement_vs_prior_pct") %in% names(weekly_validation_base))) {
      return(weekly_validation_base |> dplyr::transmute(Pos = position, N = n, `Weekly MAE` = round(final_MAE, 2), `Prior MAE` = round(baseline_MAE, 2), `MAE Gain` = paste0(round(MAE_improvement_vs_prior_pct, 1), "%"), RMSE = round(final_RMSE, 2), Corr = round(correlation, 3), `Rank Corr` = round(rank_correlation, 3), Bias = round(bias, 2)))
    }
    weekly_validation_base
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$weekly_arch_table <- renderTable({
    if (nrow(weekly_arch_base) == 0) return(data.frame(Message = "2.2 architecture ablation not generated yet."))
    weekly_arch_base |> dplyr::transmute(Pos = position, Method = method, N = n, MAE = round(MAE, 2), RMSE = round(RMSE, 2), Corr = round(correlation, 3), `Rank Corr` = round(rank_correlation, 3), Bias = round(bias, 2))
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$weekly_stack_table <- renderTable({
    if (nrow(weekly_stack_base) == 0) return(data.frame(Message = "2.2 stack selection not generated yet."))
    weekly_stack_base |> dplyr::transmute(Pos = position, Prior = pct(w_prior), Neutral = pct(w_neutral), `Structured + Matchup` = pct(w_structured_matchup), `Full Direct` = pct(w_direct), MAE = round(MAE, 2), RMSE = round(RMSE, 2), Corr = round(correlation, 3))
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$weekly_cohort_table <- renderTable({
    if (nrow(weekly_cohort_base) == 0) return(data.frame(Message = "Starter/relevant cohort validation not generated yet."))
    weekly_cohort_base |> dplyr::transmute(Pos = position, Cohort = cohort, N = n, MAE = round(MAE, 2), `Prior MAE` = round(baseline_MAE, 2), Gain = paste0(round(MAE_improvement_vs_prior_pct, 1), "%"), Corr = round(correlation, 3), `Rank Corr` = round(rank_correlation, 3))
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$weekly_matchup_table <- renderTable({
    if (nrow(weekly_matchup_base) == 0) return(data.frame(Message = "2.2 matchup calibration not generated yet."))
    weekly_matchup_base |> dplyr::transmute(Pos = position, Bucket = matchup_bucket_22, N = n, `Pred Δ` = sprintf("%+.2f", predicted_matchup_delta), `Actual Δ` = sprintf("%+.2f", actual_matchup_delta), `Actual Pts` = round(actual_mean, 2), `Pred Pts` = round(predicted_mean, 2), MAE = round(MAE, 2))
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$version_comparison_table <- renderTable({
    if (nrow(version_compare) == 0) return(data.frame(Message = "Run Model 2.0 to generate version comparisons."))
    required <- c("position", "v2_0_MAE", "v1_2_MAE", "v2_0_RMSE", "v1_2_RMSE", "v2_0_correlation", "v1_2_correlation", "MAE_change_vs_1_2_pct", "RMSE_change_vs_1_2_pct", "correlation_change_vs_1_2")
    if (!all(required %in% names(version_compare))) return(version_compare)
    version_compare |> transmute(
      Pos = position, `2.0 MAE` = round(v2_0_MAE, 2), `1.2 MAE` = round(v1_2_MAE, 2),
      `MAE vs 1.2` = paste0(sprintf("%+.1f", MAE_change_vs_1_2_pct), "%"),
      `2.0 RMSE` = round(v2_0_RMSE, 2), `1.2 RMSE` = round(v1_2_RMSE, 2),
      `RMSE vs 1.2` = paste0(sprintf("%+.1f", RMSE_change_vs_1_2_pct), "%"),
      `2.0 Corr` = round(v2_0_correlation, 3), `1.2 Corr` = round(v1_2_correlation, 3),
      `Corr vs 1.2` = sprintf("%+.3f", correlation_change_vs_1_2)
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)
}

shinyApp(ui = ui, server = server)
