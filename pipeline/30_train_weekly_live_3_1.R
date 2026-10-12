# ============================================================
# FANTASY MODEL 3.1 - GUARDED WEEKLY LEARNING
# ============================================================
# Purpose:
#   * keep canonical historical training data immutable
#   * append finalized CURRENT_SEASON player-weeks to a separate ledger
#   * build rolling current-season out-of-sample challenger forecasts
#   * promote position-by-position only after live accuracy gates pass
#   * preserve prior deployed live model versions before replacement
#
# This script NEVER rewrites data/processed/weekly_model_table_2_3.csv.

source("config.R")
ensure_packages(c("dplyr","readr","tidyr","nflreadr","xgboost","jsonlite","rpart"))
source("R/live_refresh_engine_25.R")
source("R/weekly_engine_31.R")
source("R/weekly_training_history_31.R")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("models", recursive = TRUE, showWarnings = FALSE)
dir.create("models/archive/weekly_3_1_live", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

learning_version <- "3.1-weekly"
historical_path <- "data/processed/weekly_model_table_2_3.csv"
live_history_path <- file.path("data","processed",paste0("weekly_training_live_", CURRENT_SEASON, ".csv"))
strict_learning_path <- file.path("data","processed",paste0("weekly_model_table_3_1_learning_", CURRENT_SEASON, ".csv"))
pregame_feature_path <- file.path("output", paste0("pregame_feature_history_", CURRENT_SEASON, ".csv"))
pregame_projection_path <- file.path("output", paste0("pregame_projection_archive_", CURRENT_SEASON, ".csv"))
manifest_path <- "output/weekly_3_1_live_manifest.csv"
evaluation_path <- "output/weekly_3_1_live_evaluation_history.csv"
oof_path <- "output/weekly_3_1_live_oof.csv"
residual_path <- "output/weekly_3_1_live_residual_pool.csv"

if (!file.exists(historical_path)) stop("Missing immutable historical weekly table: ", historical_path)

# -------------------------------------------------------------------------
# 1) Resolve the last CONSECUTIVE fully completed regular-season week.
# -------------------------------------------------------------------------
schedule_path <- paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv")
# Training must know which games are actually final. The repository cache can
# intentionally contain schedule/context rows without committed live scores, so
# poll nflverse first and use the cache only as an outage fallback.
schedule <- tryCatch(
  nflreadr::load_schedules(seasons = CURRENT_SEASON),
  error = function(e) {
    warning("Fresh schedule poll failed; attempting local cache. ", conditionMessage(e))
    data.frame()
  }
)
if (!nrow(schedule) && file.exists(schedule_path)) {
  schedule <- tryCatch(
    readr::read_csv(schedule_path, show_col_types = FALSE, progress = FALSE),
    error = function(e) data.frame()
  )
}
if (!nrow(schedule)) stop("Weekly learning requires the current-season schedule.")

if ("season" %in% names(schedule)) {
  schedule <- schedule[suppressWarnings(as.integer(schedule$season)) == CURRENT_SEASON, , drop = FALSE]
}
if ("game_type" %in% names(schedule)) {
  reg <- toupper(as.character(schedule$game_type)) == "REG"
  if (any(reg, na.rm = TRUE)) schedule <- schedule[reg %in% TRUE, , drop = FALSE]
}
schedule$week <- suppressWarnings(as.integer(schedule$week))
schedule <- schedule[is.finite(schedule$week), , drop = FALSE]
schedule$.completed <- live25_completed_mask(schedule)

week_state <- schedule |>
  dplyr::group_by(week) |>
  dplyr::summarise(games = dplyr::n(), all_final = all(.completed %in% TRUE), .groups = "drop") |>
  dplyr::arrange(week)

completed_through <- 0L
for (w in seq_len(max(week_state$week, na.rm = TRUE))) {
  z <- week_state[week_state$week == w, , drop = FALSE]
  if (!nrow(z) || !isTRUE(z$all_final[1])) break
  completed_through <- w
}
if (completed_through < 1L) {
  cat("[3.1 WEEKLY LEARN] No fully completed regular-season week yet; nothing to train.\n")
  quit(save = "no", status = 0)
}
cat("[3.1 WEEKLY LEARN] Fully completed through Week ", completed_through, ".\n", sep = "")

parse_kickoff31 <- function(gameday, gametime) {
  gd <- as.character(gameday)
  gt <- as.character(gametime)
  gt[is.na(gt) | !nzchar(gt)] <- "00:00:00"
  short <- grepl("^\\d{1,2}:\\d{2}$", gt)
  gt[short] <- paste0(gt[short], ":00")
  as.POSIXct(paste(gd, gt), format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
}
if (!"gametime" %in% names(schedule)) schedule$gametime <- "00:00:00"
schedule$.kickoff <- parse_kickoff31(schedule$gameday, schedule$gametime)
team_kickoff <- dplyr::bind_rows(
  schedule |> dplyr::transmute(week, team = as.character(home_team), opponent = as.character(away_team), kickoff_at = .kickoff, game_final = .completed),
  schedule |> dplyr::transmute(week, team = as.character(away_team), opponent = as.character(home_team), kickoff_at = .kickoff, game_final = .completed)
) |>
  dplyr::filter(week <= completed_through, game_final %in% TRUE, nzchar(team)) |>
  dplyr::distinct(week, team, .keep_all = TRUE)

# -------------------------------------------------------------------------
# 2) Rebuild a separate strict lagged feature table through CURRENT_SEASON.
#    The canonical historical table and canonical historical raw caches remain
#    untouched because pipeline/09 is running in weekly-learning mode.
# -------------------------------------------------------------------------
old_env <- Sys.getenv(c("FM_WEEKLY_DATA_END","FM_WEEKLY_LEARNING_MODE","FM_WEEKLY_DATA_OUTPUT"), unset = NA_character_)
on.exit({
  for (nm in names(old_env)) {
    if (is.na(old_env[[nm]])) {
      Sys.unsetenv(nm)
    } else {
      do.call(Sys.setenv, stats::setNames(list(old_env[[nm]]), nm))
    }
  }
}, add = TRUE)

Sys.setenv(
  FM_WEEKLY_DATA_END = as.character(CURRENT_SEASON),
  FM_WEEKLY_LEARNING_MODE = "true",
  FM_WEEKLY_DATA_OUTPUT = strict_learning_path
)
source("pipeline/09_build_weekly_data.R", local = FALSE)
if (!file.exists(strict_learning_path)) stop("Strict weekly-learning feature build did not produce ", strict_learning_path)

strict <- readr::read_csv(strict_learning_path, show_col_types = FALSE, progress = FALSE)
strict$season <- suppressWarnings(as.integer(strict$season))
strict$week <- suppressWarnings(as.integer(strict$week))
strict$player_id <- as.character(strict$player_id)
strict$position <- toupper(as.character(strict$position))
strict$team <- as.character(strict$team)
strict_current <- strict |>
  dplyr::filter(
    season == CURRENT_SEASON,
    week >= 1L,
    week <= completed_through,
    position %in% POSITIONS,
    nzchar(player_id),
    is.finite(suppressWarnings(as.numeric(weekly_fppg)))
  ) |>
  dplyr::inner_join(
    team_kickoff |> dplyr::select(week, team, kickoff_at, game_final),
    by = c("week","team")
  ) |>
  dplyr::mutate(
    feature_source = "strict_reconstruction",
    leakage_audited = TRUE,
    pregame_captured_at = NA_character_,
    kickoff_at = format(as.POSIXct(kickoff_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

# -------------------------------------------------------------------------
# 3) Prefer exact archived pregame feature vectors where available.
#    Reconstruction remains only for weeks predating the archive.
# -------------------------------------------------------------------------
incoming <- strict_current
if (file.exists(pregame_feature_path)) {
  pf <- tryCatch(readr::read_csv(pregame_feature_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
  if (nrow(pf) && all(c("week","player_id","position","team","pregame_captured_at") %in% names(pf))) {
    if (!"season" %in% names(pf)) pf$season <- CURRENT_SEASON
    pf$season <- suppressWarnings(as.integer(pf$season))
    pf$week <- suppressWarnings(as.integer(pf$week))
    pf$player_id <- as.character(pf$player_id)
    pf$position <- toupper(as.character(pf$position))
    pf$team <- as.character(pf$team)
    pf$.captured <- suppressWarnings(as.POSIXct(pf$pregame_captured_at, tz = "UTC"))
    exact <- pf |>
      dplyr::filter(season == CURRENT_SEASON, week <= completed_through, position %in% POSITIONS) |>
      dplyr::inner_join(
        team_kickoff |> dplyr::select(week, team, kickoff_at, game_final),
        by = c("week","team")
      ) |>
      dplyr::filter(
        game_final %in% TRUE,
        is.finite(as.numeric(.captured)),
        is.finite(as.numeric(kickoff_at)),
        .captured < kickoff_at
      ) |>
      dplyr::arrange(week, player_id, .captured) |>
      dplyr::group_by(season, week, player_id, position) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()

    if (nrow(exact)) {
      actual_map <- strict_current |>
        dplyr::select(season, week, player_id, position, weekly_fppg) |>
        dplyr::distinct(season, week, player_id, position, .keep_all = TRUE)
      exact <- exact |>
        dplyr::left_join(actual_map, by = c("season","week","player_id","position")) |>
        dplyr::filter(is.finite(suppressWarnings(as.numeric(weekly_fppg)))) |>
        dplyr::mutate(
          feature_source = "pregame_archive",
          leakage_audited = TRUE,
          kickoff_at = format(as.POSIXct(kickoff_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          pregame_captured_at = format(as.POSIXct(.captured, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        ) |>
        dplyr::select(-dplyr::any_of(".captured"))

      exact_keys <- fm31_history_key(exact)
      incoming <- dplyr::bind_rows(
        exact,
        strict_current[!fm31_history_key(strict_current) %in% exact_keys, , drop = FALSE]
      )
      cat("[3.1 WEEKLY LEARN] Exact archived pregame rows available: ", nrow(exact), ".\n", sep = "")
    }
  }
}

# -------------------------------------------------------------------------
# 4) Append finalized observations to a separate immutable live ledger.
# -------------------------------------------------------------------------
existing_live <- if (file.exists(live_history_path)) {
  tryCatch(readr::read_csv(live_history_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
} else data.frame()

live_history <- fm31_append_training_history(existing_live, incoming)
if (nrow(live_history)) {
  readr::write_csv(live_history, live_history_path)
}
cat("[3.1 WEEKLY LEARN] Live training ledger rows: ", nrow(live_history), ".\n", sep = "")

# The immutable historical store is only read, never rewritten.
historical <- readr::read_csv(historical_path, show_col_types = FALSE, progress = FALSE)
historical$season <- suppressWarnings(as.integer(historical$season))
historical$week <- suppressWarnings(as.integer(historical$week))
historical$position <- toupper(as.character(historical$position))
historical$player_id <- as.character(historical$player_id)
historical <- historical |>
  dplyr::filter(season <= TRAIN_END, position %in% POSITIONS)
historical$training_weight_multiplier_31 <- 1.0

live_history$season <- suppressWarnings(as.integer(live_history$season))
live_history$week <- suppressWarnings(as.integer(live_history$week))
live_history$position <- toupper(as.character(live_history$position))
live_history$player_id <- as.character(live_history$player_id)
# Current-season outcomes matter, but cannot overwhelm a decade of history.
live_history$training_weight_multiplier_31 <- 1.5

# -------------------------------------------------------------------------
# 5) Exact production forecasts provide the incumbent for honest live OOF.
# -------------------------------------------------------------------------
if (!file.exists(pregame_projection_path)) {
  stop("Missing frozen pregame projection archive: ", pregame_projection_path)
}
base_archive <- readr::read_csv(pregame_projection_path, show_col_types = FALSE, progress = FALSE)
if (!all(c("week","player_id","position","projection") %in% names(base_archive))) {
  stop("Pregame projection archive lacks required fields.")
}
if (!"captured_at" %in% names(base_archive)) base_archive$captured_at <- ""
base_archive <- base_archive |>
  dplyr::mutate(
    week = suppressWarnings(as.integer(week)),
    player_id = as.character(player_id),
    position = toupper(as.character(position)),
    captured_num = suppressWarnings(as.numeric(as.POSIXct(captured_at, tz = "UTC"))),
    projection = suppressWarnings(as.numeric(projection))
  ) |>
  dplyr::filter(week <= completed_through, position %in% POSITIONS, is.finite(projection)) |>
  dplyr::arrange(week, player_id, captured_num) |>
  dplyr::group_by(week, player_id, position) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup() |>
  dplyr::select(week, player_id, position, production_base_31 = projection)

# Historical 3.1 config/blend strength remains the guardrail; weekly learning
# changes fitted weights, not the validated blend architecture.
hist_manifest <- readr::read_csv("output/weekly_3_1_champion_manifest.csv", show_col_types = FALSE, progress = FALSE)

starter_cut <- c(QB = 16, RB = 10, WR = 10, TE = 8)
relevant_cut <- c(QB = 12, RB = 6, WR = 6, TE = 4.5)

oof_rows <- list()
gate_rows <- list()
final_fit_rows <- list()

for (pos in POSITIONS) {
  meta_path <- paste0("models/weekly_3_1_direct_", pos, "_meta.rds")
  if (!file.exists(meta_path)) stop("Missing validated 3.1 model metadata for ", pos)
  meta <- readRDS(meta_path)
  config_row <- meta$config
  if (is.null(config_row) || !nrow(config_row)) {
    config_row <- wk31_param_grid(pos)[1, , drop = FALSE]
  }

  mf <- hist_manifest[hist_manifest$position == pos, , drop = FALSE]
  alpha <- if (nrow(mf)) suppressWarnings(as.numeric(mf$final_alpha_2026[1])) else 0
  if (!is.finite(alpha)) alpha <- 0
  alpha <- min(alpha, as.numeric(MODEL31_FINAL_ALPHA_CAP[[pos]]))

  live_pos <- live_history |>
    dplyr::filter(position == pos, week <= completed_through) |>
    dplyr::left_join(base_archive |> dplyr::filter(position == pos),
                     by = c("week","player_id","position")) |>
    dplyr::filter(is.finite(suppressWarnings(as.numeric(production_base_31))))

  weeks <- sort(unique(live_pos$week))
  for (target_week in weeks) {
    test <- live_pos |> dplyr::filter(week == target_week)
    if (!nrow(test)) next

    train_live <- live_history |>
      dplyr::filter(position == pos, week < target_week)
    train <- dplyr::bind_rows(
      historical |> dplyr::filter(position == pos),
      train_live
    )
    if (nrow(train) < 100) next

    fit <- wk31_fit(
      train, pos, config_row,
      seed = SEED + CURRENT_SEASON * 100L + target_week * 10L + match(pos, POSITIONS)
    )
    direct <- wk31_predict(fit, test)
    cand <- pmax(
      0,
      wk31_num(test$production_base_31) +
        alpha * (direct - wk31_num(test$production_base_31))
    )

    test$actual_fppg <- wk31_num(test$weekly_fppg, NA_real_)
    test$direct_fppg_31 <- direct
    test$candidate_fppg_31 <- cand
    test$blend_alpha_31 <- alpha
    test$starter_cohort <- wk31_num(test$production_base_31) >= starter_cut[[pos]]
    test$relevant_cohort <- wk31_num(test$production_base_31) >= relevant_cut[[pos]]
    test$target_week <- target_week
    test$trained_live_through_week <- target_week - 1L

    oof_rows[[length(oof_rows) + 1L]] <- test |>
      dplyr::select(
        season, week, player_id, player_display_name, position, team,
        actual_fppg, production_base_31, direct_fppg_31, candidate_fppg_31,
        blend_alpha_31, starter_cohort, relevant_cohort,
        feature_source, target_week, trained_live_through_week
      )
  }

  pos_oof <- dplyr::bind_rows(oof_rows) |>
    dplyr::filter(position == pos)
  gate <- fm31_weekly_live_gate(pos_oof, pos)
  gate$evaluated_through_week <- completed_through
  gate$historical_rows <- nrow(historical[historical$position == pos, , drop = FALSE])
  gate$live_rows <- nrow(live_history[live_history$position == pos & live_history$week <= completed_through, , drop = FALSE])
  gate$deployed_alpha <- alpha
  gate_rows[[length(gate_rows) + 1L]] <- gate

  if (isTRUE(gate$promoted[1])) {
    final_train <- dplyr::bind_rows(
      historical |> dplyr::filter(position == pos),
      live_history |> dplyr::filter(position == pos, week <= completed_through)
    )
    final_fit <- wk31_fit(
      final_train, pos, config_row,
      seed = SEED + 31999L + completed_through * 100L + match(pos, POSITIONS)
    )

    candidate_model <- paste0("models/.weekly_3_1_live_candidate_", pos, ".json")
    candidate_meta <- paste0("models/.weekly_3_1_live_candidate_", pos, "_meta.rds")
    xgboost::xgb.save(final_fit$model, candidate_model)
    saveRDS(list(
      position = pos,
      features = final_fit$features,
      config = config_row,
      nrounds = final_fit$nrounds,
      n_train = final_fit$n_train,
      trained_through_season = CURRENT_SEASON,
      trained_through_week = completed_through,
      historical_rows = gate$historical_rows[1],
      live_rows = gate$live_rows[1],
      current_season_weight_multiplier = 1.5,
      deployed_alpha = alpha,
      learning_version = learning_version,
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ), candidate_meta)
    final_fit_rows[[pos]] <- list(model = candidate_model, meta = candidate_meta)
  }
}

live_oof <- dplyr::bind_rows(oof_rows)
gates <- dplyr::bind_rows(gate_rows)
if (nrow(live_oof)) readr::write_csv(live_oof, oof_path)
readr::write_csv(gates, "output/weekly_3_1_live_promotion.csv")

# -------------------------------------------------------------------------
# 6) Promote only positions that pass. Failed weekly challengers cannot evict
#    a previously promoted live model.
# -------------------------------------------------------------------------
old_manifest <- if (file.exists(manifest_path)) {
  tryCatch(readr::read_csv(manifest_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
} else data.frame()

manifest_rows <- list()
run_stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")

for (pos in POSITIONS) {
  gate <- gates[gates$position == pos, , drop = FALSE]
  old <- if (nrow(old_manifest)) old_manifest[old_manifest$position == pos, , drop = FALSE] else data.frame()

  alias_model <- paste0("models/weekly_3_1_live_", pos, ".json")
  alias_meta <- paste0("models/weekly_3_1_live_", pos, "_meta.rds")
  already_live <- file.exists(alias_model) && file.exists(alias_meta)

  promoted_now <- nrow(gate) && isTRUE(gate$promoted[1]) && !is.null(final_fit_rows[[pos]])
  use_live <- already_live || promoted_now
  deployed_week <- if (already_live) {
    tryCatch(as.integer(readRDS(alias_meta)$trained_through_week), error = function(e) NA_integer_)
  } else NA_integer_
  deployed_alpha <- if (nrow(old) && "deployed_alpha" %in% names(old)) {
    suppressWarnings(as.numeric(old$deployed_alpha[1]))
  } else if (nrow(gate)) suppressWarnings(as.numeric(gate$deployed_alpha[1])) else 0
  if (!is.finite(deployed_alpha)) deployed_alpha <- 0

  if (promoted_now) {
    archive_dir <- file.path("models","archive","weekly_3_1_live",run_stamp)
    dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
    if (already_live) {
      file.copy(alias_model, file.path(archive_dir, basename(alias_model)), overwrite = FALSE)
      file.copy(alias_meta, file.path(archive_dir, basename(alias_meta)), overwrite = FALSE)
    }
    ok_model <- file.copy(final_fit_rows[[pos]]$model, alias_model, overwrite = TRUE)
    ok_meta <- file.copy(final_fit_rows[[pos]]$meta, alias_meta, overwrite = TRUE)
    if (!isTRUE(ok_model) || !isTRUE(ok_meta)) {
      stop("Could not atomically promote weekly live model artifacts for ", pos)
    }
    unlink(final_fit_rows[[pos]]$model)
    unlink(final_fit_rows[[pos]]$meta)
    deployed_week <- completed_through
    deployed_alpha <- suppressWarnings(as.numeric(gate$deployed_alpha[1]))
    cat("[3.1 WEEKLY LEARN] PROMOTED ", pos, " live model through Week ", completed_through, ".\n", sep = "")
  } else {
    cand_model <- paste0("models/.weekly_3_1_live_candidate_", pos, ".json")
    cand_meta <- paste0("models/.weekly_3_1_live_candidate_", pos, "_meta.rds")
    if (file.exists(cand_model)) unlink(cand_model)
    if (file.exists(cand_meta)) unlink(cand_meta)
    cat("[3.1 WEEKLY LEARN] ", pos, " challenger not promoted; incumbent retained.\n", sep = "")
  }

  manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
    position = pos,
    use_live_model = use_live,
    deployed_through_week = deployed_week,
    deployed_alpha = deployed_alpha,
    latest_challenger_promoted = if (nrow(gate)) isTRUE(gate$promoted[1]) else FALSE,
    latest_evaluated_through_week = completed_through,
    latest_all_mae_gain_pct = if (nrow(gate)) gate$all_mae_gain_pct[1] else NA_real_,
    latest_starter_mae_gain_pct = if (nrow(gate)) gate$starter_mae_gain_pct[1] else NA_real_,
    latest_week_win_rate = if (nrow(gate)) gate$week_win_rate[1] else NA_real_,
    latest_bootstrap_p_improves = if (nrow(gate)) gate$bootstrap_p_improves[1] else NA_real_,
    latest_failed_checks = if (nrow(gate)) gate$failed_checks[1] else "no_gate",
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

manifest <- dplyr::bind_rows(manifest_rows)
readr::write_csv(manifest, manifest_path)

eval_now <- gates |>
  dplyr::mutate(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    learning_version = learning_version
  )
eval_old <- if (file.exists(evaluation_path)) {
  tryCatch(readr::read_csv(evaluation_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
} else data.frame()
readr::write_csv(dplyr::bind_rows(eval_old, eval_now), evaluation_path)

if (nrow(live_oof)) {
  residual <- live_oof |>
    dplyr::mutate(residual = actual_fppg - candidate_fppg_31, abs_error = abs(residual)) |>
    dplyr::select(
      season, week, player_id, player_display_name, position,
      actual_fppg, production_base_31, direct_fppg_31, candidate_fppg_31,
      residual, abs_error, starter_cohort, relevant_cohort, feature_source
    )
  readr::write_csv(residual, residual_path)
}

summary_lines <- c(
  "FANTASY MODEL 3.1 - GUARDED WEEKLY LEARNING",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("Completed through Week: ", completed_through),
  paste0("Immutable historical rows: ", nrow(historical)),
  paste0("Append-only current-season rows: ", nrow(live_history)),
  "",
  "POSITION GATES"
)
for (i in seq_len(nrow(gates))) {
  g <- gates[i, ]
  summary_lines <- c(summary_lines, sprintf(
    "%s | %s | live OOF n=%d weeks=%d | all MAE gain %.2f%% | starter MAE gain %.2f%% | week win %.1f%% | bootstrap %.3f | %s",
    g$position,
    ifelse(g$promoted, "PROMOTE", "KEEP INCUMBENT"),
    g$n, g$weeks, g$all_mae_gain_pct, g$starter_mae_gain_pct,
    100 * g$week_win_rate, g$bootstrap_p_improves,
    ifelse(nchar(g$failed_checks), g$failed_checks, "all gates passed")
  ))
}
writeLines(summary_lines, "output/weekly_3_1_live_quality_report.txt")

cat("\n[3.1 WEEKLY LEARN] Weekly learning complete.\n")
print(gates)
