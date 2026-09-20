# ============================================================
# FANTASY MODEL 2.5 - LIVE WEEKLY ACCURACY + PREGAME ARCHIVE
# ============================================================
# Maintains one frozen final pregame forecast per player-week and scores it once
# an actual result appears. This is deliberately separate from model training.

source("config.R")
ensure_packages(c("dplyr", "readr", "nflreadr"))
source("R/weekly_engine.R")
source("R/live_refresh_engine_25.R")

dir.create("output", recursive = TRUE, showWarnings = FALSE)

score_live_accuracy25 <- function() {
  weekly_path <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
  archive_path <- paste0("output/pregame_projection_archive_", CURRENT_SEASON, ".csv")
  history_path <- paste0("output/projection_history_", CURRENT_SEASON, ".csv")
  actual_cache <- paste0("data/raw/player_weekly_stats_", CURRENT_SEASON, ".csv")

  if (!file.exists(weekly_path)) stop("Missing live weekly projections: ", weekly_path)
  weekly <- readr::read_csv(weekly_path, show_col_types = FALSE, progress = FALSE)

  safe_char <- c("captured_at", "model_version", "player_id", "player_display_name", "position", "team", "opponent", "projection_confidence", "matchup_grade")
  normalize_archive_types <- function(d) {
    if (!nrow(d)) return(d)
    for (nm in intersect(safe_char, names(d))) d[[nm]] <- as.character(d[[nm]])
    d
  }
  ensure_col <- function(d, nm, default = NA) {
    if (!nm %in% names(d)) d[[nm]] <- rep(default, nrow(d))
    d
  }

  # -------------------------------------------------------------------------
  # 1) Audit/recover the frozen pregame archive using actual kickoff times.
  # -------------------------------------------------------------------------
  archive <- if (file.exists(archive_path)) tryCatch(
    readr::read_csv(archive_path, show_col_types = FALSE, progress = FALSE),
    error = function(e) data.frame()
  ) else data.frame()
  archive <- normalize_archive_types(archive)

  schedule_live_path <- paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv")
  parse_kickoff25 <- function(gameday, gametime) {
    gd <- live25_chr(gameday)
    gt <- live25_chr(gametime)
    gt[!nzchar(gt)] <- "00:00:00"
    short <- grepl("^\\d{1,2}:\\d{2}$", gt)
    gt[short] <- paste0(gt[short], ":00")
    as.POSIXct(paste(gd, gt), format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
  }
  parse_capture25 <- function(x) {
    raw <- live25_chr(x)
    out <- suppressWarnings(as.POSIXct(raw, format = "%Y-%m-%d %H:%M:%S %z", tz = "UTC"))
    bad <- !is.finite(as.numeric(out))
    if (any(bad)) {
      alt <- suppressWarnings(as.POSIXct(raw[bad], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
      out[bad] <- alt
    }
    out
  }

  schedule_team <- data.frame()
  if (file.exists(schedule_live_path)) {
    ss_all <- tryCatch(
      readr::read_csv(schedule_live_path, show_col_types = FALSE, progress = FALSE),
      error = function(e) data.frame()
    )
    if (nrow(ss_all) && all(c("week", "home_team", "away_team", "gameday") %in% names(ss_all))) {
      if (!"gametime" %in% names(ss_all)) ss_all$gametime <- "00:00:00"
      completed_mask <- live25_completed_mask(ss_all)
      ss_all <- ss_all |>
        dplyr::mutate(
          week = as.integer(live25_num(week)),
          .kickoff = parse_kickoff25(gameday, gametime),
          .completed = completed_mask
        ) |>
        dplyr::filter(is.finite(week), is.finite(as.numeric(.kickoff)))
      schedule_team <- dplyr::bind_rows(
        ss_all |> dplyr::transmute(
          week, team = live25_chr(home_team), kickoff = .kickoff,
          game_completed = .completed
        ),
        ss_all |> dplyr::transmute(
          week, team = live25_chr(away_team), kickoff = .kickoff,
          game_completed = .completed
        )
      ) |>
        dplyr::filter(nzchar(team)) |>
        dplyr::distinct(week, team, .keep_all = TRUE)
    }
  }

  # Existing rows are allowed into accuracy evaluation only when we can prove
  # their capture timestamp preceded that player's kickoff. This removes stale
  # postgame rows created by older active-week logic instead of scoring them.
  if (nrow(archive) && nrow(schedule_team) &&
      all(c("week", "team", "captured_at") %in% names(archive))) {
    original_names <- names(archive)
    audited <- archive |>
      dplyr::mutate(
        week = as.integer(live25_num(week)),
        team = live25_chr(team),
        .captured_time = parse_capture25(captured_at)
      ) |>
      dplyr::left_join(schedule_team, by = c("week", "team"))
    honest <- is.finite(as.numeric(audited$.captured_time)) &
      is.finite(as.numeric(audited$kickoff)) &
      audited$.captured_time < audited$kickoff
    removed <- sum(!honest, na.rm = TRUE)
    archive <- audited[honest, original_names, drop = FALSE]
    archive <- normalize_archive_types(archive)
    if (removed > 0) {
      cat("[2.5 LIVE SCORE] Removed ", removed,
          " archive rows without a provable pre-kickoff timestamp.\n", sep = "")
    }
  }

  # Recover a missing most-recent started week from timestamped projection
  # history. This is strict: rows without both a valid capture time and kickoff
  # are rejected. We never reconstruct a forecast from postgame/current values.
  recent_started_week <- NA_integer_
  if (nrow(schedule_team)) {
    now_num <- as.numeric(Sys.time())
    started_weeks <- schedule_team$week[
      is.finite(as.numeric(schedule_team$kickoff)) &
        as.numeric(schedule_team$kickoff) <= now_num
    ]
    if (length(started_weeks)) recent_started_week <- max(started_weeks, na.rm = TRUE)
  }
  archive_weeks <- if (nrow(archive) && "week" %in% names(archive)) {
    as.integer(live25_num(archive$week))
  } else integer()
  need_history_recovery <- !nrow(archive) ||
    (is.finite(recent_started_week) && !any(archive_weeks == recent_started_week, na.rm = TRUE))

  if (need_history_recovery && file.exists(history_path) && nrow(schedule_team)) {
    hist <- tryCatch(
      readr::read_csv(history_path, show_col_types = FALSE, progress = FALSE),
      error = function(e) data.frame()
    )
    if (nrow(hist) && all(c("week", "player_id", "projected_weekly_fppg") %in% names(hist))) {
      for (nm in c(
        "generated_at", "model_version", "player_display_name", "position",
        "team", "opponent", "weekly_floor", "weekly_ceiling",
        "expected_abs_error", "projection_confidence", "weekly_position_rank",
        "matchup_grade", "projected_weekly_fppg_232", "incumbent_fppg_25"
      )) {
        hist <- ensure_col(
          hist, nm,
          if (nm %in% c(
            "generated_at", "model_version", "player_display_name", "position",
            "team", "opponent", "projection_confidence", "matchup_grade"
          )) "" else NA_real_
        )
      }

      hist <- hist |>
        dplyr::mutate(
          week = as.integer(live25_num(week)),
          player_id = live25_chr(player_id),
          generated_at = live25_chr(generated_at),
          model_version = live25_chr(model_version),
          player_display_name = live25_chr(player_display_name),
          position = live25_chr(position),
          team = live25_chr(team),
          opponent = live25_chr(opponent),
          projection_confidence = live25_chr(projection_confidence),
          matchup_grade = live25_chr(matchup_grade),
          .captured_time = parse_capture25(generated_at)
        ) |>
        dplyr::filter(position %in% POSITIONS, nzchar(player_id), nzchar(team), is.finite(week)) |>
        dplyr::left_join(schedule_team, by = c("week", "team")) |>
        dplyr::filter(
          is.finite(as.numeric(.captured_time)),
          is.finite(as.numeric(kickoff)),
          .captured_time < kickoff
        ) |>
        dplyr::arrange(player_id, week, .captured_time) |>
        dplyr::group_by(player_id, week) |>
        dplyr::slice_tail(n = 1) |>
        dplyr::ungroup()

      recovered <- hist |>
        dplyr::transmute(
          captured_at = generated_at, model_version, week, player_id,
          player_display_name, position, team, opponent,
          projection = live25_num(projected_weekly_fppg),
          incumbent_projection = dplyr::coalesce(
            live25_num(incumbent_fppg_25), live25_num(projected_weekly_fppg)
          ),
          controller_projection_232 = live25_num(projected_weekly_fppg_232),
          floor = live25_num(weekly_floor),
          ceiling = live25_num(weekly_ceiling),
          expected_abs_error = live25_num(expected_abs_error),
          projection_confidence,
          weekly_position_rank = live25_num(weekly_position_rank),
          matchup_grade
        ) |>
        dplyr::filter(is.finite(projection))

      if (nrow(archive) && nrow(recovered)) {
        recovered <- recovered |>
          dplyr::anti_join(
            archive |> dplyr::transmute(
              week = as.integer(live25_num(week)),
              player_id = live25_chr(player_id)
            ),
            by = c("week", "player_id")
          )
      }
      if (nrow(recovered)) {
        archive <- dplyr::bind_rows(
          normalize_archive_types(archive),
          normalize_archive_types(recovered)
        ) |>
          dplyr::arrange(week, position, weekly_position_rank, player_display_name)
        cat("[2.5 LIVE SCORE] Recovered ", nrow(recovered),
            " honest pregame rows from projection history.\n", sep = "")
      }
    }
  }

  # -------------------------------------------------------------------------
  # 2) Update ONLY currently-unplayed players in the next active week.
  # -------------------------------------------------------------------------
  for (nm in c("week", "player_id", "player_display_name", "position", "team", "opponent", "projected_weekly_fppg")) weekly <- ensure_col(weekly, nm, NA)
  for (nm in c("is_actual", "weekly_floor", "weekly_ceiling", "expected_abs_error", "weekly_position_rank", "projected_weekly_fppg_232", "incumbent_fppg_25")) weekly <- ensure_col(weekly, nm, NA_real_)
  for (nm in c("projection_confidence", "matchup_grade")) weekly <- ensure_col(weekly, nm, "")

  unplayed <- weekly |>
    dplyr::mutate(.week_num = as.integer(live25_num(week)), .actual_num = live25_num(is_actual, 0)) |>
    dplyr::filter(position %in% POSITIONS, .actual_num == 0, is.finite(.week_num))

  if (nrow(unplayed)) {
    active_week <- NA_integer_
    if (nrow(schedule_team)) {
      future_weeks <- schedule_team$week[
        is.finite(as.numeric(schedule_team$kickoff)) &
          as.numeric(schedule_team$kickoff) > as.numeric(Sys.time())
      ]
      if (length(future_weeks)) active_week <- min(future_weeks, na.rm = TRUE)
    }
    if (!is.finite(active_week)) active_week <- min(unplayed$.week_num, na.rm = TRUE)

    active <- unplayed |> dplyr::filter(.week_num == active_week)
    if (nrow(schedule_team)) {
      active <- active |>
        dplyr::left_join(
          schedule_team |> dplyr::rename(.kickoff = kickoff),
          by = c(".week_num" = "week", "team")
        ) |>
        dplyr::filter(
          !is.finite(as.numeric(.kickoff)) |
            as.numeric(.kickoff) > as.numeric(Sys.time())
        ) |>
        dplyr::select(-.kickoff)
    }

    # Never overwrite a pregame archive after a game has started. The automatic
    # poll may run during a live window because schedule scores can change before
    # player-week box scores are published. A finite score freezes that team-week.
    schedule_live_path <- paste0("data/raw/schedules_live_", CURRENT_SEASON, ".csv")
    if (file.exists(schedule_live_path)) {
      ss <- tryCatch(readr::read_csv(schedule_live_path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
      if (nrow(ss) && all(c("week", "home_team", "away_team") %in% names(ss))) {
        if (!"home_score" %in% names(ss)) ss$home_score <- NA_real_
        if (!"away_score" %in% names(ss)) ss$away_score <- NA_real_
        started <- ss |>
          dplyr::mutate(.week_num = as.integer(live25_num(week)), .started = is.finite(live25_num(home_score)) & is.finite(live25_num(away_score))) |>
          dplyr::filter(.week_num == active_week, .started) |>
          dplyr::transmute(.week_num, home_team = live25_chr(home_team), away_team = live25_chr(away_team))
        if (nrow(started)) {
          started_teams <- unique(c(started$home_team, started$away_team))
          active <- active |> dplyr::filter(!live25_chr(team) %in% started_teams)
        }
      }
    }

    now_rows <- active |>
      dplyr::transmute(
        captured_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), model_version = "2.5",
        week = .week_num, player_id = live25_chr(player_id), player_display_name = live25_chr(player_display_name),
        position = live25_chr(position), team = live25_chr(team), opponent = live25_chr(opponent),
        projection = live25_num(projected_weekly_fppg),
        incumbent_projection = dplyr::coalesce(live25_num(incumbent_fppg_25), live25_num(projected_weekly_fppg)),
        controller_projection_232 = live25_num(projected_weekly_fppg_232),
        floor = live25_num(weekly_floor), ceiling = live25_num(weekly_ceiling),
        expected_abs_error = live25_num(expected_abs_error), projection_confidence = live25_chr(projection_confidence),
        weekly_position_rank = live25_num(weekly_position_rank), matchup_grade = live25_chr(matchup_grade)
      ) |>
      dplyr::filter(nzchar(player_id), is.finite(projection))

    if (nrow(now_rows)) {
      if (nrow(archive)) archive <- archive |> dplyr::filter(!(week == active_week & player_id %in% now_rows$player_id))
      archive <- dplyr::bind_rows(normalize_archive_types(archive), normalize_archive_types(now_rows)) |>
        dplyr::arrange(week, position, weekly_position_rank, player_display_name)
      readr::write_csv(archive, archive_path)
      cat("[2.5 LIVE SCORE] Pregame archive updated for Week ", active_week, ": ", nrow(now_rows), " unplayed players.\n", sep = "")
    }
  }

  if (nrow(archive)) {
    readr::write_csv(normalize_archive_types(archive), archive_path)
  }

  if (!nrow(archive)) {
    cat("[2.5 LIVE SCORE] No pregame archive rows yet; accuracy tracking will begin after a pregame forecast is captured.\n")
    return(invisible(NULL))
  }
  archive <- ensure_col(archive, "incumbent_projection", NA_real_)
  archive <- ensure_col(archive, "controller_projection_232", NA_real_)

  # -------------------------------------------------------------------------
  # 3) Load actual current-season player-week outcomes under model scoring.
  # -------------------------------------------------------------------------
  raw_actual <- if (file.exists(actual_cache)) tryCatch(readr::read_csv(actual_cache, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
  if (!nrow(raw_actual)) raw_actual <- tryCatch(nflreadr::load_player_stats(seasons = CURRENT_SEASON, summary_level = "week"), error = function(e) data.frame())
  actual <- normalize_weekly_stats21(raw_actual)
  if (!nrow(actual)) {
    cat("[2.5 LIVE SCORE] No completed current-season player-week stats yet.\n")
    return(invisible(NULL))
  }

  actual <- actual |>
    dplyr::filter(position %in% POSITIONS, is.finite(live25_num(week)), nzchar(live25_chr(player_id))) |>
    dplyr::transmute(
      week = as.integer(live25_num(week)), player_id = live25_chr(player_id),
      player_display_name_actual = live25_chr(player_display_name), position_actual = live25_chr(position),
      team_actual = live25_chr(team), actual_fppg = live25_num(weekly_fppg)
    ) |>
    dplyr::filter(is.finite(actual_fppg)) |>
    dplyr::distinct(week, player_id, .keep_all = TRUE)

  # Accuracy is final-game only. Weekly stat feeds can expose partial rows while
  # a game is in progress, so never score a player-week until the schedule marks
  # that player's team game completed.
  if (!nrow(schedule_team) || !"game_completed" %in% names(schedule_team)) {
    cat("[2.5 LIVE SCORE] Completion schedule unavailable; preserving prior accuracy outputs.\n")
    return(invisible(NULL))
  }
  actual <- actual |>
    dplyr::inner_join(
      schedule_team |>
        dplyr::select(week, team = team, game_completed) |>
        dplyr::distinct(week, team, .keep_all = TRUE),
      by = c("week", "team_actual" = "team")
    ) |>
    dplyr::filter(isTRUE(game_completed) | game_completed == TRUE) |>
    dplyr::select(-game_completed)

  if (!nrow(actual)) {
    cat("[2.5 LIVE SCORE] No finalized player-week outcomes are available yet.\n")
    return(invisible(NULL))
  }

  scored <- normalize_archive_types(archive) |>
    dplyr::inner_join(actual, by = c("week", "player_id")) |>
    dplyr::mutate(
      player_display_name = dplyr::if_else(nzchar(player_display_name), player_display_name, player_display_name_actual),
      position = dplyr::if_else(nzchar(position), position, position_actual),
      team = dplyr::if_else(nzchar(team), team, team_actual),
      projection = live25_num(projection), incumbent_projection = live25_num(incumbent_projection), actual_fppg = live25_num(actual_fppg),
      floor = live25_num(floor), ceiling = live25_num(ceiling), expected_abs_error = live25_num(expected_abs_error),
      error = actual_fppg - projection, abs_error = abs(error), sq_error = error^2,
      incumbent_error = actual_fppg - incumbent_projection, incumbent_abs_error = abs(incumbent_error), incumbent_sq_error = incumbent_error^2
    ) |>
    dplyr::filter(position %in% POSITIONS, is.finite(projection), is.finite(actual_fppg))

  if (!nrow(scored)) {
    cat("[2.5 LIVE SCORE] Actual stats exist, but no completed player-weeks have an archived pregame forecast yet.\n")
    return(invisible(NULL))
  }

  scored <- scored |>
    dplyr::group_by(week, position) |>
    dplyr::arrange(dplyr::desc(projection), .by_group = TRUE) |>
    dplyr::mutate(projected_position_rank = dplyr::row_number()) |>
    dplyr::arrange(dplyr::desc(actual_fppg), .by_group = TRUE) |>
    dplyr::mutate(actual_position_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      starter_cutoff = as.numeric(WEEKLY_22_STARTER_CUTOFF[position]),
      starter_cohort = projected_position_rank <= starter_cutoff,
      relevant_cohort = projected_position_rank <= pmax(starter_cutoff * 2, starter_cutoff + 6),
      within_interval = is.finite(floor) & is.finite(ceiling) & actual_fppg >= floor & actual_fppg <= ceiling,
      within_expected_error = is.finite(expected_abs_error) & abs_error <= expected_abs_error
    )

  safe_cor <- function(a, b, method = "pearson") {
    keep <- is.finite(a) & is.finite(b)
    if (sum(keep) < 4 || stats::sd(a[keep]) == 0 || stats::sd(b[keep]) == 0) return(NA_real_)
    suppressWarnings(stats::cor(a[keep], b[keep], method = method))
  }
  pair_accuracy_one <- function(d) {
    d <- d[d$relevant_cohort %in% TRUE, , drop = FALSE]
    if (nrow(d) < 2) return(c(n_pairs = 0, correct = 0))
    cmb <- utils::combn(seq_len(nrow(d)), 2)
    pred_diff <- d$projection[cmb[1, ]] - d$projection[cmb[2, ]]
    act_diff <- d$actual_fppg[cmb[1, ]] - d$actual_fppg[cmb[2, ]]
    # Treat this as a real start/sit metric, not an easy all-pairs ranking stat:
    # score only relevant-player decisions where the model saw a plausible choice.
    keep <- is.finite(pred_diff) & is.finite(act_diff) & abs(pred_diff) >= 0.25 & abs(pred_diff) <= 5 & abs(act_diff) > 1e-9
    if (!any(keep)) return(c(n_pairs = 0, correct = 0))
    c(n_pairs = sum(keep), correct = sum(sign(pred_diff[keep]) == sign(act_diff[keep])))
  }
  pair_accuracy <- function(d) {
    pieces <- split(d, d$position)
    z <- lapply(pieces, pair_accuracy_one)
    n <- sum(vapply(z, function(x) x[["n_pairs"]], numeric(1)))
    correct <- sum(vapply(z, function(x) x[["correct"]], numeric(1)))
    c(n_pairs = n, accuracy = if (n > 0) correct / n else NA_real_)
  }
  summarise_accuracy <- function(d) {
    pairs <- pair_accuracy(d)
    valid_interval <- is.finite(d$floor) & is.finite(d$ceiling)
    valid_expected <- is.finite(d$expected_abs_error)
    incumbent_ok <- is.finite(d$incumbent_projection)
    inc_mae <- if (any(incumbent_ok)) mean(d$incumbent_abs_error[incumbent_ok], na.rm = TRUE) else NA_real_
    inc_rmse <- if (any(incumbent_ok)) sqrt(mean(d$incumbent_sq_error[incumbent_ok], na.rm = TRUE)) else NA_real_
    starter_inc_ok <- incumbent_ok & d$starter_cohort %in% TRUE
    inc_starter_mae <- if (any(starter_inc_ok)) mean(d$incumbent_abs_error[starter_inc_ok], na.rm = TRUE) else NA_real_
    model_mae <- mean(d$abs_error, na.rm = TRUE)
    model_rmse <- sqrt(mean(d$sq_error, na.rm = TRUE))
    model_starter_mae <- if (any(d$starter_cohort, na.rm = TRUE)) mean(d$abs_error[d$starter_cohort], na.rm = TRUE) else NA_real_
    data.frame(
      n = nrow(d), mae = model_mae, rmse = model_rmse, bias = mean(d$error, na.rm = TRUE),
      incumbent_mae = inc_mae, incumbent_rmse = inc_rmse,
      mae_improvement_pct = if (is.finite(inc_mae) && inc_mae > 0) 100 * (inc_mae - model_mae) / inc_mae else NA_real_,
      correlation = safe_cor(d$projection, d$actual_fppg), rank_correlation = safe_cor(d$projection, d$actual_fppg, "spearman"),
      starter_n = sum(d$starter_cohort, na.rm = TRUE),
      starter_mae = model_starter_mae, incumbent_starter_mae = inc_starter_mae,
      starter_mae_improvement_pct = if (is.finite(inc_starter_mae) && inc_starter_mae > 0 && is.finite(model_starter_mae)) 100 * (inc_starter_mae - model_starter_mae) / inc_starter_mae else NA_real_,
      relevant_mae = if (any(d$relevant_cohort, na.rm = TRUE)) mean(d$abs_error[d$relevant_cohort], na.rm = TRUE) else NA_real_,
      interval_coverage = if (any(valid_interval)) mean(d$within_interval[valid_interval], na.rm = TRUE) else NA_real_,
      expected_error_coverage = if (any(valid_expected)) mean(d$within_expected_error[valid_expected], na.rm = TRUE) else NA_real_,
      start_sit_pairs = as.integer(pairs[["n_pairs"]]), start_sit_accuracy = as.numeric(pairs[["accuracy"]])
    )
  }

  weekly_summary <- dplyr::bind_rows(lapply(split(scored, scored$week), function(d) cbind(data.frame(week = as.integer(d$week[1])), summarise_accuracy(d)))) |> dplyr::arrange(week)
  weekly_position <- dplyr::bind_rows(lapply(split(scored, interaction(scored$week, scored$position, drop = TRUE)), function(d) cbind(data.frame(week = as.integer(d$week[1]), position = as.character(d$position[1])), summarise_accuracy(d)))) |> dplyr::arrange(week, factor(position, levels = POSITIONS))
  cumulative_position <- dplyr::bind_rows(lapply(split(scored, scored$position), function(d) cbind(data.frame(position = as.character(d$position[1])), summarise_accuracy(d)))) |> dplyr::arrange(factor(position, levels = POSITIONS))
  cumulative_summary <- cbind(data.frame(season = CURRENT_SEASON), summarise_accuracy(scored))
  misses <- scored |> dplyr::arrange(dplyr::desc(abs_error)) |> dplyr::select(week, player_id, player_display_name, position, team, opponent, projection, incumbent_projection, actual_fppg, error, abs_error, incumbent_error, incumbent_abs_error, floor, ceiling, expected_abs_error, projection_confidence, starter_cohort, relevant_cohort) |> dplyr::slice_head(n = 50)

  readr::write_csv(scored, paste0("output/live_accuracy_player_weeks_", CURRENT_SEASON, ".csv"))
  readr::write_csv(weekly_summary, paste0("output/live_accuracy_weekly_summary_", CURRENT_SEASON, ".csv"))
  readr::write_csv(weekly_position, paste0("output/live_accuracy_weekly_position_", CURRENT_SEASON, ".csv"))
  readr::write_csv(cumulative_position, paste0("output/live_accuracy_cumulative_position_", CURRENT_SEASON, ".csv"))
  readr::write_csv(cumulative_summary, paste0("output/live_accuracy_cumulative_summary_", CURRENT_SEASON, ".csv"))
  readr::write_csv(misses, paste0("output/live_accuracy_biggest_misses_", CURRENT_SEASON, ".csv"))

  cat("\n[2.5 LIVE SCORE] Completed player-weeks scored: ", nrow(scored), "\n", sep = "")
  print(weekly_summary)
  invisible(list(scored = scored, weekly = weekly_summary, by_position = weekly_position))
}

score_live_accuracy25()
