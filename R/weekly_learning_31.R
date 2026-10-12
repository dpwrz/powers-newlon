# ============================================================
# FANTASY MODEL 3.1 - WEEKLY ONLINE LEARNING SUPPORT
# ============================================================
# Historical training data remains immutable. Current-season learning uses:
#   1) frozen pre-kickoff feature snapshots,
#   2) an append-only actual-result revision ledger,
#   3) a materialized latest-view table for fitting.
#
# No function in this file rewrites data/processed/weekly_model_table_2_3.csv.

if (!exists("MODEL31_SAFE_FEATURES")) source("R/weekly_engine_31.R")

wl31_chr <- function(x, default = "") {
  y <- as.character(x)
  y[is.na(y)] <- default
  y
}

wl31_num <- function(x, default = NA_real_) {
  y <- suppressWarnings(as.numeric(x))
  if (!is.na(default)) y[!is.finite(y)] <- default
  y
}

wl31_bool <- function(x, default = FALSE) {
  if (is.logical(x)) {
    x[is.na(x)] <- default
    return(x)
  }
  z <- toupper(trimws(as.character(x)))
  out <- z %in% c("TRUE","T","1","YES","Y")
  out[is.na(z) | !nzchar(z)] <- default
  out
}

wl31_read_csv <- function(path) {
  if (!file.exists(path)) return(data.frame())
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) data.frame()
  )
}

wl31_atomic_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".csv")
  readr::write_csv(x, tmp, na = "")
  if (!file.rename(tmp, path)) {
    if (!file.copy(tmp, path, overwrite = TRUE)) stop("Could not write ", path)
    unlink(tmp)
  }
  invisible(path)
}

wl31_file_md5 <- function(path) {
  if (!file.exists(path)) return("")
  unname(as.character(tools::md5sum(path)))
}

wl31_schema_hash <- function(features = MODEL31_SAFE_FEATURES) {
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf), add = TRUE)
  writeLines(sort(unique(as.character(features))), tf)
  unname(as.character(tools::md5sum(tf)))
}

wl31_completed_schedule_weeks <- function(schedule, now = Sys.time(),
                                            post_kickoff_hours = 6) {
  if (is.null(schedule) || !nrow(schedule) || !"week" %in% names(schedule)) {
    return(integer())
  }
  s <- schedule
  s$week <- suppressWarnings(as.integer(s$week))

  # Preferred proof: an explicit finalized result for every game in the week.
  result <- if ("result" %in% names(s)) {
    suppressWarnings(as.numeric(s$result))
  } else {
    rep(NA_real_, nrow(s))
  }
  s$.result_final <- is.finite(result)

  # Fallback for schedule feeds that never populate result/score fields.
  # nflverse gametime is Eastern. Six hours after the latest kickoff is a
  # deliberately conservative completion boundary for the weekly Tuesday job.
  if ("kickoff" %in% names(s)) {
    kick <- suppressWarnings(as.POSIXct(s$kickoff, tz = "America/New_York"))
  } else if (all(c("gameday","gametime") %in% names(s))) {
    gd <- wl31_chr(s$gameday)
    gt <- wl31_chr(s$gametime, "00:00:00")
    gt[!nzchar(gt)] <- "00:00:00"
    short <- grepl("^\\d{1,2}:\\d{2}$", gt)
    gt[short] <- paste0(gt[short], ":00")
    kick <- suppressWarnings(as.POSIXct(
      paste(gd, gt),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "America/New_York"
    ))
  } else {
    kick <- as.POSIXct(rep(NA_character_, nrow(s)), tz = "America/New_York")
  }
  s$.kickoff <- kick

  now_num <- as.numeric(as.POSIXct(now, tz = "UTC"))
  buffer_sec <- max(0, suppressWarnings(as.numeric(post_kickoff_hours))) * 3600
  if (!is.finite(buffer_sec)) buffer_sec <- 6 * 3600

  z <- s |>
    dplyr::filter(is.finite(week)) |>
    dplyr::group_by(week) |>
    dplyr::summarise(
      game_rows = dplyr::n(),
      result_all_final = game_rows > 0 && all(.result_final),
      kickoff_rows = sum(is.finite(as.numeric(.kickoff))),
      latest_kickoff = if (kickoff_rows == game_rows && game_rows > 0) {
        max(as.numeric(.kickoff), na.rm = TRUE)
      } else {
        NA_real_
      },
      time_safely_complete = is.finite(latest_kickoff) &&
        now_num >= latest_kickoff + buffer_sec,
      .groups = "drop"
    ) |>
    dplyr::filter(result_all_final | time_safely_complete)

  sort(unique(as.integer(z$week)))
}

wl31_normalize_bind_types <- function(old, new) {
  char_cols <- c(
    "captured_at_utc","kickoff_utc","player_id","player_display_name",
    "position","team","opponent","projection_model_version",
    "feature_schema_hash","ledgered_at_utc","revision_id"
  )
  for (nm in intersect(char_cols, names(old))) old[[nm]] <- as.character(old[[nm]])
  for (nm in intersect(char_cols, names(new))) new[[nm]] <- as.character(new[[nm]])
  list(old = old, new = new)
}

wl31_active_week <- function(schedule_team, now = Sys.time()) {
  if (is.null(schedule_team) || !nrow(schedule_team) ||
      !all(c("week","kickoff") %in% names(schedule_team))) return(NA_integer_)
  now_num <- as.numeric(as.POSIXct(now, tz = "UTC"))
  w <- suppressWarnings(as.integer(schedule_team$week[
    is.finite(as.numeric(schedule_team$kickoff)) &
      as.numeric(schedule_team$kickoff) > now_num
  ]))
  w <- w[is.finite(w)]
  if (!length(w)) NA_integer_ else min(w)
}

wl31_capture_features <- function(
    weekly,
    schedule_team,
    captured_at = Sys.time(),
    season = CURRENT_SEASON,
    archive_path = file.path("data","processed",
                             paste0("weekly_learning_features_", season, ".csv")),
    status_path = file.path("output",
                            paste0("weekly_learning_capture_status_", season, ".csv"))) {

  if (is.null(weekly) || !nrow(weekly)) return(invisible(data.frame()))
  # Recompute the three 3.1 derived inputs from their lagged source columns so
  # older honest snapshots remain usable even if they did not persist them.
  weekly <- wk31_add_features(weekly)
  req <- c("week","player_id","position","team","projected_weekly_fppg")
  miss <- setdiff(req, names(weekly))
  if (length(miss)) stop("Weekly learning capture missing required column(s): ",
                         paste(miss, collapse = ", "))

  feature_missing <- setdiff(MODEL31_SAFE_FEATURES, names(weekly))
  if (length(feature_missing)) {
    stop("Weekly learning capture missing 3.1 safe feature(s): ",
         paste(feature_missing, collapse = ", "))
  }

  active_week <- wl31_active_week(schedule_team, captured_at)
  if (!is.finite(active_week)) {
    status <- data.frame(
      captured_at_utc = format(captured_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      season = season, week = NA_integer_, rows_captured = 0L,
      archive_rows = nrow(wl31_read_csv(archive_path)),
      status = "no_future_week", stringsAsFactors = FALSE
    )
    wl31_atomic_write_csv(status, status_path)
    return(invisible(data.frame()))
  }

  now_num <- as.numeric(as.POSIXct(captured_at, tz = "UTC"))
  fs <- schedule_team
  if (!"score_started" %in% names(fs)) fs$score_started <- FALSE
  if (!"opponent" %in% names(fs)) fs$opponent <- ""
  fs <- fs |>
    dplyr::mutate(
      week = suppressWarnings(as.integer(week)),
      team = wl31_chr(team),
      opponent = wl31_chr(opponent),
      score_started = wl31_bool(score_started)
    ) |>
    dplyr::filter(
      week == active_week,
      nzchar(team),
      is.finite(as.numeric(kickoff)),
      as.numeric(kickoff) > now_num,
      !score_started
    ) |>
    dplyr::distinct(week, team, .keep_all = TRUE)

  if (!nrow(fs)) {
    status <- data.frame(
      captured_at_utc = format(captured_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      season = season, week = active_week, rows_captured = 0L,
      archive_rows = nrow(wl31_read_csv(archive_path)),
      status = "no_pregame_teams", stringsAsFactors = FALSE
    )
    wl31_atomic_write_csv(status, status_path)
    return(invisible(data.frame()))
  }

  w <- weekly
  if (!"season" %in% names(w)) w$season <- season
  if (!"is_actual" %in% names(w)) w$is_actual <- 0
  if (!"player_display_name" %in% names(w)) w$player_display_name <- ""
  if (!"opponent" %in% names(w)) w$opponent <- ""
  if (!"production_base_31" %in% names(w)) w$production_base_31 <- w$projected_weekly_fppg
  if (!"blend_alpha_31" %in% names(w)) w$blend_alpha_31 <- 0
  if (!"model31_promoted" %in% names(w)) w$model31_promoted <- FALSE
  if (!"availability_factor" %in% names(w)) w$availability_factor <- 1

  schema_hash <- wl31_schema_hash()
  cols <- unique(c(
    "season","week","player_id","player_display_name","position","team","opponent",
    "production_base_31","projected_weekly_fppg","blend_alpha_31",
    "model31_promoted","availability_factor",
    MODEL31_SAFE_FEATURES
  ))

  cand <- w |>
    dplyr::mutate(
      season = suppressWarnings(as.integer(season)),
      week = suppressWarnings(as.integer(week)),
      player_id = wl31_chr(player_id),
      player_display_name = wl31_chr(player_display_name),
      position = toupper(wl31_chr(position)),
      team = wl31_chr(team),
      opponent = wl31_chr(opponent),
      is_actual = wl31_num(is_actual, 0)
    ) |>
    dplyr::filter(
      season == !!season,
      week == active_week,
      is_actual == 0,
      position %in% POSITIONS,
      nzchar(player_id),
      team %in% fs$team
    ) |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::left_join(
      fs |> dplyr::select(week, team, schedule_opponent = opponent, kickoff),
      by = c("week","team"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      opponent = dplyr::if_else(
        nzchar(trimws(wl31_chr(opponent))),
        wl31_chr(opponent),
        wl31_chr(schedule_opponent)
      ),
      captured_at_utc = format(captured_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      kickoff_utc = format(as.POSIXct(kickoff, origin = "1970-01-01", tz = "UTC"),
                           "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      production_projection = wl31_num(projected_weekly_fppg),
      production_base_31 = wl31_num(production_base_31),
      blend_alpha_31 = wl31_num(blend_alpha_31, 0),
      model31_promoted = wl31_bool(model31_promoted),
      availability_factor = wl31_num(availability_factor, 1),
      projection_model_version = "3.1",
      feature_schema_hash = schema_hash
    ) |>
    dplyr::select(
      captured_at_utc, kickoff_utc, projection_model_version,
      feature_schema_hash, season, week, player_id, player_display_name,
      position, team, opponent, production_projection, production_base_31,
      blend_alpha_31, model31_promoted, availability_factor,
      dplyr::all_of(MODEL31_SAFE_FEATURES)
    ) |>
    dplyr::distinct(season, week, player_id, .keep_all = TRUE)

  old <- wl31_read_csv(archive_path)
  if (nrow(cand)) {
    if (nrow(old)) {
      key_old <- paste(old$season, old$week, old$player_id, sep = "::")
      key_new <- paste(cand$season, cand$week, cand$player_id, sep = "::")
      old <- old[!key_old %in% key_new, , drop = FALSE]
      zz <- wl31_normalize_bind_types(old, cand)
      old <- zz$old; cand <- zz$new
    }
    archive <- dplyr::bind_rows(old, cand) |>
      dplyr::arrange(season, week, position, player_display_name)
    wl31_atomic_write_csv(archive, archive_path)
  } else {
    archive <- old
  }

  status <- data.frame(
    captured_at_utc = format(captured_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    season = season,
    week = active_week,
    rows_captured = nrow(cand),
    archive_rows = nrow(archive),
    status = if (nrow(cand)) "ok" else "no_rows",
    stringsAsFactors = FALSE
  )
  wl31_atomic_write_csv(status, status_path)

  cat("[3.1 LEARNING] Frozen pregame feature rows: ", nrow(cand),
      " | active week=", active_week,
      " | archive rows=", nrow(archive), "\n", sep = "")
  invisible(cand)
}

wl31_update_ledger <- function(
    feature_path,
    accuracy_path,
    ledger_path,
    current_path,
    season = CURRENT_SEASON,
    current_season_weight = 1.5) {

  features <- wl31_read_csv(feature_path)
  actual <- wl31_read_csv(accuracy_path)
  old <- wl31_read_csv(ledger_path)
  if (!nrow(features) || !nrow(actual)) {
    if (nrow(old)) {
      cur <- wl31_current_view(old)
      wl31_atomic_write_csv(cur, current_path)
      return(cur)
    }
    return(data.frame())
  }

  reqf <- c("season","week","player_id","position","production_projection",
            "production_base_31","captured_at_utc","feature_schema_hash",
            MODEL31_SAFE_FEATURES)
  missa <- setdiff(c("week","player_id","actual_fppg","projection"), names(actual))
  missf <- setdiff(reqf, names(features))
  if (length(missf)) stop("Feature archive missing: ", paste(missf, collapse = ", "))
  if (length(missa)) stop("Accuracy file missing: ", paste(missa, collapse = ", "))

  aa <- actual |>
    dplyr::transmute(
      week = suppressWarnings(as.integer(week)),
      player_id = wl31_chr(player_id),
      actual_fppg = wl31_num(actual_fppg, NA_real_),
      frozen_incumbent_projection = wl31_num(projection, NA_real_)
    ) |>
    dplyr::filter(
      is.finite(actual_fppg),
      is.finite(frozen_incumbent_projection),
      nzchar(player_id)
    ) |>
    dplyr::distinct(week, player_id, .keep_all = TRUE)

  cand <- features |>
    dplyr::mutate(
      season = suppressWarnings(as.integer(season)),
      week = suppressWarnings(as.integer(week)),
      player_id = wl31_chr(player_id)
    ) |>
    dplyr::filter(season == !!season, nzchar(player_id)) |>
    dplyr::inner_join(aa, by = c("week","player_id")) |>
    dplyr::mutate(
      weekly_fppg = actual_fppg,
      online_recency_weight_31 = as.numeric(current_season_weight),
      ledgered_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      .actual_revision_value = vapply(
        actual_fppg,
        function(x) if (is.finite(x)) sprintf("%.12g", x) else "NA",
        character(1)
      ),
      revision_id = paste(
        season, week, player_id, captured_at_utc,
        .actual_revision_value,
        feature_schema_hash, sep = "::"
      )
    ) |>
    dplyr::select(-.actual_revision_value)

  if (nrow(old) && "revision_id" %in% names(old)) {
    cand <- cand[!cand$revision_id %in% old$revision_id, , drop = FALSE]
  }
  if (nrow(cand)) {
    zz <- wl31_normalize_bind_types(old, cand)
    ledger <- dplyr::bind_rows(zz$old, zz$new)
  } else {
    ledger <- old
  }
  if (nrow(ledger)) {
    wl31_atomic_write_csv(ledger, ledger_path)
    cur <- wl31_current_view(ledger)
    wl31_atomic_write_csv(cur, current_path)
  } else {
    cur <- data.frame()
  }
  cat("[3.1 LEARNING] Ledger revisions added: ", nrow(cand),
      " | revisions total=", nrow(ledger),
      " | current rows=", nrow(cur), "\n", sep = "")
  cur
}

wl31_current_view <- function(ledger) {
  if (is.null(ledger) || !nrow(ledger)) return(data.frame())
  z <- ledger
  if (!"ledgered_at_utc" %in% names(z)) z$ledgered_at_utc <- ""
  z |>
    dplyr::mutate(
      season = suppressWarnings(as.integer(season)),
      week = suppressWarnings(as.integer(week)),
      player_id = wl31_chr(player_id),
      ledgered_at_utc = wl31_chr(ledgered_at_utc)
    ) |>
    dplyr::arrange(season, week, player_id, ledgered_at_utc) |>
    dplyr::group_by(season, week, player_id) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()
}

wl31_coverage_by_week <- function(current_rows, accuracy_rows) {
  if (!nrow(current_rows) || !nrow(accuracy_rows)) return(data.frame())
  a <- accuracy_rows |>
    dplyr::mutate(
      week = suppressWarnings(as.integer(week)),
      position = toupper(wl31_chr(position)),
      player_id = wl31_chr(player_id)
    ) |>
    dplyr::filter(position %in% POSITIONS, nzchar(player_id)) |>
    dplyr::distinct(week, player_id, .keep_all = TRUE) |>
    dplyr::count(week, position, name = "actual_rows")
  c <- current_rows |>
    dplyr::mutate(
      week = suppressWarnings(as.integer(week)),
      position = toupper(wl31_chr(position)),
      player_id = wl31_chr(player_id)
    ) |>
    dplyr::filter(position %in% POSITIONS, nzchar(player_id)) |>
    dplyr::distinct(week, player_id, .keep_all = TRUE) |>
    dplyr::count(week, position, name = "feature_rows")
  dplyr::full_join(a, c, by = c("week","position")) |>
    dplyr::mutate(
      actual_rows = dplyr::coalesce(actual_rows, 0L),
      feature_rows = dplyr::coalesce(feature_rows, 0L),
      coverage = dplyr::if_else(actual_rows > 0, feature_rows / actual_rows, 0)
    ) |>
    dplyr::arrange(week, factor(position, levels = POSITIONS))
}

wl31_eligible_weeks <- function(coverage, position_threshold = 0.70,
                                overall_threshold = 0.80) {
  if (!nrow(coverage)) return(integer())
  by_week <- coverage |>
    dplyr::group_by(week) |>
    dplyr::summarise(
      positions_present = sum(actual_rows > 0),
      min_position_coverage = min(coverage[actual_rows > 0], na.rm = TRUE),
      overall_coverage = sum(feature_rows) / pmax(sum(actual_rows), 1),
      .groups = "drop"
    ) |>
    dplyr::filter(
      positions_present == length(POSITIONS),
      is.finite(min_position_coverage),
      min_position_coverage >= position_threshold,
      overall_coverage >= overall_threshold
    )
  sort(unique(as.integer(by_week$week)))
}

wl31_add_cohorts <- function(d) {
  if (!nrow(d)) return(d)
  cutoff_map <- if (exists("WEEKLY_22_STARTER_CUTOFF")) {
    WEEKLY_22_STARTER_CUTOFF
  } else {
    REPLACEMENT_RANK
  }
  rank_projection <- if ("production_projection" %in% names(d)) {
    wl31_num(d$production_projection, NA_real_)
  } else if ("incumbent_projection" %in% names(d)) {
    wl31_num(d$incumbent_projection, NA_real_)
  } else {
    rep(NA_real_, nrow(d))
  }
  d$.wl31_rank_projection <- rank_projection
  d |>
    dplyr::group_by(week, position) |>
    dplyr::arrange(dplyr::desc(.wl31_rank_projection), .by_group = TRUE) |>
    dplyr::mutate(projected_position_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      starter_cutoff = as.numeric(cutoff_map[position]),
      starter_cohort = projected_position_rank <= starter_cutoff,
      relevant_cohort = projected_position_rank <=
        pmax(starter_cutoff * 2, starter_cutoff + 6)
    ) |>
    dplyr::select(-.wl31_rank_projection)
}

wl31_position_metrics <- function(d) {
  if (!nrow(d)) return(data.frame())
  safe_mae <- function(a, p) mean(abs(a - p), na.rm = TRUE)
  safe_rmse <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
  rows <- lapply(split(d, d$position), function(z) {
    zs <- z[z$starter_cohort %in% TRUE, , drop = FALSE]
    week_gain <- z |>
      dplyr::group_by(week) |>
      dplyr::summarise(
        inc = mean(abs(actual_fppg - incumbent_projection), na.rm = TRUE),
        cand = mean(abs(actual_fppg - candidate_projection), na.rm = TRUE),
        .groups = "drop"
      )
    data.frame(
      position = as.character(z$position[1]),
      n = nrow(z),
      oos_weeks = dplyr::n_distinct(z$week),
      starter_n = nrow(zs),
      incumbent_MAE = safe_mae(z$actual_fppg, z$incumbent_projection),
      candidate_MAE = safe_mae(z$actual_fppg, z$candidate_projection),
      incumbent_RMSE = safe_rmse(z$actual_fppg, z$incumbent_projection),
      candidate_RMSE = safe_rmse(z$actual_fppg, z$candidate_projection),
      incumbent_starter_MAE = if (nrow(zs)) safe_mae(zs$actual_fppg, zs$incumbent_projection) else NA_real_,
      candidate_starter_MAE = if (nrow(zs)) safe_mae(zs$actual_fppg, zs$candidate_projection) else NA_real_,
      week_wins = sum(week_gain$cand < week_gain$inc, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows) |>
    dplyr::mutate(
      MAE_gain_pct = 100 * (incumbent_MAE - candidate_MAE) / pmax(incumbent_MAE, 1e-9),
      RMSE_gain_pct = 100 * (incumbent_RMSE - candidate_RMSE) / pmax(incumbent_RMSE, 1e-9),
      starter_MAE_gain_pct = 100 * (incumbent_starter_MAE - candidate_starter_MAE) /
        pmax(incumbent_starter_MAE, 1e-9)
    )
}

wl31_online_gate <- function(metric_row, historical_promoted) {
  if (is.null(metric_row) || !nrow(metric_row)) {
    return(list(promote = FALSE, failed = "no_oos_evidence"))
  }
  r <- metric_row[1, , drop = FALSE]
  checks <- c(
    historical_gate = isTRUE(historical_promoted),
    oos_weeks = is.finite(r$oos_weeks) && r$oos_weeks >= 2,
    sample = is.finite(r$n) && r$n >= 40,
    starter_sample = is.finite(r$starter_n) && r$starter_n >= 12,
    all_MAE = is.finite(r$MAE_gain_pct) && r$MAE_gain_pct >= 0,
    starter_MAE = is.finite(r$starter_MAE_gain_pct) && r$starter_MAE_gain_pct >= 0,
    RMSE_guard = is.finite(r$RMSE_gain_pct) && r$RMSE_gain_pct >= -1,
    week_stability = is.finite(r$week_wins) &&
      r$week_wins >= ceiling(r$oos_weeks / 2)
  )
  list(
    promote = all(checks),
    failed = paste(names(checks)[!checks], collapse = ";")
  )
}
