# ============================================================
# FANTASY MODEL 3.1 - IMMUTABLE WEEKLY LEARNING SUPPORT
# ============================================================
# Historical 2016-2025 training data remain untouched. Current-season rows are
# appended to a separate ledger only after the corresponding NFL week is final.

fm31_history_key <- function(d, key = c("season","week","player_id","position")) {
  if (!nrow(d)) return(character())
  do.call(paste, c(lapply(d[key], as.character), sep = "|"))
}

fm31_append_training_history <- function(existing, incoming,
                                         key = c("season","week","player_id","position")) {
  stopifnot(is.data.frame(existing), is.data.frame(incoming))
  if (!all(key %in% names(incoming))) stop("Missing incoming identity columns")
  if (nrow(existing) && !all(key %in% names(existing))) stop("Missing existing identity columns")
  for (nm in c("game_final","feature_source","leakage_audited")) {
    if (!nm %in% names(incoming)) stop("Missing ", nm)
  }

  incoming <- incoming[!is.na(incoming$game_final) & incoming$game_final %in% TRUE, , drop = FALSE]
  if (!nrow(incoming)) return(existing)

  exact <- as.character(incoming$feature_source) == "pregame_archive"
  reconstructed <- as.character(incoming$feature_source) == "strict_reconstruction"

  if (any(exact)) {
    if (!all(c("pregame_captured_at","kickoff_at") %in% names(incoming))) {
      stop("Exact pregame rows require capture and kickoff timestamps")
    }
    cap <- suppressWarnings(as.POSIXct(incoming$pregame_captured_at[exact], tz = "UTC"))
    kick <- suppressWarnings(as.POSIXct(incoming$kickoff_at[exact], tz = "UTC"))
    if (any(is.na(cap) | is.na(kick) | cap >= kick)) stop("Invalid pregame archive evidence")
  }
  if (any(reconstructed & !(incoming$leakage_audited %in% TRUE))) {
    stop("Strictly reconstructed rows must pass leakage audit")
  }
  if (any(!(exact | reconstructed))) stop("Unsupported current-season feature source")

  kk <- fm31_history_key(incoming, key)
  if (anyDuplicated(kk)) stop("Duplicate incoming observations")
  if (nrow(existing)) {
    new <- incoming[!kk %in% fm31_history_key(existing, key), , drop = FALSE]
  } else {
    new <- incoming
  }
  if (!nrow(new)) return(existing)

  cols <- union(names(existing), names(new))
  for (nm in setdiff(cols, names(existing))) existing[[nm]] <- rep(NA, nrow(existing))
  for (nm in setdiff(cols, names(new))) new[[nm]] <- rep(NA, nrow(new))
  out <- rbind(existing[cols], new[cols])
  out[order(as.integer(out$season), as.integer(out$week), as.character(out$position),
            as.character(out$player_id)), , drop = FALSE]
}

fm31_weekly_live_gate <- function(d, pos, min_weeks = 3L, min_starters = 12L) {
  if (!nrow(d)) {
    return(data.frame(position = pos, promoted = FALSE, n = 0L, weeks = 0L,
                      starter_n = 0L, all_mae_gain_pct = NA_real_,
                      starter_mae_gain_pct = NA_real_, week_win_rate = NA_real_,
                      bootstrap_p_improves = NA_real_, failed_checks = "no_oof_rows"))
  }

  d <- d[as.character(d$position) == pos, , drop = FALSE]
  weeks <- sort(unique(as.integer(d$week)))
  starter <- if ("starter_cohort" %in% names(d)) wk25_bool(d$starter_cohort) else rep(FALSE, nrow(d))

  base_mae <- mean(abs(wk31_num(d$actual_fppg) - wk31_num(d$production_base_31)), na.rm = TRUE)
  cand_mae <- mean(abs(wk31_num(d$actual_fppg) - wk31_num(d$candidate_fppg_31)), na.rm = TRUE)
  base_st <- if (any(starter)) mean(abs(wk31_num(d$actual_fppg[starter]) - wk31_num(d$production_base_31[starter])), na.rm = TRUE) else NA_real_
  cand_st <- if (any(starter)) mean(abs(wk31_num(d$actual_fppg[starter]) - wk31_num(d$candidate_fppg_31[starter])), na.rm = TRUE) else NA_real_

  week_gain <- vapply(weeks, function(w) {
    z <- d[as.integer(d$week) == w, , drop = FALSE]
    b <- mean(abs(wk31_num(z$actual_fppg) - wk31_num(z$production_base_31)), na.rm = TRUE)
    c <- mean(abs(wk31_num(z$actual_fppg) - wk31_num(z$candidate_fppg_31)), na.rm = TRUE)
    b - c
  }, numeric(1))
  boot <- wk31_bootstrap_gain(d, "Starter", seed = 3190L + match(pos, c("QB","RB","WR","TE")))

  gain_pct <- function(b, c) if (is.finite(b) && b > 0 && is.finite(c)) 100 * (b - c) / b else NA_real_
  all_gain <- gain_pct(base_mae, cand_mae)
  starter_gain <- gain_pct(base_st, cand_st)
  win_rate <- if (length(week_gain)) mean(week_gain > 0, na.rm = TRUE) else NA_real_

  checks <- c(
    enough_weeks = length(weeks) >= min_weeks,
    enough_starters = sum(starter, na.rm = TRUE) >= min_starters,
    all_MAE = is.finite(all_gain) && all_gain > 0,
    starter_MAE = is.finite(starter_gain) && starter_gain > 0,
    weekly_stability = is.finite(win_rate) && win_rate >= 0.50,
    paired_confidence = is.finite(boot$p_improves[1]) && boot$p_improves[1] >= 0.55
  )

  data.frame(
    position = pos,
    promoted = all(checks),
    n = nrow(d),
    weeks = length(weeks),
    starter_n = sum(starter, na.rm = TRUE),
    all_mae_gain_pct = all_gain,
    starter_mae_gain_pct = starter_gain,
    week_win_rate = win_rate,
    bootstrap_p_improves = boot$p_improves[1],
    failed_checks = paste(names(checks)[!checks], collapse = ";"),
    stringsAsFactors = FALSE
  )
}
