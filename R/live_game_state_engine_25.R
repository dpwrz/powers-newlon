# ============================================================
# FANTASY MODEL 2.5 - LIVE GAME STATE FILTER
# ============================================================
# Conservative within-game updater. This is not a replacement for the weekly
# champion models; it translates partial game state into expected final state.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

source("R/league_scoring_engine_25.R")

fm25_clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))
fm25_num <- function(x) { y <- suppressWarnings(as.numeric(x)); ifelse(is.finite(y), y, 0) }

fm25_game_elapsed <- function(quarter = 1, clock = "15:00") {
  q <- max(1, fm25_num(quarter))
  if (q > 4) return(.98)
  parts <- strsplit(as.character(clock %||% "15:00"), ":", fixed = TRUE)[[1]]
  m <- if (length(parts)) fm25_num(parts[1]) else 15
  s <- if (length(parts) > 1) fm25_num(parts[2]) else 0
  elapsed <- (q - 1) * 15 + (15 - fm25_clamp(m + s / 60, 0, 15))
  fm25_clamp(elapsed / 60, .02, .98)
}

fm25_volume_final <- function(observed, expected, elapsed) {
  observed <- fm25_num(observed); expected <- fm25_num(expected)
  if (expected <= 0) return(observed)
  expected_to_date <- max(expected * elapsed, .25)
  pace <- fm25_clamp(observed / expected_to_date, .35, 2.25)
  trust <- fm25_clamp(.12 + elapsed * .68, .12, .72)
  adjusted_rate <- 1 + trust * (pace - 1)
  max(observed, observed + expected * (1 - elapsed) * adjusted_rate)
}

fm25_efficiency_blend <- function(observed_value, observed_opp, base_rate, min_opp, cap_lo, cap_hi) {
  observed_value <- fm25_num(observed_value); observed_opp <- fm25_num(observed_opp); base_rate <- fm25_num(base_rate)
  if (observed_opp < min_opp || base_rate <= 0) return(base_rate)
  live_rate <- observed_value / max(observed_opp, 1)
  trust <- fm25_clamp(observed_opp / (observed_opp + min_opp * 3), 0, .35)
  fm25_clamp(base_rate * (1 - trust) + live_rate * trust, base_rate * cap_lo, base_rate * cap_hi)
}

fm25_project_live_final <- function(position, pregame, live, quarter, clock, scoring) {
  e <- fm25_game_elapsed(quarter, clock)
  p <- function(nm) fm25_num(pregame[[nm]])
  l <- function(nm) fm25_num(live[[nm]])

  pass_att <- fm25_volume_final(l("pass_att"), p("pass_att"), e)
  rush_att <- fm25_volume_final(l("rush_att"), p("rush_att"), e)
  targets <- fm25_volume_final(l("rec_tgt"), p("rec_tgt"), e)

  ypa <- fm25_efficiency_blend(l("pass_yd"), l("pass_att"), if (p("pass_att") > 0) p("pass_yd")/p("pass_att") else 0, 8, .70, 1.35)
  ypc <- fm25_efficiency_blend(l("rush_yd"), l("rush_att"), if (p("rush_att") > 0) p("rush_yd")/p("rush_att") else 0, 5, .65, 1.45)
  base_catch <- if (p("rec_tgt") > 0) p("rec")/p("rec_tgt") else 0
  live_catch <- if (l("rec_tgt") > 0) l("rec")/l("rec_tgt") else base_catch
  catch_trust <- fm25_clamp(l("rec_tgt")/(l("rec_tgt") + 12), 0, .30)
  catch_rate <- fm25_clamp(base_catch * (1-catch_trust) + live_catch*catch_trust, .25, .95)
  rec <- max(l("rec"), l("rec") + max(0, targets-l("rec_tgt"))*catch_rate)
  ypr <- fm25_efficiency_blend(l("rec_yd"), l("rec"), if (p("rec") > 0) p("rec_yd")/p("rec") else 0, 4, .60, 1.50)

  final <- list(
    pass_att = pass_att,
    pass_yd = l("pass_yd") + max(0, pass_att-l("pass_att"))*ypa,
    pass_td = l("pass_td") + max(0, pass_att-l("pass_att")) * ifelse(p("pass_att")>0,p("pass_td")/p("pass_att"),0),
    pass_int = l("pass_int") + max(0, pass_att-l("pass_att")) * ifelse(p("pass_att")>0,p("pass_int")/p("pass_att"),0),
    rush_att = rush_att,
    rush_yd = l("rush_yd") + max(0, rush_att-l("rush_att"))*ypc,
    rush_td = l("rush_td") + max(0, rush_att-l("rush_att")) * ifelse(p("rush_att")>0,p("rush_td")/p("rush_att"),0),
    rec_tgt = targets,
    rec = rec,
    rec_yd = l("rec_yd") + max(0, rec-l("rec"))*ypr,
    rec_td = l("rec_td") + max(0, targets-l("rec_tgt")) * ifelse(p("rec_tgt")>0,p("rec_td")/p("rec_tgt"),0),
    fum_lost = l("fum_lost")
  )

  opp_expected <- if (position == "QB") p("pass_att") + p("rush_att") else if (position == "RB") p("rush_att") + .65*p("rec_tgt") else p("rec_tgt")
  opp_final <- if (position == "QB") final$pass_att + final$rush_att else if (position == "RB") final$rush_att + .65*final$rec_tgt else final$rec_tgt
  surprise <- if (opp_expected > 1) fm25_clamp((opp_final-opp_expected)/opp_expected,-.60,.60) else 0

  list(
    elapsed = e,
    current_points = fm25_score_statline(live, scoring, position),
    final_points = fm25_score_statline(final, scoring, position),
    final_stats = final,
    role_surprise = surprise
  )
}

fm25_live_future_delta <- function(base_projection, role_surprise, weeks_ahead = 1) {
  base <- fm25_num(base_projection)
  decay <- .58 ^ max(0, weeks_ahead - 1)
  fm25_clamp(base * role_surprise * .12 * decay, -base*.10, base*.10)
}
