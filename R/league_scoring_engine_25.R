# ============================================================
# FANTASY MODEL 2.5 - SLEEPER LEAGUE SCORING BRIDGE
# ============================================================
# Keeps the validated point model intact and adjusts only the scoring-system
# component implied by the structured stat line.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

fm25_base_scoring <- c(
  pass_yd = 0.04, pass_td = 4, pass_int = -2, pass_2pt = 2,
  rush_yd = 0.1, rush_td = 6, rush_2pt = 2,
  rec = 1, rec_yd = 0.1, rec_td = 6, rec_2pt = 2,
  fum_lost = -2
)

fm25_score_statline <- function(stats, scoring = fm25_base_scoring, position = "") {
  getn <- function(nm) {
    x <- suppressWarnings(as.numeric(stats[[nm]] %||% 0))
    ifelse(is.finite(x), x, 0)
  }
  total <- 0
  if (is.null(scoring)) scoring <- numeric()
  if (is.list(scoring) && !is.atomic(scoring)) scoring <- unlist(scoring, use.names = TRUE)
  for (nm in names(scoring)) {
    wt <- suppressWarnings(as.numeric(scoring[[nm]]))
    if (!is.finite(wt) || wt == 0) next
    if (nm %in% c("pass_att","pass_cmp","pass_yd","pass_td","pass_int","pass_2pt","pass_fd",
                  "rush_att","rush_yd","rush_td","rush_2pt","rush_fd",
                  "rec_tgt","rec","rec_yd","rec_td","rec_2pt","rec_fd","fum_lost")) {
      total <- total + getn(nm) * wt
    } else if (nm == "bonus_pass_yd_300" && getn("pass_yd") >= 300) total <- total + wt
    else if (nm == "bonus_pass_yd_400" && getn("pass_yd") >= 400) total <- total + wt
    else if (nm == "bonus_rush_yd_100" && getn("rush_yd") >= 100) total <- total + wt
    else if (nm == "bonus_rush_yd_200" && getn("rush_yd") >= 200) total <- total + wt
    else if (nm == "bonus_rec_yd_100" && getn("rec_yd") >= 100) total <- total + wt
    else if (nm == "bonus_rec_yd_200" && getn("rec_yd") >= 200) total <- total + wt
    else if (nm %in% c("bonus_rec_te","rec_te") && position == "TE") total <- total + getn("rec") * wt
    else if (nm %in% c("bonus_rec_rb","rec_rb") && position == "RB") total <- total + getn("rec") * wt
    else if (nm %in% c("bonus_rec_wr","rec_wr") && position == "WR") total <- total + getn("rec") * wt
    else if (nm %in% c("bonus_rec_qb","rec_qb") && position == "QB") total <- total + getn("rec") * wt
  }
  as.numeric(total)
}

fm25_adjust_projection_for_league <- function(base_projection, stats, sleeper_scoring, position = "") {
  p <- suppressWarnings(as.numeric(base_projection))
  if (!is.finite(p)) return(NA_real_)
  p + fm25_score_statline(stats, sleeper_scoring, position) - fm25_score_statline(stats, fm25_base_scoring, position)
}
