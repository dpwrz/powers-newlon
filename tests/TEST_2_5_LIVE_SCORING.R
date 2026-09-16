# Fantasy Model 2.5 Live / League Scoring smoke test
source("config.R")
source("R/league_scoring_engine_25.R")
source("R/live_game_state_engine_25.R")

sample_stats <- list(pass_yd=250, pass_td=2, pass_int=1, rush_yd=20, rush_td=0, rec=0, rec_yd=0, rec_td=0, fum_lost=0)
base <- fm25_score_statline(sample_stats, fm25_base_scoring, "QB")
stopifnot(abs(base - (250*.04 + 2*4 - 2 + 20*.1)) < 1e-8)

ppr <- fm25_score_statline(list(rec=7,rec_yd=100,rec_td=1), c(rec=1,rec_yd=.1,rec_td=6), "WR")
stopifnot(abs(ppr - 23) < 1e-8)

live <- fm25_project_live_final(
  "WR",
  pregame=list(rec_tgt=9,rec=6,rec_yd=80,rec_td=.5,rush_att=0,rush_yd=0,rush_td=0,pass_att=0,pass_yd=0,pass_td=0,pass_int=0),
  live=list(rec_tgt=6,rec=4,rec_yd=61,rec_td=0,rush_att=0,rush_yd=0,rush_td=0,pass_att=0,pass_yd=0,pass_td=0,pass_int=0,fum_lost=0),
  quarter=3, clock="08:00", scoring=c(rec=1,rec_yd=.1,rec_td=6)
)
stopifnot(is.finite(live$final_points), live$final_points >= live$current_points)
cat("[2.5 LIVE SCORING TEST] League scoring + live state filter OK.\n")
