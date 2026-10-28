# ============================================================
# FANTASY MODEL 3.2 - DENOISE / SIGNAL SAFETY TESTS
# ============================================================
source("config.R")
source("R/weekly_engine_31.R")
source("R/weekly_engine_32.R")

sample <- data.frame(
  season=2025L,week=6L,player_id="p1",position="WR",team="AAA",
  preseason_prior_fppg=12,prior_game_fppg=13,roll3_fppg=14,roll5_fppg=12,
  season_to_date_fppg=13,roll3_targets=8,roll5_targets=7,
  roll3_target_share=.25,roll3_carry_share=.02,roll3_pass_attempt_share=0,
  roll3_offense_pct=.90,roll5_offense_pct=.85,roll3_offense_snaps=58,
  roll3_team_pass_attempts=36,roll3_team_carries=25,implied_team_total=24,
  preseason_deep_target_rate=.30,preseason_redzone_target_rate=.15,
  season_to_date_rush_ypc=0,roll5_carries=0,
  stringsAsFactors=FALSE
)
ctx <- data.frame(
  season=2025L,week=1:5,team="AAA",
  off_plays=c(60,62,64,61,63),
  off_pass_rate=c(.58,.60,.61,.57,.62),
  off_pass_epa=c(.05,.10,.08,.04,.12),
  off_rush_epa=c(-.02,.01,.03,0,.02),
  off_pass_success=c(.45,.48,.47,.44,.49),
  off_rush_success=c(.40,.42,.43,.41,.44),
  off_sack_allowed_rate=c(.06,.05,.04,.07,.04),
  off_qb_hit_allowed_rate=c(.12,.10,.11,.13,.09),
  off_deep_pass_rate=c(.15,.18,.17,.14,.19),
  off_explosive_pass_rate=c(.10,.11,.09,.08,.12),
  off_redzone_pass_td_rate=c(.20,.25,.22,.18,.27),
  off_redzone_rush_td_rate=c(.15,.18,.16,.14,.19),
  off_neutral_pass_rate=0,off_redzone_pass_rate=0,
  off_no_huddle_rate=0,off_shotgun_rate=0,
  stringsAsFactors=FALSE
)
z <- wk32_add_features(sample,ctx)
stopifnot(nrow(z)==1L)
stopifnot(abs(z$expected_targets_32[1]-9) < 1e-9)
stopifnot(z$role_stability_32[1] > 0.8)
stopifnot(z$team_off_plays_roll4[1] > 60)
stopifnot(z$scoring_environment_32[1] > 0)

for(pos in c("QB","RB","WR","TE")){
  sets <- wk32_feature_sets(pos)
  stopifnot(all(c("denoise","denoise_context","full_context") %in% names(sets)))
  stopifnot(length(sets$denoise) < length(sets$full_context))
  stopifnot(!length(intersect(sets$denoise,MODEL25_FORBIDDEN_FEATURES)))
  stopifnot(!length(intersect(sets$denoise_context,MODEL25_FORBIDDEN_FEATURES)))
}

good <- do.call(rbind,lapply(2:7,function(w){
  data.frame(
    season=2026L,week=w,player_id=paste0("p",w,"_",1:10),position="WR",
    actual_fppg=10,incumbent_fppg_32=12,candidate_fppg_32=11,
    starter_cohort=TRUE,relevant_cohort=TRUE,stringsAsFactors=FALSE
  )
}))
g <- wk32_promotion_gate(good,"WR",min_weeks=4L,min_starters=20L)
stopifnot(isTRUE(g$promoted[1]))

bad <- good
bad$candidate_fppg_32 <- 13
g2 <- wk32_promotion_gate(bad,"WR",min_weeks=4L,min_starters=20L)
stopifnot(!isTRUE(g2$promoted[1]))

cat("Model 3.2 denoise/signal safety checks passed.\n")
