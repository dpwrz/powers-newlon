# ============================================================
# FANTASY MODEL 3.2 - DENOISE + NEW SIGNAL TOURNAMENT
# ============================================================
# 3.2 competes against the rolling 3.1 weekly learner, not against a weak
# strawman. Feature-set selection is based only on immutable historical seasons.
# Deployment requires a separate 2026 rolling OOF gate.

source("config.R")
ensure_packages(c("dplyr","readr","xgboost","nflreadr"))
source("R/weekly_engine_31.R")
source("R/weekly_training_history_31.R")
source("R/weekly_engine_32.R")

dir.create("models", recursive=TRUE, showWarnings=FALSE)
dir.create("models/archive/weekly_3_2_live", recursive=TRUE, showWarnings=FALSE)
dir.create("output", recursive=TRUE, showWarnings=FALSE)

historical_path <- "data/processed/weekly_model_table_2_3.csv"
live_path <- file.path("data","processed",paste0("weekly_training_live_",CURRENT_SEASON,".csv"))
offense_path <- "data/raw/weekly_offense_context_raw.csv"
oof31_path <- "output/weekly_3_1_live_oof.csv"
manifest31_path <- "output/weekly_3_1_live_manifest.csv"
config31_path <- "output/weekly_3_1_live_config.csv"

req <- c(historical_path,live_path,offense_path,oof31_path,manifest31_path,config31_path)
miss <- req[!file.exists(req)]
if(length(miss)) stop("3.2 missing prerequisite(s): ",paste(miss,collapse=", "))

historical <- readr::read_csv(historical_path,show_col_types=FALSE,progress=FALSE)
live <- readr::read_csv(live_path,show_col_types=FALSE,progress=FALSE)
offctx <- wk32_offense_context(offense_path)
oof31 <- readr::read_csv(oof31_path,show_col_types=FALSE,progress=FALSE)
manifest31 <- readr::read_csv(manifest31_path,show_col_types=FALSE,progress=FALSE)
config31 <- readr::read_csv(config31_path,show_col_types=FALSE,progress=FALSE)

for(dn in c("historical","live","oof31")){
  d <- get(dn)
  d$season <- suppressWarnings(as.integer(d$season))
  d$week <- suppressWarnings(as.integer(d$week))
  d$player_id <- as.character(d$player_id)
  d$position <- toupper(as.character(d$position))
  assign(dn,d)
}
historical <- historical |> dplyr::filter(season <= TRAIN_END, position %in% POSITIONS)
historical$training_weight_multiplier_31 <- 1
live$training_weight_multiplier_31 <- 1.5

# Current 3.1 architecture is the benchmark. For positions where the weekly
# learner is deployed, use its chronological OOF challenger prediction; where it
# failed promotion (currently QB), use the exact underlying production base.
use31 <- setNames(
  wk25_bool(manifest31$use_live_model),
  as.character(manifest31$position)
)
oof31$incumbent_fppg_32 <- ifelse(
  use31[as.character(oof31$position)] %in% TRUE,
  wk32_num(oof31$candidate_fppg_31,NA_real_),
  wk32_num(oof31$production_base_31,NA_real_)
)
inc31 <- oof31 |>
  dplyr::select(
    season,week,player_id,position,incumbent_fppg_32,
    actual_fppg,starter_cohort,relevant_cohort
  ) |>
  dplyr::distinct(season,week,player_id,position,.keep_all=TRUE)

# -------------------------------------------------------------------------
# Historical-only feature-surface selection.
# -------------------------------------------------------------------------
selection_rows <- list()
selected_sets <- list()
config_rows <- list()

for(pos in POSITIONS){
  cc <- config31[as.character(config31$position)==pos,,drop=FALSE]
  grid <- wk31_param_grid(pos)
  config_row <- if(nrow(cc)) grid[grid$config_id==as.character(cc$config_id[1]),,drop=FALSE] else data.frame()
  if(!nrow(config_row)){
    tune <- wk31_select_config(historical |> dplyr::filter(position==pos),pos,
      seed=SEED+32100L+match(pos,POSITIONS))
    config_row <- tune[tune$selected %in% TRUE,,drop=FALSE]
    if(!nrow(config_row)) config_row <- tune[1,,drop=FALSE]
  }
  config_rows[[pos]] <- config_row

  sel <- wk32_select_feature_set(
    historical |> dplyr::filter(position==pos),
    pos,config_row,offctx,
    seed=SEED+32200L+match(pos,POSITIONS)
  )
  selection_rows[[length(selection_rows)+1L]] <- sel
  pick <- sel$feature_set[sel$selected %in% TRUE][1]
  if(!length(pick) || is.na(pick)) pick <- "denoise_context"
  selected_sets[[pos]] <- pick
  cat("[3.2] ",pos," historical feature surface: ",pick,".\n",sep="")
}
selection <- dplyr::bind_rows(selection_rows)
readr::write_csv(selection,"output/weekly_3_2_feature_set_selection.csv")

# -------------------------------------------------------------------------
# Strict rolling 2026 OOF against the current 3.1 weekly architecture.
# -------------------------------------------------------------------------
starter_cut <- c(QB=16,RB=10,WR=10,TE=8)
relevant_cut <- c(QB=12,RB=6,WR=6,TE=4.5)
oof_rows <- list()
gate_rows <- list()
importance_rows <- list()
final_candidates <- list()

for(pos in POSITIONS){
  features <- wk32_feature_sets(pos)[[selected_sets[[pos]]]]
  config_row <- config_rows[[pos]]

  lp <- live |>
    dplyr::filter(position==pos) |>
    dplyr::inner_join(
      inc31 |> dplyr::filter(position==pos),
      by=c("season","week","player_id","position"),
      suffix=c("", "_inc")
    )
  weeks <- sort(unique(lp$week))
  prior_oof <- data.frame()

  for(target_week in weeks){
    test <- lp |> dplyr::filter(week==target_week)
    if(!nrow(test)) next
    train <- dplyr::bind_rows(
      historical |> dplyr::filter(position==pos),
      live |> dplyr::filter(position==pos,week<target_week)
    )
    if(nrow(train)<100) next

    fit <- wk32_fit(
      train,pos,config_row,features,offctx,
      seed=SEED+32300L+target_week*10L+match(pos,POSITIONS)
    )
    direct <- wk32_predict(fit,test,offctx)

    alpha <- if(!nrow(prior_oof)) 0.10 else wk32_select_alpha(prior_oof,pos)
    cand <- pmax(
      0,
      wk32_num(test$incumbent_fppg_32) +
        alpha*(direct-wk32_num(test$incumbent_fppg_32))
    )

    cur <- data.frame(
      season=test$season,week=test$week,player_id=test$player_id,
      player_display_name=if("player_display_name"%in%names(test))as.character(test$player_display_name) else "",
      position=pos,team=if("team"%in%names(test))as.character(test$team) else "",
      actual_fppg=wk32_num(test$actual_fppg,NA_real_),
      incumbent_fppg_32=wk32_num(test$incumbent_fppg_32,NA_real_),
      direct_fppg_32=direct,candidate_fppg_32=cand,blend_alpha_32=alpha,
      starter_cohort=wk25_bool(test$starter_cohort),
      relevant_cohort=wk25_bool(test$relevant_cohort),
      target_week=target_week,trained_live_through_week=target_week-1L,
      feature_set=selected_sets[[pos]],
      stringsAsFactors=FALSE
    )
    prior_oof <- dplyr::bind_rows(prior_oof,cur)
    oof_rows[[length(oof_rows)+1L]] <- cur
  }

  pos_oof <- prior_oof
  gate <- wk32_promotion_gate(pos_oof,pos)
  gate$feature_set <- selected_sets[[pos]]
  gate$n_features <- length(features)
  gate$evaluated_through_week <- if(nrow(pos_oof))max(pos_oof$week,na.rm=TRUE) else NA_integer_
  gate_rows[[length(gate_rows)+1L]] <- gate

  # Fit a final candidate every run so importance can be audited even if the
  # promotion gate rejects it.
  final_train <- dplyr::bind_rows(
    historical |> dplyr::filter(position==pos),
    live |> dplyr::filter(position==pos)
  )
  final_fit <- wk32_fit(
    final_train,pos,config_row,features,offctx,
    seed=SEED+32400L+match(pos,POSITIONS)
  )
  tmp_model <- paste0("models/.weekly_3_2_candidate_",pos,".json")
  tmp_meta <- paste0("models/.weekly_3_2_candidate_",pos,"_meta.rds")
  xgboost::xgb.save(final_fit$model,tmp_model)

  final_alpha <- if(nrow(pos_oof)) wk32_select_alpha(pos_oof,pos) else 0
  saveRDS(list(
    position=pos,features=features,feature_set=selected_sets[[pos]],
    config=config_row,nrounds=final_fit$nrounds,n_train=final_fit$n_train,
    trained_through_season=CURRENT_SEASON,
    trained_through_week=if(nrow(live))max(live$week,na.rm=TRUE) else NA_integer_,
    deployed_alpha=final_alpha,
    generated_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
    model_version="3.2"
  ),tmp_meta)
  final_candidates[[pos]] <- list(model=tmp_model,meta=tmp_meta,alpha=final_alpha)

  imp <- tryCatch(
    xgboost::xgb.importance(feature_names=features,model=final_fit$model),
    error=function(e)data.frame()
  )
  if(nrow(imp)){
    imp$position <- pos
    imp$feature_set <- selected_sets[[pos]]
    importance_rows[[length(importance_rows)+1L]] <- as.data.frame(imp)
  }
}

oof32 <- dplyr::bind_rows(oof_rows)
gates <- dplyr::bind_rows(gate_rows)
readr::write_csv(oof32,"output/weekly_3_2_live_oof.csv")
readr::write_csv(gates,"output/weekly_3_2_live_promotion.csv")
if(length(importance_rows)){
  readr::write_csv(dplyr::bind_rows(importance_rows),"output/weekly_3_2_feature_importance.csv")
}

# -------------------------------------------------------------------------
# New-signal effect audit. Correlation with incumbent residual is the useful
# quantity: it asks whether a pregame factor explains what the incumbent misses.
# -------------------------------------------------------------------------
effect_rows <- list()
for(pos in POSITIONS){
  z <- live |>
    dplyr::filter(position==pos) |>
    dplyr::inner_join(
      inc31 |> dplyr::filter(position==pos) |>
        dplyr::select(season,week,player_id,position,incumbent_fppg_32,actual_fppg),
      by=c("season","week","player_id","position")
    )
  if(!nrow(z)) next
  z <- wk32_add_features(z,offctx)
  residual <- wk32_num(z$actual_fppg,NA_real_) - wk32_num(z$incumbent_fppg_32,NA_real_)

  for(feat in unique(c(MODEL32_DERIVED_FEATURES,MODEL32_CONTEXT_BY_POSITION[[pos]]))){
    if(!feat%in%names(z)) next
    x <- wk32_num(z[[feat]],NA_real_)
    keep <- is.finite(x)&is.finite(residual)
    if(sum(keep)<30) next
    cor_res <- suppressWarnings(stats::cor(x[keep],residual[keep],method="spearman"))
    cor_out <- suppressWarnings(stats::cor(x[keep],wk32_num(z$actual_fppg[keep],NA_real_),method="spearman"))
    effect_rows[[length(effect_rows)+1L]] <- data.frame(
      position=pos,feature=feat,n=sum(keep),
      residual_spearman=cor_res,outcome_spearman=cor_out,
      abs_residual_signal=abs(cor_res),
      stringsAsFactors=FALSE
    )
  }
}
effects <- dplyr::bind_rows(effect_rows) |>
  dplyr::arrange(position,dplyr::desc(abs_residual_signal))
readr::write_csv(effects,"output/weekly_3_2_signal_effects.csv")

# -------------------------------------------------------------------------
# Promotion lifecycle. A failed new challenger never evicts an already-deployed
# 3.2 model; previous artifacts are archived before replacement.
# -------------------------------------------------------------------------
manifest_path <- "output/weekly_3_2_live_manifest.csv"
old_manifest <- if(file.exists(manifest_path)){
  tryCatch(readr::read_csv(manifest_path,show_col_types=FALSE,progress=FALSE),error=function(e)data.frame())
}else data.frame()

stamp <- format(Sys.time(),"%Y%m%dT%H%M%SZ",tz="UTC")
manifest_rows <- list()
for(pos in POSITIONS){
  gate <- gates[gates$position==pos,,drop=FALSE]
  old <- if(nrow(old_manifest)) old_manifest[as.character(old_manifest$position)==pos,,drop=FALSE] else data.frame()
  alias_model <- paste0("models/weekly_3_2_live_",pos,".json")
  alias_meta <- paste0("models/weekly_3_2_live_",pos,"_meta.rds")
  already <- file.exists(alias_model)&&file.exists(alias_meta)
  promote_now <- nrow(gate)&&isTRUE(gate$promoted[1])

  if(promote_now){
    ad <- file.path("models","archive","weekly_3_2_live",stamp)
    dir.create(ad,recursive=TRUE,showWarnings=FALSE)
    if(already){
      file.copy(alias_model,file.path(ad,basename(alias_model)),overwrite=FALSE)
      file.copy(alias_meta,file.path(ad,basename(alias_meta)),overwrite=FALSE)
    }
    ok1 <- file.copy(final_candidates[[pos]]$model,alias_model,overwrite=TRUE)
    ok2 <- file.copy(final_candidates[[pos]]$meta,alias_meta,overwrite=TRUE)
    if(!isTRUE(ok1)||!isTRUE(ok2)) stop("Could not promote 3.2 artifacts for ",pos)
    cat("[3.2] PROMOTED ",pos," | ",selected_sets[[pos]]," | alpha=",final_candidates[[pos]]$alpha,".\n",sep="")
  } else {
    cat("[3.2] KEEP INCUMBENT ",pos," | failed: ",if(nrow(gate))gate$failed_checks[1] else "no_gate",".\n",sep="")
  }
  unlink(final_candidates[[pos]]$model)
  unlink(final_candidates[[pos]]$meta)

  deployed_week <- if(promote_now&&nrow(gate))gate$evaluated_through_week[1] else if(already){
    tryCatch(as.integer(readRDS(alias_meta)$trained_through_week),error=function(e)NA_integer_)
  } else NA_integer_
  deployed_alpha <- if(promote_now) final_candidates[[pos]]$alpha else if(nrow(old)&&"deployed_alpha"%in%names(old)){
    suppressWarnings(as.numeric(old$deployed_alpha[1]))
  } else 0
  use_live <- promote_now||already

  manifest_rows[[length(manifest_rows)+1L]] <- data.frame(
    position=pos,use_live_model=use_live,
    deployed_through_week=deployed_week,deployed_alpha=deployed_alpha,
    feature_set=if(promote_now)selected_sets[[pos]] else if(nrow(old)&&"feature_set"%in%names(old))as.character(old$feature_set[1]) else selected_sets[[pos]],
    latest_challenger_promoted=promote_now,
    latest_all_mae_gain_pct=if(nrow(gate))gate$all_mae_gain_pct[1] else NA_real_,
    latest_starter_mae_gain_pct=if(nrow(gate))gate$starter_mae_gain_pct[1] else NA_real_,
    latest_week_win_rate=if(nrow(gate))gate$week_win_rate[1] else NA_real_,
    latest_bootstrap_p_improves=if(nrow(gate))gate$bootstrap_p_improves[1] else NA_real_,
    latest_failed_checks=if(nrow(gate))gate$failed_checks[1] else "no_gate",
    updated_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
    stringsAsFactors=FALSE
  )
}
manifest <- dplyr::bind_rows(manifest_rows)
readr::write_csv(manifest,manifest_path)

if(nrow(oof32)){
  residual <- oof32 |>
    dplyr::mutate(
      residual=actual_fppg-candidate_fppg_32,
      abs_error=abs(residual)
    )
  readr::write_csv(residual,"output/weekly_3_2_live_residual_pool.csv")
}

lines <- c(
  "FANTASY MODEL 3.2 - DENOISE + OFFENSIVE CONTEXT",
  paste0("Generated: ",format(Sys.time(),"%Y-%m-%d %H:%M:%S %z")),
  "",
  "POSITION RESULTS"
)
for(i in seq_len(nrow(gates))){
  g <- gates[i,]
  lines <- c(lines,sprintf(
    "%s | %s | set=%s (%d features) | all MAE %+0.2f%% | starter MAE %+0.2f%% | week win %.1f%% | bootstrap %.3f | max bad week %.2f%% | %s",
    g$position,ifelse(g$promoted,"PROMOTE","KEEP INCUMBENT"),g$feature_set,g$n_features,
    g$all_mae_gain_pct,g$starter_mae_gain_pct,100*g$week_win_rate,
    g$bootstrap_p_improves,g$max_week_mae_degradation_pct,
    ifelse(nchar(g$failed_checks),g$failed_checks,"all gates passed")
  ))
}
writeLines(lines,"output/weekly_3_2_quality_report.txt")
cat("\n[3.2] Denoise + signal tournament complete.\n")
print(gates)
