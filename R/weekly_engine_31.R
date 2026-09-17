# ============================================================
# FANTASY MODEL 3.1 - PERFORMANCE / ACCURACY ENGINE
# ============================================================
# Challenger only. Production is replaced by 3.1 only after honest OOF gates.
# Additions:
#   1) pre-kickoff decision-weighted XGBoost training
#   2) position-specific hyperparameter selection on prior seasons only
#   3) adaptive early-season prior features derived only from safe lagged inputs

if (!exists("MODEL25_SAFE_FEATURES")) source("R/weekly_engine_25.R")

MODEL31_DERIVED_FEATURES <- c(
  "adaptive_prior_weight_31",
  "adaptive_prior_fppg_31",
  "role_change_score_31"
)
MODEL31_SAFE_FEATURES <- unique(c(MODEL25_SAFE_FEATURES, MODEL31_DERIVED_FEATURES))
MODEL31_ALPHA_GRID <- c(0, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40)
MODEL31_FINAL_ALPHA_CAP <- c(QB = 0.25, RB = 0.35, WR = 0.40, TE = 0.30)
MODEL31_BOOTSTRAP_REPS <- 1000L

wk31_num <- function(x, default = 0) {
  z <- suppressWarnings(as.numeric(x))
  z[!is.finite(z)] <- default
  z
}

wk31_add_features <- function(d) {
  if (!nrow(d)) return(d)

  gp <- wk31_num(if ("games_played_prior" %in% names(d)) d$games_played_prior else 0)
  pre <- wk31_num(if ("preseason_prior_fppg" %in% names(d)) d$preseason_prior_fppg else 0)
  r3 <- wk31_num(if ("roll3_fppg" %in% names(d)) d$roll3_fppg else pre)
  r5 <- wk31_num(if ("roll5_fppg" %in% names(d)) d$roll5_fppg else r3)
  role_trend <- abs(wk31_num(if ("role_fppg_trend" %in% names(d)) d$role_fppg_trend else 0))
  snap_trend <- abs(wk31_num(if ("snap_trend" %in% names(d)) d$snap_trend else 0))
  target_trend <- abs(wk31_num(if ("target_trend" %in% names(d)) d$target_trend else 0))
  carry_trend <- abs(wk31_num(if ("carry_trend" %in% names(d)) d$carry_trend else 0))

  # Role-change magnitude rises when multiple lagged usage signals move together.
  role_change <- tanh(
    role_trend / 4 +
      snap_trend / 30 +
      target_trend / 3 +
      carry_trend / 4
  )
  sample_conf <- pmin(1, pmax(0, gp / 5))
  adaptive_w <- pmin(0.95, pmax(0.10, 0.15 + 0.55 * sample_conf + 0.30 * role_change))

  # Before enough NFL games exist, retain more preseason information. Once role
  # evidence becomes convincing, current usage receives weight faster.
  current_form <- ifelse(gp >= 3, r3, ifelse(gp > 0, 0.65 * r3 + 0.35 * r5, pre))
  adaptive_prior <- (1 - adaptive_w) * pre + adaptive_w * current_form

  d$role_change_score_31 <- role_change
  d$adaptive_prior_weight_31 <- adaptive_w
  d$adaptive_prior_fppg_31 <- pmax(0, adaptive_prior)
  d
}

wk31_expected_fppg <- function(d) {
  x <- wk31_add_features(d)
  pmax(
    wk31_num(x$adaptive_prior_fppg_31),
    0.75 * wk31_num(if ("roll3_fppg" %in% names(x)) x$roll3_fppg else 0) +
      0.25 * wk31_num(if ("preseason_prior_fppg" %in% names(x)) x$preseason_prior_fppg else 0)
  )
}

wk31_training_weight <- function(d, pos = NULL) {
  if (!nrow(d)) return(numeric())
  if (is.null(pos) || !length(pos)) pos <- as.character(d$position[1])
  exp_fppg <- wk31_expected_fppg(d)
  cuts <- list(
    QB = c(relevant = 12, starter = 16),
    RB = c(relevant = 6, starter = 10),
    WR = c(relevant = 6, starter = 10),
    TE = c(relevant = 4.5, starter = 8)
  )
  cc <- cuts[[as.character(pos)[1]]]
  if (is.null(cc)) return(rep(1, nrow(d)))
  w <- ifelse(exp_fppg >= cc[["starter"]], 3,
              ifelse(exp_fppg >= cc[["relevant"]], 2, 1))
  # Keep weights bounded so depth observations still inform variance and role
  # transitions while fantasy-relevant rows dominate the objective.
  as.numeric(pmin(3, pmax(1, w)))
}

wk31_param_grid <- function(pos) {
  # Same search space for reproducibility, selected independently per position.
  data.frame(
    config_id = sprintf("%s_%02d", pos, 1:12),
    max_depth = c(2,3,3,4,2,3,4,3,2,4,3,4),
    eta = c(.020,.020,.030,.020,.040,.040,.030,.015,.030,.015,.025,.025),
    min_child_weight = c(20,20,30,30,15,40,40,25,45,20,50,30),
    subsample = c(.85,.85,.85,.80,.90,.80,.85,1.00,.75,.90,.90,.75),
    colsample_bytree = c(.65,.70,.65,.65,.75,.60,.70,.55,.80,.60,.75,.70),
    lambda = c(20,20,25,30,15,30,35,25,40,15,35,25),
    alpha = c(1,1,1,1.5,.5,1.5,2,1,.5,2,1,1.5),
    gamma = c(.05,.05,.10,.10,0,.15,.15,.05,.10,0,.15,.05),
    nrounds = c(300,325,300,325,275,275,300,400,250,400,350,350),
    stringsAsFactors = FALSE
  )
}

wk31_params_from_row <- function(r, seed = 42L, objective = "reg:pseudohubererror") {
  list(
    objective = objective,
    eval_metric = "mae",
    max_depth = as.integer(r$max_depth[[1]]),
    eta = as.numeric(r$eta[[1]]),
    min_child_weight = as.numeric(r$min_child_weight[[1]]),
    subsample = as.numeric(r$subsample[[1]]),
    colsample_bytree = as.numeric(r$colsample_bytree[[1]]),
    lambda = as.numeric(r$lambda[[1]]),
    alpha = as.numeric(r$alpha[[1]]),
    gamma = as.numeric(r$gamma[[1]]),
    tree_method = "hist",
    seed = as.integer(seed)
  )
}

wk31_fit <- function(d, pos, config_row, target_col = "weekly_fppg",
                     features = MODEL31_SAFE_FEATURES, seed = 42L) {
  if (!requireNamespace("xgboost", quietly = TRUE)) stop("xgboost is required for Model 3.1.")
  d <- wk31_add_features(d)
  y <- wk31_num(d[[target_col]], NA_real_)
  keep <- is.finite(y)
  if (sum(keep) < 100) stop("Not enough chronological rows for 3.1 ", pos, " model.")

  X <- wk25_prepare_matrix(d[keep, , drop = FALSE], features)
  w <- wk31_training_weight(d[keep, , drop = FALSE], pos)
  dm <- xgboost::xgb.DMatrix(X, label = y[keep], weight = w)
  nrounds <- as.integer(config_row$nrounds[[1]])

  fit <- tryCatch(
    xgboost::xgb.train(
      params = wk31_params_from_row(config_row, seed, "reg:pseudohubererror"),
      data = dm, nrounds = nrounds, verbose = 0
    ),
    error = function(e) {
      message("[3.1] Pseudo-Huber unavailable; using squared error: ", conditionMessage(e))
      xgboost::xgb.train(
        params = wk31_params_from_row(config_row, seed, "reg:squarederror"),
        data = dm, nrounds = nrounds, verbose = 0
      )
    }
  )
  list(
    model = fit, features = features, config = config_row,
    nrounds = nrounds, n_train = sum(keep), position = pos
  )
}

wk31_predict <- function(obj, d) {
  if (!nrow(d)) return(numeric())
  d <- wk31_add_features(d)
  X <- wk25_prepare_matrix(d, obj$features)
  pmax(0, as.numeric(stats::predict(obj$model, xgboost::xgb.DMatrix(X))))
}

wk31_weighted_metrics <- function(actual, pred, weight = NULL) {
  a <- wk31_num(actual, NA_real_)
  p <- wk31_num(pred, NA_real_)
  if (is.null(weight)) weight <- rep(1, length(a))
  w <- wk31_num(weight, 1)
  keep <- is.finite(a) & is.finite(p) & is.finite(w) & w > 0
  if (!any(keep)) return(c(MAE = NA_real_, RMSE = NA_real_))
  a <- a[keep]; p <- p[keep]; w <- w[keep]
  w <- w / sum(w)
  c(
    MAE = sum(w * abs(a - p)),
    RMSE = sqrt(sum(w * (a - p)^2))
  )
}

wk31_select_config <- function(train, pos, seed = 42L) {
  train <- train[as.character(train$position) == pos, , drop = FALSE]
  years <- sort(unique(as.integer(train$season)))
  years <- years[is.finite(years)]
  grid <- wk31_param_grid(pos)

  # Inner validation is strictly earlier than the outer target season.
  if (length(years) < 2) {
    grid$inner_MAE <- NA_real_
    grid$inner_RMSE <- NA_real_
    grid$selected <- seq_len(nrow(grid)) == 1
    return(grid)
  }

  val_year <- max(years)
  inner_train <- train[as.integer(train$season) < val_year, , drop = FALSE]
  inner_val <- train[as.integer(train$season) == val_year, , drop = FALSE]
  if (nrow(inner_train) < 100 || nrow(inner_val) < 20) {
    grid$inner_MAE <- NA_real_
    grid$inner_RMSE <- NA_real_
    grid$selected <- seq_len(nrow(grid)) == 1
    return(grid)
  }

  wv <- wk31_training_weight(inner_val, pos)
  mae <- rmse <- rep(NA_real_, nrow(grid))
  for (i in seq_len(nrow(grid))) {
    fit <- tryCatch(
      wk31_fit(inner_train, pos, grid[i, , drop = FALSE],
               seed = seed + i * 13L + val_year),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    pred <- wk31_predict(fit, inner_val)
    mm <- wk31_weighted_metrics(inner_val$weekly_fppg, pred, wv)
    mae[i] <- mm[["MAE"]]
    rmse[i] <- mm[["RMSE"]]
  }
  score <- mae + 0.10 * rmse
  best <- if (any(is.finite(score))) which.min(replace(score, !is.finite(score), Inf)) else 1L
  grid$inner_MAE <- mae
  grid$inner_RMSE <- rmse
  grid$selected <- seq_len(nrow(grid)) == best
  grid
}

wk31_core_metrics <- function(actual, pred) {
  wk25_core_metrics(actual, pred)
}

wk31_cohort_metrics <- function(d, pred_col, base_col = NULL) {
  masks <- list(
    All = rep(TRUE, nrow(d)),
    Relevant = if ("relevant_cohort" %in% names(d)) wk25_bool(d$relevant_cohort) else rep(TRUE, nrow(d)),
    Starter = if ("starter_cohort" %in% names(d)) wk25_bool(d$starter_cohort) else rep(FALSE, nrow(d))
  )
  rows <- lapply(names(masks), function(lbl) {
    z <- d[masks[[lbl]], , drop = FALSE]
    cm <- wk31_core_metrics(z$actual_fppg, z[[pred_col]])
    out <- cbind(data.frame(cohort = lbl, stringsAsFactors = FALSE), cm)
    if (!is.null(base_col)) {
      bm <- wk31_core_metrics(z$actual_fppg, z[[base_col]])
      names(bm) <- paste0("base_", names(bm))
      names(out)[names(out) %in% c("n","MAE","RMSE","correlation","rank_correlation")] <-
        paste0("candidate_", names(out)[names(out) %in% c("n","MAE","RMSE","correlation","rank_correlation")])
      out <- cbind(out, bm)
    }
    out
  })
  dplyr::bind_rows(rows)
}

wk31_decision_score <- function(d, alpha) {
  if (!nrow(d)) return(Inf)
  cand <- pmax(0, wk31_num(d$production_base_31) +
                 alpha * (wk31_num(d$direct_fppg_31) - wk31_num(d$production_base_31)))
  tmp <- d
  tmp$.candidate31 <- cand
  m <- wk31_cohort_metrics(tmp, ".candidate31", "production_base_31")
  ratio <- function(cohort, metric) {
    z <- m[m$cohort == cohort, , drop = FALSE]
    if (!nrow(z)) return(1)
    b <- z[[paste0("base_", metric)]][1]
    c_ <- z[[paste0("candidate_", metric)]][1]
    if (!is.finite(b) || b <= 0 || !is.finite(c_)) 1 else c_ / b
  }
  0.45 * ratio("Starter", "MAE") +
    0.30 * ratio("Relevant", "MAE") +
    0.15 * ratio("All", "MAE") +
    0.05 * ratio("Starter", "RMSE") +
    0.05 * ratio("Relevant", "RMSE")
}

wk31_select_alpha <- function(prior_oof, pos, final = FALSE) {
  if (!nrow(prior_oof)) return(0.10)
  cap <- if (final) as.numeric(MODEL31_FINAL_ALPHA_CAP[[pos]]) else 0.35
  grid <- MODEL31_ALPHA_GRID[MODEL31_ALPHA_GRID <= cap + 1e-9]
  scores <- vapply(grid, function(a) wk31_decision_score(prior_oof, a), numeric(1))
  if (!any(is.finite(scores))) return(0)
  best <- min(scores[is.finite(scores)])
  near <- grid[is.finite(scores) & scores <= best + 0.001]
  if (!length(near)) 0 else min(near)
}

wk31_bootstrap_gain <- function(d, cohort = "Starter", reps = MODEL31_BOOTSTRAP_REPS, seed = 42L) {
  if (cohort == "Starter" && "starter_cohort" %in% names(d)) d <- d[wk25_bool(d$starter_cohort), , drop = FALSE]
  if (cohort == "Relevant" && "relevant_cohort" %in% names(d)) d <- d[wk25_bool(d$relevant_cohort), , drop = FALSE]
  if (!nrow(d)) return(data.frame(cohort = cohort, mean_MAE_gain = NA_real_, p_improves = NA_real_,
                                  lower_90 = NA_real_, upper_90 = NA_real_))
  d$.gain <- abs(wk31_num(d$actual_fppg) - wk31_num(d$production_base_31)) -
    abs(wk31_num(d$actual_fppg) - wk31_num(d$candidate_fppg_31))
  clusters <- unique(paste(d$season, d$week, sep = "::"))
  set.seed(seed)
  sims <- numeric(reps)
  for (b in seq_len(reps)) {
    draw <- sample(clusters, length(clusters), replace = TRUE)
    idx <- unlist(lapply(draw, function(k) which(paste(d$season, d$week, sep = "::") == k)),
                  use.names = FALSE)
    sims[b] <- mean(d$.gain[idx], na.rm = TRUE)
  }
  data.frame(
    cohort = cohort,
    mean_MAE_gain = mean(d$.gain, na.rm = TRUE),
    p_improves = mean(sims > 0, na.rm = TRUE),
    lower_90 = as.numeric(stats::quantile(sims, .05, na.rm = TRUE)),
    upper_90 = as.numeric(stats::quantile(sims, .95, na.rm = TRUE))
  )
}

wk31_promotion_gate <- function(d, pos) {
  m <- wk31_cohort_metrics(d, "candidate_fppg_31", "production_base_31")
  get <- function(cohort, prefix, metric) {
    z <- m[m$cohort == cohort, , drop = FALSE]
    if (!nrow(z)) return(NA_real_)
    z[[paste0(prefix, "_", metric)]][1]
  }
  gain <- function(cohort, metric) {
    b <- get(cohort, "base", metric)
    c_ <- get(cohort, "candidate", metric)
    if (!is.finite(b) || b <= 0 || !is.finite(c_)) NA_real_ else 100 * (b - c_) / b
  }

  years <- sort(unique(as.integer(d$season)))
  yr_gain <- vapply(years, function(yy) {
    starter_mask <- if ("starter_cohort" %in% names(d)) wk25_bool(d$starter_cohort) else rep(FALSE, nrow(d))
    z <- d[as.integer(d$season) == yy & starter_mask, , drop = FALSE]
    if (!nrow(z)) return(NA_real_)
    b <- mean(abs(wk31_num(z$actual_fppg) - wk31_num(z$production_base_31)))
    c_ <- mean(abs(wk31_num(z$actual_fppg) - wk31_num(z$candidate_fppg_31)))
    100 * (b - c_) / pmax(b, 1e-9)
  }, numeric(1))
  need_wins <- if (length(years) >= 3) 2L else 1L
  boot <- wk31_bootstrap_gain(d, "Starter", seed = 3100L + match(pos, c("QB","RB","WR","TE")))

  checks <- c(
    starter_MAE = is.finite(gain("Starter","MAE")) && gain("Starter","MAE") > 0,
    starter_RMSE = is.finite(gain("Starter","RMSE")) && gain("Starter","RMSE") >= -0.5,
    relevant_MAE = is.finite(gain("Relevant","MAE")) && gain("Relevant","MAE") >= 0,
    relevant_RMSE = is.finite(gain("Relevant","RMSE")) && gain("Relevant","RMSE") >= -0.5,
    all_MAE = is.finite(gain("All","MAE")) && gain("All","MAE") >= 0,
    year_stability = sum(yr_gain > 0, na.rm = TRUE) >= need_wins,
    paired_confidence = is.finite(boot$p_improves[1]) && boot$p_improves[1] >= 0.55
  )
  data.frame(
    position = pos,
    promoted_for_2026 = all(checks),
    starter_MAE_gain_pct = gain("Starter","MAE"),
    starter_RMSE_gain_pct = gain("Starter","RMSE"),
    relevant_MAE_gain_pct = gain("Relevant","MAE"),
    relevant_RMSE_gain_pct = gain("Relevant","RMSE"),
    all_MAE_gain_pct = gain("All","MAE"),
    starter_bootstrap_p_improves = boot$p_improves[1],
    failed_checks = paste(names(checks)[!checks], collapse = ";"),
    stringsAsFactors = FALSE
  )
}
