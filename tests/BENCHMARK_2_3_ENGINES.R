# ============================================================
# FANTASY MODEL 2.3 - OPTIONAL WEEKLY ML ENGINE BENCHMARK
# ============================================================
# This script never changes production models. It compares optional engines on
# the same outer season walk-forward used by the 2.3 full-direct base challenger.
source("config.R")
ensure_packages(c("dplyr", "readr", "rpart"))
source("R/weekly_engine.R")
source("R/weekly_engine_22.R")

path <- if (file.exists("data/processed/weekly_model_table_2_3.csv")) "data/processed/weekly_model_table_2_3.csv" else "data/processed/weekly_model_table_2_2.csv"
if (!file.exists(path)) stop("Run 09_build_weekly_data.R before the engine benchmark.")
df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

engines <- "rpart"
if (requireNamespace("ranger", quietly = TRUE)) engines <- c(engines, "ranger")
if (requireNamespace("xgboost", quietly = TRUE)) engines <- c(engines, "xgboost")
cat("[2.3 ENGINE BENCHMARK] Available engines: ", paste(engines, collapse = ", "), "\n", sep = "")
if (length(engines) == 1) cat("[2.3 ENGINE BENCHMARK] Optional ranger/xgboost are not installed; reporting rpart reference only.\n")

fit_engine22_bench <- function(train, features, engine, seed) {
  X <- prepare_feature_frame(train, features)
  y <- wk_num(train$weekly_fppg)
  keep <- is.finite(y)
  X <- X[keep, , drop = FALSE]; y <- y[keep]
  if (engine == "rpart") return(list(engine = engine, fit = fit_fantasy_model(train[keep, , drop = FALSE], features, "weekly_fppg", seed = seed, n_trees = WEEKLY_22_VALIDATION_TREES), features = features))
  dat <- X; dat$.target <- y
  if (engine == "ranger") {
    fit <- ranger::ranger(
      .target ~ ., data = dat, num.trees = 500,
      mtry = max(2, floor(sqrt(length(features)))), min.node.size = 8,
      sample.fraction = .85, replace = TRUE, seed = seed,
      importance = "impurity", num.threads = 1
    )
    return(list(engine = engine, fit = fit, features = features))
  }
  if (engine == "xgboost") {
    mat <- as.matrix(X)
    fit <- xgboost::xgboost(
      data = mat, label = y, objective = "reg:squarederror",
      nrounds = 350, max_depth = 4, eta = .03,
      subsample = .85, colsample_bytree = .80,
      min_child_weight = 8, lambda = 1, alpha = 0,
      nthread = 1, verbose = 0, seed = seed
    )
    return(list(engine = engine, fit = fit, features = features))
  }
  stop("Unknown engine: ", engine)
}

predict_engine22_bench <- function(object, new_data) {
  X <- prepare_feature_frame(new_data, object$features)
  if (object$engine == "rpart") return(pmax(0, predict_fantasy_model(object$fit, new_data)))
  if (object$engine == "ranger") return(pmax(0, as.numeric(predict(object$fit, data = X)$predictions)))
  if (object$engine == "xgboost") return(pmax(0, as.numeric(predict(object$fit, as.matrix(X)))))
  stop("Unknown engine: ", object$engine)
}

available_years <- sort(unique(as.integer(df$season)))
validation_years <- utils::tail(available_years[available_years <= TRAIN_END], WEEKLY_23_VALIDATION_YEARS)
rows <- list()
for (pos in POSITIONS) {
  feats <- get_features22(pos, "full", names(df))
  for (yr in validation_years) {
    tr <- df |> dplyr::filter(position == pos, season < yr)
    te <- df |> dplyr::filter(position == pos, season == yr)
    if (nrow(tr) < WEEKLY_22_MIN_TRAIN_ROWS || nrow(te) < 15) next
    for (engine in engines) {
      cat("[2.3 ENGINE BENCHMARK] ", yr, " ", pos, " ", engine, "\n", sep = "")
      obj <- tryCatch(fit_engine22_bench(tr, feats, engine, seed = SEED + yr * 100 + match(pos, POSITIONS) * 10 + match(engine, engines)), error = function(e) {
        warning(engine, " failed for ", pos, " ", yr, ": ", conditionMessage(e)); NULL
      })
      if (is.null(obj)) next
      pred <- predict_engine22_bench(obj, te)
      rows[[length(rows) + 1]] <- data.frame(
        season = yr, position = pos, engine = engine,
        actual_fppg = wk_num(te$weekly_fppg), prediction = pred,
        stringsAsFactors = FALSE
      )
      rm(obj); invisible(gc(full = TRUE))
    }
  }
}

preds <- dplyr::bind_rows(rows)
if (nrow(preds) == 0) stop("No engine benchmark predictions were produced.")
metrics <- preds |>
  dplyr::group_by(position, engine) |>
  dplyr::summarise(
    n = dplyr::n(),
    MAE = mean(abs(prediction - actual_fppg), na.rm = TRUE),
    RMSE = sqrt(mean((prediction - actual_fppg)^2, na.rm = TRUE)),
    correlation = safe_cor21(prediction, actual_fppg),
    rank_correlation = safe_cor21(prediction, actual_fppg, "spearman"),
    bias = mean(prediction - actual_fppg, na.rm = TRUE), .groups = "drop"
  ) |>
  dplyr::group_by(position) |>
  dplyr::arrange(MAE, RMSE, .by_group = TRUE) |>
  dplyr::mutate(engine_rank = dplyr::row_number()) |>
  dplyr::ungroup()

readr::write_csv(preds, "output/weekly_2_3_engine_benchmark_predictions.csv")
readr::write_csv(metrics, "output/weekly_2_3_engine_benchmark.csv")
cat("\n[2.3 ENGINE BENCHMARK] Complete: output/weekly_2_3_engine_benchmark.csv\n")
print(metrics)
