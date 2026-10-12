# Structural safety tests for Model 3.1 weekly online learning.
source("config.R")
source("R/weekly_engine_31.R")

stopifnot("extra_weight" %in% names(formals(wk31_fit)))

store_code <- paste(readLines("pipeline/30_update_online_training_3_1.R", warn = FALSE), collapse = "\n")
train_code <- paste(readLines("pipeline/31_train_weekly_online_3_1.R", warn = FALSE), collapse = "\n")
runner_code <- paste(readLines("runners/RUN_3_1_WEEKLY.R", warn = FALSE), collapse = "\n")

# Current-season rows must be admitted only through the honest accuracy audit.
stopifnot(grepl("inner_join\\(honest", store_code))
stopifnot(grepl("live_accuracy_player_weeks_", store_code, fixed = TRUE))

# Immutable historical source is never a write target in the online pipelines.
stopifnot(!grepl('write_csv\\([^\\n]*weekly_model_table_2_3', store_code))
stopifnot(!grepl('write_csv\\([^\\n]*weekly_model_table_2_3', train_code))
stopifnot(grepl("hist_md5_before", runner_code, fixed = TRUE))
stopifnot(grepl("hist_md5_after", runner_code, fixed = TRUE))

# Honest online validation: Week K can only train on current-season Weeks < K.
stopifnot(grepl("op\\$week < target_week", train_code))
stopifnot(grepl("Alpha for Week K is selected ONLY from Weeks < K", train_code, fixed = TRUE))

# A past cutoff model/snapshot is never overwritten.
stopifnot(grepl("if (file.exists(snapshot_path))", store_code, fixed = TRUE))
stopifnot(grepl("if (!file.exists(archive_model))", train_code, fixed = TRUE))
stopifnot(grepl("if (!file.exists(archive_meta))", train_code, fixed = TRUE))

cat("Model 3.1 weekly online-learning safety checks passed.\n")
