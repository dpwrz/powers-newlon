if (file.exists("data/raw/weekly_defense_context_raw.csv")) {
  source("MOBILE_RUN_WEEKLY_FAST.R")
} else {
  source("MOBILE_RUN_WEEKLY.R")
}
