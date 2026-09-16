# Fast current project refresh: preserve the validated season foundation and
# rebuild the 2.2 weekly state/models/projections from cached PBP context.
if (file.exists("data/raw/weekly_defense_context_raw.csv")) {
  source("MOBILE_RUN_WEEKLY_FAST.R")
} else {
  source("MOBILE_RUN_WEEKLY.R")
}
