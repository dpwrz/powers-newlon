# Fantasy Model 3.1 - normal production refresh
source("runners/RUN_3_1_AUTO_REFRESH.R")

# Persist browser-facing aliases after the model exporter finishes.
if (file.exists("scripts/normalize_web_snapshot.R") && file.exists("output/model_snapshot.json")) {
  source("scripts/normalize_web_snapshot.R")
  fm_normalize_web_snapshot("output/model_snapshot.json")
}
