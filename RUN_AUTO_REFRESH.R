# Fantasy Model 3.0 - normal production refresh
source("runners/RUN_3_0_AUTO_REFRESH.R")

# Persist the browser-facing aliases in the snapshot after the model exporter
# finishes. This keeps Web 1.1 compatibility work on the producer side instead
# of reparsing and rewriting the full JSON in every browser session.
if (file.exists("scripts/normalize_web_snapshot.R") && file.exists("output/model_snapshot.json")) {
  source("scripts/normalize_web_snapshot.R")
  fm_normalize_web_snapshot("output/model_snapshot.json")
}
