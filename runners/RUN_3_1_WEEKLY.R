# Fantasy Model 3.1 - guarded weekly commit.
# Run after Monday night / official weekly data are available.
#
# Order is intentional:
#   1) refresh finalized actuals + accuracy;
#   2) append/finalize current-season training rows and train versioned challengers;
#   3) force a production projection pass using only challengers that passed gates.

source("runners/RUN_3_1_ACCURACY_REFRESH.R")

rscript <- file.path(R.home("bin"), "Rscript")

# Recover pre-existing 2026 weeks from timestamped Git snapshots when possible.
# The backfill is honest-by-construction: a row is accepted only if its commit
# existed before that player's team kickoff.
backfill_status <- system2(
  rscript,
  args = c("maintenance/BACKFILL_WEEKLY_LEARNING_FEATURES_FROM_GIT.R"),
  stdout = "",
  stderr = ""
)
if (!identical(as.integer(backfill_status), 0L)) {
  warning("Weekly learning Git-history backfill returned status ", backfill_status,
          "; continuing with already-frozen feature rows.")
}

learning_status <- system2(
  rscript,
  args = c("runners/RUN_3_1_WEEKLY_LEARNING.R"),
  stdout = "",
  stderr = ""
)
if (!identical(as.integer(learning_status), 0L)) {
  stop("Weekly learning subprocess failed with status ", learning_status)
}

Sys.setenv(
  FM_RELEASE_VERSION = "3.1",
  FM_FORCE_REFRESH = "true",
  FM_REFRESH_DEFENSE = "true",
  FM_REFRESH_DYNASTY = "true"
)
source("runners/RUN_3_1_AUTO_REFRESH.R")
