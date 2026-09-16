# Fantasy Model 3.0 production auto-refresh wrapper.
# Internal 2.5 filenames are preserved for validated model lineage.
Sys.setenv(FM_RELEASE_VERSION = "3.0.1")
source("runners/RUN_2_5_AUTO_REFRESH.R")
