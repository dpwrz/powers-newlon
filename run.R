# Fantasy Model 3.0 stable entrypoint.
.fm30_bootstrap_root <- function() {
  candidates <- c(getwd(), dirname(getwd()))
  frames <- sys.frames()
  if (length(frames)) {
    for (fr in frames) {
      of <- fr$ofile
      if (!is.null(of) && length(of) == 1L && !is.na(of) && nzchar(of)) {
        d <- dirname(normalizePath(of, winslash = "/", mustWork = FALSE))
        candidates <- c(d, dirname(d), candidates)
      }
    }
  }
  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  for (p in candidates) if (file.exists(file.path(p, "config.R")) && dir.exists(file.path(p, "runners"))) return(p)
  stop("Fantasy Model 3.0 project root not found.")
}
setwd(.fm30_bootstrap_root())
source("config.R")
required <- c(
  "output/weekly_2_5_champion_manifest.csv",
  paste0("models/weekly_2_5_direct_", POSITIONS, ".json"),
  paste0("models/weekly_2_5_direct_", POSITIONS, "_meta.rds")
)
if (any(!file.exists(required))) {
  cat("[3.0] First-run production artifacts missing. Starting one-time setup.\n")
  source("RUN_FIRST_TIME_SETUP.R")
} else {
  source("RUN_AUTO_REFRESH.R")
}
