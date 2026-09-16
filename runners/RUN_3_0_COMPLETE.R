# ============================================================
# FANTASY MODEL 3.0 - DYNASTY INTELLIGENCE BOOTSTRAP
# ============================================================
# ---- 3.0 project-root bootstrap (Posit/RStudio safe) ----
.fm3_bootstrap_root <- function() {
  candidates <- character()
  frames <- sys.frames()
  if (length(frames)) {
    for (fr in frames) {
      of <- fr$ofile
      if (!is.null(of) && length(of) == 1L && !is.na(of) && nzchar(of)) {
        f <- tryCatch(normalizePath(of, winslash = "/", mustWork = TRUE), error = function(e) NA_character_)
        if (!is.na(f)) {
          d <- dirname(f)
          candidates <- c(candidates, d, dirname(d))
        }
      }
    }
  }
  wd <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
  p <- wd
  for (i in 0:5) {
    candidates <- c(candidates, p)
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  candidates <- c(candidates, tryCatch(list.dirs(wd, recursive = FALSE, full.names = TRUE), error = function(e) character()))
  candidates <- unique(candidates[nzchar(candidates)])
  ok <- vapply(candidates, function(x) {
    file.exists(file.path(x, "config.R")) && file.exists(file.path(x, "VERSION.txt")) &&
      dir.exists(file.path(x, "runners")) && dir.exists(file.path(x, "pipeline"))
  }, logical(1))
  hits <- candidates[ok]
  if (!length(hits)) stop("Fantasy Model 3.0 project root not found. Set working directory to the project folder first.")
  root <- normalizePath(hits[[1]], winslash = "/", mustWork = TRUE)
  if (!identical(normalizePath(getwd(), winslash = "/", mustWork = TRUE), root)) setwd(root)
  invisible(root)
}
.fm3_bootstrap_root()
# -----------------------------------------------------------

source("config.R")
base_required <- c(
  paste0("output/final_", CURRENT_SEASON, "_rankings.csv"),
  "output/final_dynasty_rankings.csv",
  paste0("output/weekly_", CURRENT_SEASON, "_projections.csv"),
  "data/processed/weekly_model_table_2_3.csv"
)
if (any(!file.exists(base_required))) {
  cat("[3.0] Required 2.4.3 outputs are missing. Running the current projection build first.\n")
  source("runners/RUN_2_4_COMPLETE.R")
} else {
  cat("[3.0] Existing 2.4.3 production outputs found; reusing them.\n")
}
source("runners/RUN_3_0_DATA_LAB.R")
source("pipeline/20_build_dynasty_intelligence_3_0.R")
cat("\nFantasy Model 3.0 bootstrap complete.\n")
cat("Next: source(\"runners/RUN_DYNASTY_APP.R\")\n")
