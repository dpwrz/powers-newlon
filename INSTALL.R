# Fantasy Model 3.0 - clean-install dependency bootstrap
.fm30_root <- function() {
  script_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  candidates <- c(getwd(), dirname(getwd()))
  if (!is.null(script_path) && length(script_path) == 1 && nzchar(script_path)) {
    d <- dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
    candidates <- c(d, dirname(d), candidates)
  }
  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  for (p in candidates) {
    if (file.exists(file.path(p, "config.R")) && dir.exists(file.path(p, "R")) && dir.exists(file.path(p, "pipeline"))) return(p)
  }
  stop("Could not locate Fantasy Model 3.0 project root.")
}
root <- .fm30_root(); setwd(root)
source("config.R")
core <- c(
  "dplyr", "readr", "tidyr", "tibble", "purrr", "janitor", "jsonlite",
  "nflreadr", "rpart", "xgboost", "ranger", "slider", "httr2", "shiny"
)
ensure_packages(core)
dir.create("data/state", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create("models", recursive = TRUE, showWarnings = FALSE)
cat("\n[3.0] R dependencies are ready.\n")
cat("[3.0] Next: source(\"RUN_FIRST_TIME_SETUP.R\")\n")
