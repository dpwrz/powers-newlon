# Validate final-forecast error feedback before allowing it into 2026 production.
.fm25_feedback_root <- function() {
  p <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  candidates <- character()
  for (i in 0:6) { candidates <- c(candidates, p); q <- dirname(p); if (identical(q, p)) break; p <- q }
  script_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(script_path) && nzchar(script_path)) {
    d <- dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)); candidates <- c(d, dirname(d), candidates)
  }
  for (x in unique(candidates)) if (file.exists(file.path(x, "config.R")) && dir.exists(file.path(x, "pipeline"))) return(x)
  stop("Could not locate Fantasy Model root.")
}
setwd(.fm25_feedback_root())
source("pipeline/25_train_validate_error_feedback_2_5.R")
