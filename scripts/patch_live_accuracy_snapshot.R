#!/usr/bin/env Rscript

# Patch only the live-accuracy section of the existing production snapshot.
# This avoids rebuilding every player projection when only completed-game
# accuracy metrics changed.

args <- if (interactive()) character() else commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1 && nzchar(args[[1]])) {
  normalizePath(args[[1]], mustWork = FALSE)
} else {
  normalizePath(Sys.getenv("FANTASY_MODEL_ROOT", unset = getwd()), mustWork = FALSE)
}
setwd(root)

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite is required.")

season <- suppressWarnings(as.integer(Sys.getenv("FM_SEASON", unset = "2026")))
if (!is.finite(season)) season <- 2026L

snapshot_path <- file.path("output", "model_snapshot.json")
if (!file.exists(snapshot_path)) stop("Missing production snapshot: ", snapshot_path)

read_rows <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
}
as_records <- function(d) {
  if (is.null(d) || !nrow(d)) return(list())
  jsonlite::fromJSON(
    jsonlite::toJSON(d, dataframe = "rows", na = "null", auto_unbox = TRUE, digits = NA),
    simplifyVector = FALSE
  )
}
atomic_json <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".json")
  jsonlite::write_json(x, tmp, auto_unbox = TRUE, na = "null", digits = NA, pretty = FALSE)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}

snap <- jsonlite::fromJSON(snapshot_path, simplifyVector = FALSE)
if (is.null(snap$quality) || !is.list(snap$quality)) snap$quality <- list()

out <- "output"
snap$quality$live_weekly <- as_records(read_rows(file.path(out, paste0("live_accuracy_weekly_summary_", season, ".csv"))))
snap$quality$live_position <- as_records(read_rows(file.path(out, paste0("live_accuracy_weekly_position_", season, ".csv"))))
snap$quality$live_cumulative <- as_records(read_rows(file.path(out, paste0("live_accuracy_cumulative_position_", season, ".csv"))))
snap$quality$live_overall <- as_records(read_rows(file.path(out, paste0("live_accuracy_cumulative_summary_", season, ".csv"))))
snap$quality$live_misses <- as_records(read_rows(file.path(out, paste0("live_accuracy_biggest_misses_", season, ".csv"))))
snap$quality$live_accuracy_generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

atomic_json(snap, snapshot_path)
cat("[WEB ACCURACY] Patched live accuracy into ", snapshot_path, "\n", sep = "")
