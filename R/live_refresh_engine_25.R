# ============================================================
# FANTASY MODEL 2.5 - LIVE REFRESH / ACCURACY SUPPORT
# ============================================================
# Small, production-safe helpers used by the automatic refresh layer.

live25_chr <- function(x, default = "") {
  y <- as.character(x)
  y[is.na(y)] <- default
  y
}

live25_num <- function(x, default = NA_real_) {
  y <- suppressWarnings(as.numeric(x))
  if (!is.na(default)) y[!is.finite(y)] <- default
  y
}

live25_bool <- function(x, default = FALSE) {
  if (is.logical(x)) { x[is.na(x)] <- default; return(x) }
  z <- tolower(trimws(as.character(x)))
  out <- z %in% c("true", "t", "1", "yes", "y")
  out[is.na(z) | !nzchar(z)] <- default
  out
}

live25_first_col <- function(d, choices, default = NA) {
  hit <- choices[choices %in% names(d)]
  if (!length(hit)) return(rep(default, nrow(d)))
  d[[hit[[1]]]]
}

live25_md5_objects <- function(...) {
  objects <- list(...)
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(objects, tmp, compress = FALSE)
  unname(tools::md5sum(tmp))
}

live25_compact_stats <- function(x) {
  if (is.null(x) || !nrow(x)) return(data.frame())
  keep <- intersect(c(
    "season", "week", "player_id", "player_display_name", "position", "recent_team", "team",
    "completions", "attempts", "passing_yards", "passing_tds", "passing_interceptions",
    "carries", "rushing_yards", "rushing_tds", "targets", "receptions", "receiving_yards",
    "receiving_tds", "fumbles_lost", "fantasy_points", "fantasy_points_ppr"
  ), names(x))
  if (!length(keep)) return(as.data.frame(x))
  z <- as.data.frame(x[, keep, drop = FALSE])
  ord <- intersect(c("season", "week", "player_id"), names(z))
  if (length(ord)) z <- z[do.call(order, z[ord]), , drop = FALSE]
  z
}

live25_compact_schedule <- function(x) {
  if (is.null(x) || !nrow(x)) return(data.frame())
  keep <- intersect(c(
    "season", "week", "game_id", "game_type", "gameday", "gametime", "weekday",
    "home_team", "away_team", "home_score", "away_score", "result",
    "spread_line", "total_line", "home_moneyline", "away_moneyline", "roof", "surface"
  ), names(x))
  z <- as.data.frame(x[, keep, drop = FALSE])
  ord <- intersect(c("season", "week", "game_id"), names(z))
  if (length(ord)) z <- z[do.call(order, z[ord]), , drop = FALSE]
  z
}

live25_completed_games <- function(schedule) {
  if (is.null(schedule) || !nrow(schedule)) return(0L)
  result <- live25_num(live25_first_col(schedule, c("result")))
  if (any(is.finite(result))) return(as.integer(sum(is.finite(result))))
  hs <- live25_num(live25_first_col(schedule, c("home_score")))
  as <- live25_num(live25_first_col(schedule, c("away_score")))
  as.integer(sum(is.finite(hs) & is.finite(as)))
}

live25_latest_actual_week <- function(stats) {
  if (is.null(stats) || !nrow(stats) || !"week" %in% names(stats)) return(0L)
  w <- live25_num(stats$week)
  w <- w[is.finite(w)]
  if (!length(w)) 0L else as.integer(max(w))
}

live25_read_json <- function(path) {
  if (!file.exists(path) || !requireNamespace("jsonlite", quietly = TRUE)) return(list())
  tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE), error = function(e) list())
}

live25_write_json <- function(x, path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite is required by the live refresh layer.")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  invisible(path)
}

live25_append_log <- function(path, row) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  old <- if (file.exists(path)) tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else data.frame()
  for (nm in intersect(names(old), names(row))) {
    if (is.character(old[[nm]]) || is.character(row[[nm]])) {
      old[[nm]] <- as.character(old[[nm]])
      row[[nm]] <- as.character(row[[nm]])
    }
  }
  readr::write_csv(dplyr::bind_rows(old, row), path)
  invisible(path)
}

live25_force_refresh <- function() {
  tolower(trimws(Sys.getenv("FM_FORCE_REFRESH", "false"))) %in% c("1", "true", "t", "yes", "y")
}
