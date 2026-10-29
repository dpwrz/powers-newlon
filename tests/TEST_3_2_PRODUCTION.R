# ============================================================
# MODEL 3.2 - PRODUCTION INTEGRATION CHECK
# ============================================================
source("config.R")
ensure_packages(c("dplyr","readr"))

promotion <- readr::read_csv("output/weekly_3_2_promotion.csv", show_col_types = FALSE, progress = FALSE)
weekly_path <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")
stopifnot(file.exists(weekly_path))
d <- readr::read_csv(weekly_path, show_col_types = FALSE, progress = FALSE)
stopifnot("weekly_live_model_used_32" %in% names(d))

bool <- function(x) toupper(as.character(x)) %in% c("TRUE","T","1","YES","Y")
unplayed <- suppressWarnings(as.numeric(d$is_actual)) == 0

for (pos in POSITIONS) {
  pr <- promotion[promotion$position == pos, , drop = FALSE]
  promoted <- nrow(pr) && bool(pr$promoted_32[1])
  z <- d[d$position == pos & unplayed, , drop = FALSE]
  if (!nrow(z)) next
  used <- bool(z$weekly_live_model_used_32)
  if (promoted) {
    stopifnot(any(used))
    stopifnot(file.exists(paste0("models/weekly_3_2_live_", pos, ".json")))
    stopifnot(file.exists(paste0("models/weekly_3_2_live_", pos, "_meta.rds")))
  } else {
    stopifnot(!any(used))
  }
}
cat("Model 3.2 production integration checks passed.\n")
