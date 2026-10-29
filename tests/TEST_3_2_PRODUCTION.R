# ============================================================
# MODEL 3.2 - FAST PRODUCTION ROUTING CHECK
# ============================================================
# This test intentionally does NOT rebuild the production projection pipeline.
# It exercises the exact shared routing helper against the current saved
# projection table and trained 3.2 model artifacts.

source("config.R")
ensure_packages(c("dplyr","readr","xgboost"))
source("R/weekly_engine_31.R")
source("R/weekly_engine_32.R")

promotion_path <- "output/weekly_3_2_promotion.csv"
residual_path <- "output/weekly_3_2_residual_pool.csv"
weekly_path <- paste0("output/weekly_", CURRENT_SEASON, "_projections.csv")

stopifnot(file.exists(promotion_path), file.exists(weekly_path))
promotion <- readr::read_csv(promotion_path, show_col_types = FALSE, progress = FALSE)
residual <- if (file.exists(residual_path)) {
  readr::read_csv(residual_path, show_col_types = FALSE, progress = FALSE)
} else data.frame()
d <- readr::read_csv(weekly_path, show_col_types = FALSE, progress = FALSE)

stopifnot(all(c("position","player_id","week","projected_weekly_fppg","is_actual") %in% names(d)))
d$.row_id_32_test <- seq_len(nrow(d))
before <- d[, c(".row_id_32_test","position","player_id","week","projected_weekly_fppg","is_actual")]

out <- wk32_apply_promoted_layer(
  future_pred = d,
  promotion32 = promotion,
  residual32 = residual,
  positions = POSITIONS,
  model_dir = "models"
)

stopifnot(nrow(out) == nrow(d))
stopifnot(all(c(
  "weekly_live_model_used_32","weekly_live_direct_fppg_32",
  "weekly_live_alpha_32","weekly_live_variant_32"
) %in% names(out)))

bool <- function(x) toupper(as.character(x)) %in% c("TRUE","T","1","YES","Y")
out <- out[order(out$.row_id_32_test), , drop = FALSE]
before <- before[order(before$.row_id_32_test), , drop = FALSE]
unplayed <- suppressWarnings(as.numeric(out$is_actual)) == 0
played <- !unplayed

# Completed player-weeks must remain frozen.
if (any(played)) {
  stopifnot(isTRUE(all.equal(
    suppressWarnings(as.numeric(out$projected_weekly_fppg[played])),
    suppressWarnings(as.numeric(before$projected_weekly_fppg[played])),
    tolerance = 1e-12, check.attributes = FALSE
  )))
}

for (pos in POSITIONS) {
  pr <- promotion[promotion$position == pos, , drop = FALSE]
  promoted <- nrow(pr) && "promoted_32" %in% names(pr) && bool(pr$promoted_32[1])
  idx <- out$position == pos & unplayed
  if (!any(idx)) next

  used <- bool(out$weekly_live_model_used_32[idx])
  if (promoted) {
    stopifnot(file.exists(paste0("models/weekly_3_2_live_", pos, ".json")))
    stopifnot(file.exists(paste0("models/weekly_3_2_live_", pos, "_meta.rds")))
    stopifnot(any(used))
    stopifnot(all(is.finite(suppressWarnings(as.numeric(out$weekly_live_direct_fppg_32[idx][used])))))
    stopifnot(all(suppressWarnings(as.numeric(out$weekly_live_alpha_32[idx][used])) > 0))

    changed <- abs(
      suppressWarnings(as.numeric(out$projected_weekly_fppg[idx])) -
      suppressWarnings(as.numeric(before$projected_weekly_fppg[idx]))
    ) > 1e-10
    stopifnot(any(changed & used))
  } else {
    stopifnot(!any(used))
    stopifnot(isTRUE(all.equal(
      suppressWarnings(as.numeric(out$projected_weekly_fppg[idx])),
      suppressWarnings(as.numeric(before$projected_weekly_fppg[idx])),
      tolerance = 1e-12, check.attributes = FALSE
    )))
  }
}

cat("Model 3.2 fast production routing checks passed.\n")
