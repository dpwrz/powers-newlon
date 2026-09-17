# ============================================================
# FANTASY MODEL 3.1 - COMPONENT ERROR ATTRIBUTION
# ============================================================
# Diagnostic only. It does not change production projections.

source("config.R")
ensure_packages(c("dplyr", "readr"))
source("R/weekly_engine_31.R")

weekly_path <- "data/processed/weekly_model_table_2_3.csv"
val_path <- "output/weekly_2_5_validation_predictions.csv"
fb_path <- "output/weekly_2_5_feedback_validation_predictions.csv"
fb_promo_path <- "output/weekly_2_5_feedback_promotion.csv"
manifest_path <- "output/weekly_2_5_champion_manifest.csv"

req <- c(weekly_path, val_path, manifest_path)
miss <- req[!file.exists(req)]
if (length(miss)) stop("3.1 component audit missing prerequisite(s): ", paste(miss, collapse = ", "))

weekly <- readr::read_csv(weekly_path, show_col_types = FALSE, progress = FALSE)
val <- readr::read_csv(val_path, show_col_types = FALSE, progress = FALSE)
manifest <- readr::read_csv(manifest_path, show_col_types = FALSE, progress = FALSE)

promo_map <- setNames(wk25_bool(manifest$promoted_for_2026), as.character(manifest$position))
val$production_base_31 <- ifelse(
  promo_map[as.character(val$position)] %in% TRUE,
  wk31_num(val$candidate_fppg_25),
  wk31_num(val$production_base_25)
)

if (file.exists(fb_path) && file.exists(fb_promo_path)) {
  fb <- readr::read_csv(fb_path, show_col_types = FALSE, progress = FALSE)
  fp <- readr::read_csv(fb_promo_path, show_col_types = FALSE, progress = FALSE)
  fp_map <- setNames(wk25_bool(fp$promoted_for_2026), as.character(fp$position))
  keep <- intersect(c("season","week","player_id","position",
                      "feedback_base_projection_25","feedback_post_projection_25"), names(fb))
  fb2 <- fb[, keep, drop = FALSE]
  names(fb2)[names(fb2) == "feedback_base_projection_25"] <- "feedback_base_31"
  names(fb2)[names(fb2) == "feedback_post_projection_25"] <- "feedback_post_31"
  val <- dplyr::left_join(val, fb2, by = c("season","week","player_id","position"))
  use_fb <- fp_map[as.character(val$position)] %in% TRUE & is.finite(wk31_num(val$feedback_post_31, NA_real_))
  val$production_base_31[use_fb] <- wk31_num(val$feedback_post_31[use_fb])
}

keys <- c("season","week","player_id","position")
wk_keep <- unique(c(
  keys, "weekly_fppg", "targets", "carries", "pass_attempts", "attempts",
  "receiving_yards", "rushing_yards", "passing_yards",
  "receiving_tds", "rushing_tds", "passing_tds",
  "offense_pct", "target_share_week", "carry_share_week", "pass_attempt_share_week",
  "roll3_targets", "roll3_carries", "roll3_pass_attempts",
  "roll3_ypt", "roll3_rush_ypc", "roll3_pass_ypa",
  "roll3_rec_td_rate", "roll3_rush_td_rate", "roll3_pass_td_rate",
  "roll3_offense_pct", "roll3_target_share", "roll3_carry_share",
  "roll3_pass_attempt_share", "injury_risk", "practice_risk"
))
wk_keep <- intersect(wk_keep, names(weekly))
d <- dplyr::left_join(val, weekly[, wk_keep, drop = FALSE], by = keys, suffix = c("", "_hist"))

if (!"actual_fppg" %in% names(d)) {
  if ("weekly_fppg" %in% names(d)) d$actual_fppg <- wk31_num(d$weekly_fppg, NA_real_)
  else stop("3.1 component audit requires actual_fppg or weekly_fppg.")
}

numcol <- function(name, default = 0) {
  if (name %in% names(d)) wk31_num(d[[name]], default) else rep(default, nrow(d))
}
attempts <- if ("pass_attempts" %in% names(d)) numcol("pass_attempts") else numcol("attempts")
r3_attempts <- numcol("roll3_pass_attempts")
pos <- as.character(d$position)

d$forecast_error_31 <- wk31_num(d$actual_fppg, NA_real_) - wk31_num(d$production_base_31, NA_real_)
d$abs_error_31 <- abs(d$forecast_error_31)

d$actual_volume_31 <- ifelse(pos == "QB", attempts,
  ifelse(pos == "RB", numcol("carries") + numcol("targets"), numcol("targets")))
d$expected_volume_31 <- ifelse(pos == "QB", r3_attempts,
  ifelse(pos == "RB", numcol("roll3_carries") + numcol("roll3_targets"), numcol("roll3_targets")))
d$volume_surprise_31 <- d$actual_volume_31 - d$expected_volume_31

d$actual_efficiency_31 <- ifelse(
  pos == "QB", numcol("passing_yards") / pmax(attempts, 1),
  ifelse(pos == "RB",
    (numcol("rushing_yards") + numcol("receiving_yards")) /
      pmax(numcol("carries") + numcol("targets"), 1),
    numcol("receiving_yards") / pmax(numcol("targets"), 1)
  )
)
d$expected_efficiency_31 <- ifelse(
  pos == "QB", numcol("roll3_pass_ypa"),
  ifelse(pos == "RB",
    0.65 * numcol("roll3_rush_ypc") + 0.35 * numcol("roll3_ypt"),
    numcol("roll3_ypt")
  )
)
d$efficiency_surprise_31 <- d$actual_efficiency_31 - d$expected_efficiency_31

actual_td <- numcol("passing_tds") + numcol("rushing_tds") + numcol("receiving_tds")
expected_td <- ifelse(
  pos == "QB", r3_attempts * numcol("roll3_pass_td_rate"),
  ifelse(pos == "RB",
    numcol("roll3_carries") * numcol("roll3_rush_td_rate") +
      numcol("roll3_targets") * numcol("roll3_rec_td_rate"),
    numcol("roll3_targets") * numcol("roll3_rec_td_rate")
  )
)
d$td_surprise_31 <- actual_td - expected_td

actual_role <- if ("offense_pct" %in% names(d)) numcol("offense_pct") else
  ifelse(pos == "QB", numcol("pass_attempt_share_week"),
         ifelse(pos == "RB", pmax(numcol("carry_share_week"), numcol("target_share_week")),
                numcol("target_share_week")))
expected_role <- if ("roll3_offense_pct" %in% names(d)) numcol("roll3_offense_pct") else
  ifelse(pos == "QB", numcol("roll3_pass_attempt_share"),
         ifelse(pos == "RB", pmax(numcol("roll3_carry_share"), numcol("roll3_target_share")),
                numcol("roll3_target_share")))
d$role_surprise_31 <- actual_role - expected_role
d$availability_surprise_31 <- pmax(0, expected_role - actual_role) +
  0.25 * numcol("injury_risk") + 0.15 * numcol("practice_risk")

robust_scale <- function(x) {
  x <- wk31_num(x, NA_real_)
  med <- stats::median(x, na.rm = TRUE)
  sc <- stats::mad(x, center = med, constant = 1.4826, na.rm = TRUE)
  if (!is.finite(sc) || sc < 1e-9) sc <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(sc) || sc < 1e-9) sc <- 1
  abs(x - med) / sc
}

parts <- split(seq_len(nrow(d)), d$position)
d$primary_driver_31 <- "residual/variance"
for (p in names(parts)) {
  idx <- parts[[p]]
  score <- cbind(
    volume = robust_scale(d$volume_surprise_31[idx]),
    efficiency = robust_scale(d$efficiency_surprise_31[idx]),
    touchdown = robust_scale(d$td_surprise_31[idx]),
    role = robust_scale(d$role_surprise_31[idx]),
    availability = robust_scale(d$availability_surprise_31[idx])
  )
  mx <- max.col(score, ties.method = "first")
  driver <- colnames(score)[mx]
  max_score <- score[cbind(seq_len(nrow(score)), mx)]
  driver[!is.finite(max_score) | max_score < 1] <- "residual/variance"
  d$primary_driver_31[idx] <- driver
}

corr_safe <- function(x, y) {
  x <- wk31_num(x, NA_real_); y <- wk31_num(y, NA_real_)
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 20 || stats::sd(x[keep]) == 0 || stats::sd(y[keep]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(abs(x[keep]), y[keep], method = "spearman"))
}

summary_rows <- list()
for (p in unique(as.character(d$position))) {
  z <- d[d$position == p, , drop = FALSE]
  for (cohort in c("All","Relevant","Starter")) {
    mask <- rep(TRUE, nrow(z))
    if (cohort == "Relevant" && "relevant_cohort" %in% names(z)) mask <- wk25_bool(z$relevant_cohort)
    if (cohort == "Starter" && "starter_cohort" %in% names(z)) mask <- wk25_bool(z$starter_cohort)
    zz <- z[mask, , drop = FALSE]
    if (!nrow(zz)) next
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      position = p, cohort = cohort, n = nrow(zz),
      MAE = mean(zz$abs_error_31, na.rm = TRUE),
      RMSE = sqrt(mean(zz$forecast_error_31^2, na.rm = TRUE)),
      volume_error_association = corr_safe(zz$volume_surprise_31, zz$abs_error_31),
      efficiency_error_association = corr_safe(zz$efficiency_surprise_31, zz$abs_error_31),
      td_error_association = corr_safe(zz$td_surprise_31, zz$abs_error_31),
      role_error_association = corr_safe(zz$role_surprise_31, zz$abs_error_31),
      availability_error_association = corr_safe(zz$availability_surprise_31, zz$abs_error_31),
      stringsAsFactors = FALSE
    )
  }
}

driver_summary <- d |>
  dplyr::count(position, primary_driver_31, name = "n") |>
  dplyr::group_by(position) |>
  dplyr::mutate(share = n / sum(n)) |>
  dplyr::ungroup()

readr::write_csv(d, "output/weekly_3_1_component_error_rows.csv")
readr::write_csv(dplyr::bind_rows(summary_rows), "output/weekly_3_1_component_error_summary.csv")
readr::write_csv(driver_summary, "output/weekly_3_1_error_driver_share.csv")
cat("[3.1] Component error audit complete. Production unchanged.\n")
