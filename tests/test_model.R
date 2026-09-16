# ============================================================
# FANTASY MODEL 2.0 - OUTPUT SMOKE TEST
# ============================================================
source("config.R")
required <- c(
  "output/model_quality_report.txt",
  "output/validation_metrics_2_0.csv",
  "output/validation_2_0_model_comparison.csv",
  "output/selected_2_0_architecture_weights.csv",
  "output/opportunity_signal_correlations.csv",
  "output/shrinkage_signal_metrics.csv",
  paste0("output/", CURRENT_SEASON, "_projections_2_0.csv"),
  paste0("output/final_", CURRENT_SEASON, "_rankings.csv"),
  "output/final_dynasty_rankings.csv"
)
missing <- required[!file.exists(required)]
if (length(missing) > 0) stop("Missing Fantasy Model 2.0 output(s): ", paste(missing, collapse = ", "))

m <- read.csv("output/validation_metrics_2_0.csv", stringsAsFactors = FALSE)
w <- read.csv("output/selected_2_0_architecture_weights.csv", stringsAsFactors = FALSE)
p <- read.csv(paste0("output/", CURRENT_SEASON, "_projections_2_0.csv"), stringsAsFactors = FALSE)

if (!all(POSITIONS %in% m$position)) stop("Validation metrics do not contain every position.")
if (!all(POSITIONS %in% w$position)) stop("Architecture weights do not contain every position.")
if (!all(c("projected_fppg", "opportunity_weight", "projection_architecture") %in% names(p))) stop("2.0 projection columns are incomplete.")
if (any(!is.finite(p$projected_fppg))) stop("2.0 contains non-finite projected FPPG values.")
if (any(p$projected_fppg < 0)) stop("2.0 contains negative projected FPPG values.")

cat("\nFANTASY MODEL 2.0 SMOKE TEST PASSED\n")
cat("Projected players: ", nrow(p), "\n", sep = "")
cat("Selected architecture weights:\n")
print(w[, intersect(c("position", "selected_opportunity_weight", "selected_direct_weight", "validation_MAE", "validation_RMSE", "validation_correlation"), names(w)), drop = FALSE])
