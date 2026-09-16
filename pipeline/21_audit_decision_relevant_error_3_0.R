# ============================================================
# STEP 21 - DECISION-RELEVANT ERROR AUDIT (PORTABLE / MEMORY SAFE)
# ============================================================
source("config.R")
ensure_packages(c("dplyr", "readr", "tibble"))

run_decision_relevant_audit_30 <- function() {
  dir.create("output", showWarnings = FALSE, recursive = TRUE)
  path <- "output/weekly_2_4_validation_predictions.csv"
  status_path <- "output/decision_relevant_audit_status_3_0.csv"

  # The portable 3.0 distribution intentionally omits the 2.4 validation-prediction
  # table (~66 MB uncompressed). The audit is diagnostic only and must never block
  # the dynasty application or the live 3.0 role forecast.
  if (!file.exists(path)) {
    readr::write_csv(
      tibble::tibble(
        full_audit_available = FALSE,
        status = "SKIPPED_MISSING_2_4_VALIDATION_PREDICTIONS",
        required_file = path,
        action = paste(
          "Dynasty build may continue. For the full starter/relevant/depth audit,",
          "restore weekly_2_4_validation_predictions.csv from the 2.4.3 full project",
          "or the optional 3.0 audit-data add-on; do not rerun the entire 2.4 pipeline just for this file."
        )
      ),
      status_path
    )

    # If compact 2.4 cohort metrics are present, preserve a useful lightweight
    # production summary even when row-level historical predictions are absent.
    cohort_path <- "output/weekly_2_4_cohort_metrics.csv"
    promotion_path <- "output/weekly_2_4_promotion.csv"
    if (file.exists(cohort_path)) {
      coh <- readr::read_csv(cohort_path, show_col_types = FALSE, progress = FALSE)
      prom <- if (file.exists(promotion_path)) {
        readr::read_csv(promotion_path, show_col_types = FALSE, progress = FALSE) |>
          dplyr::select(position, promoted_for_2026)
      } else {
        tibble::tibble(position = unique(coh$position), promoted_for_2026 = FALSE)
      }
      coh <- coh |> dplyr::left_join(prom, by = "position") |>
        dplyr::mutate(
          production_source = dplyr::if_else(promoted_for_2026 %in% TRUE, "2.4 challenger", "locked benchmark"),
          MAE = dplyr::if_else(promoted_for_2026 %in% TRUE, model24_MAE, locked_MAE),
          RMSE = dplyr::if_else(promoted_for_2026 %in% TRUE, model24_RMSE, locked_RMSE)
        ) |>
        dplyr::select(position, cohort, n, production_source, MAE, RMSE,
                      locked_MAE, model24_MAE, locked_RMSE, model24_RMSE,
                      promoted_for_2026)
      readr::write_csv(coh, "output/decision_relevant_error_3_0.csv")
      cat("[3.0 AUDIT] Full row-level 2.4 validation file is not present; wrote lightweight cohort audit instead.\n")
    } else {
      cat("[3.0 AUDIT] Optional full audit skipped: ", path, " is not present in the portable package.\n", sep = "")
    }
    cat("[3.0 AUDIT] This does NOT block the Dynasty Intelligence build.\n")
    return(invisible(FALSE))
  }

  # Read only the columns used by this audit. The original 2.4 validation file
  # contains 300+ fields; selecting ~25 fields materially reduces Posit Cloud RAM.
  d <- readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE,
    col_select = c(
      season, week, player_id, player_display_name, position, team, opponent,
      actual_fppg, week_phase_23, starter_cohort, relevant_cohort,
      injury_risk, practice_risk, snap_trend, target_trend, carry_trend,
      projected_targets, projected_carries, matchup_delta_22_raw,
      roll3_targets, roll3_carries, roll3_offense_pct,
      locked_base_fppg_24, projected_weekly_fppg_24
    )
  )

  promotion_path <- "output/weekly_2_4_promotion.csv"
  prom <- if (file.exists(promotion_path)) {
    readr::read_csv(promotion_path, show_col_types = FALSE, progress = FALSE)
  } else {
    data.frame()
  }
  if (nrow(prom)) {
    d <- d |> dplyr::left_join(prom |> dplyr::select(position, promoted_for_2026), by = "position")
  } else {
    d$promoted_for_2026 <- FALSE
  }

  d <- d |>
    dplyr::mutate(
      production_pred = dplyr::if_else(
        promoted_for_2026 %in% TRUE,
        suppressWarnings(as.numeric(projected_weekly_fppg_24)),
        suppressWarnings(as.numeric(locked_base_fppg_24))
      ),
      error = production_pred - actual_fppg,
      abs_error = abs(error),
      squared_error = error^2,
      cohort = dplyr::case_when(
        starter_cohort %in% TRUE ~ "Starter",
        relevant_cohort %in% TRUE ~ "Relevant non-starter",
        TRUE ~ "Depth"
      )
    )

  metrics <- d |>
    dplyr::group_by(position, cohort) |>
    dplyr::summarise(
      n = dplyr::n(),
      MAE = mean(abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
      bias = mean(error, na.rm = TRUE),
      .groups = "drop"
    )
  readr::write_csv(metrics, "output/decision_relevant_error_3_0.csv")

  phase <- d |>
    dplyr::group_by(position, week_phase_23, cohort) |>
    dplyr::summarise(
      n = dplyr::n(),
      MAE = mean(abs_error, na.rm = TRUE),
      bias = mean(error, na.rm = TRUE),
      .groups = "drop"
    )
  readr::write_csv(phase, "output/decision_relevant_error_by_phase_3_0.csv")

  misses <- d |>
    dplyr::arrange(dplyr::desc(abs_error)) |>
    dplyr::mutate(
      likely_error_family = dplyr::case_when(
        dplyr::coalesce(injury_risk, 0) >= 0.5 | dplyr::coalesce(practice_risk, 0) >= 0.5 ~ "Availability / injury",
        abs(dplyr::coalesce(snap_trend, 0)) >= 0.15 |
          abs(dplyr::coalesce(target_trend, 0)) >= 2 |
          abs(dplyr::coalesce(carry_trend, 0)) >= 4 ~ "Role regime change",
        dplyr::coalesce(projected_targets, 0) >= 7 & actual_fppg > production_pred + 8 ~ "Efficiency / TD upside",
        dplyr::coalesce(projected_carries, 0) >= 14 & actual_fppg > production_pred + 8 ~ "Efficiency / TD upside",
        abs(dplyr::coalesce(matchup_delta_22_raw, 0)) >= 2 ~ "Matchup sensitivity",
        TRUE ~ "Unclassified / variance"
      )
    ) |>
    dplyr::select(
      season, week, player_id, player_display_name, position, team, opponent,
      actual_fppg, production_pred, error, abs_error, cohort, likely_error_family,
      roll3_targets, roll3_carries, roll3_offense_pct, snap_trend, target_trend,
      carry_trend, injury_risk, practice_risk, matchup_delta_22_raw
    ) |>
    dplyr::slice_head(n = 500)
  readr::write_csv(misses, "output/decision_relevant_biggest_misses_3_0.csv")

  readr::write_csv(
    tibble::tibble(
      full_audit_available = TRUE,
      status = "COMPLETE",
      required_file = path,
      action = "None"
    ),
    status_path
  )

  rm(d, metrics, phase, misses)
  invisible(gc(full = TRUE))
  cat("[3.0 AUDIT] Full decision-relevant error audit written.\n")
  invisible(TRUE)
}

run_decision_relevant_audit_30()
