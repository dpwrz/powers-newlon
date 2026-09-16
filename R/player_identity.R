# ============================================================
# FANTASY MODEL 3.0 - PLAYER IDENTITY BRIDGE
# 3.0.4: collision-safe name/position fallback + schema-safe IDs
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "stringr", "readr", "tibble"))

fm3_norm_name <- function(x) {
  x <- iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", x)
  x <- gsub("[^a-z0-9]", "", x)
  x
}

fm3_alias_vector <- function(d, aliases, default = "") {
  n <- nrow(d)
  hit <- aliases[aliases %in% names(d)]
  if (!length(hit)) return(rep(default, n))
  x <- d[[hit[[1]]]]
  if (length(x) != n) return(rep(default, n))
  x
}

fm3_identity_sleeper_schema <- function(d) {
  if (is.null(d) || !is.data.frame(d)) d <- tibble::tibble()
  d <- tibble::as_tibble(d)
  n <- nrow(d)

  if (!"sleeper_id" %in% names(d)) d[["sleeper_id"]] <- fm3_alias_vector(d, c("player_id", "id"), "")
  if (!"gsis_id" %in% names(d)) d[["gsis_id"]] <- fm3_alias_vector(d, c("player_gsis_id", "nflverse_id"), "")
  if (!"full_name" %in% names(d)) d[["full_name"]] <- fm3_alias_vector(d, c("player_name", "display_name"), "")
  if (!"position" %in% names(d)) d[["position"]] <- fm3_alias_vector(d, c("pos"), "")
  if (!"team" %in% names(d)) d[["team"]] <- fm3_alias_vector(d, c("current_team"), "FA")

  # Force canonical vectors by explicit indexing.  This intentionally avoids
  # dplyr data-mask lookup so a missing/renamed ID column can never become an
  # "object 'gsis_id' not found" error.
  d[["sleeper_id"]] <- as.character(d[["sleeper_id"]])
  d[["gsis_id"]] <- as.character(d[["gsis_id"]])
  d[["full_name"]] <- as.character(d[["full_name"]])
  d[["position"]] <- toupper(as.character(d[["position"]]))
  d[["team"]] <- toupper(as.character(d[["team"]]))
  d[["sleeper_id"]][is.na(d[["sleeper_id"]])] <- ""
  d[["gsis_id"]][is.na(d[["gsis_id"]])] <- ""
  d[["full_name"]][is.na(d[["full_name"]])] <- ""
  d[["position"]][is.na(d[["position"]])] <- ""
  d[["team"]][is.na(d[["team"]]) | !nzchar(d[["team"]])] <- "FA"
  d
}

fm3_identity_model_schema <- function(d) {
  if (is.null(d) || !is.data.frame(d)) d <- tibble::tibble()
  d <- tibble::as_tibble(d)
  n <- nrow(d)
  if (!"gsis_id" %in% names(d)) {
    d[["gsis_id"]] <- if ("player_id" %in% names(d)) as.character(d[["player_id"]]) else rep("", n)
  }
  if (!"player_display_name" %in% names(d)) d[["player_display_name"]] <- rep("", n)
  if (!"position" %in% names(d)) d[["position"]] <- rep("", n)
  if (!"current_team" %in% names(d)) d[["current_team"]] <- rep("", n)
  d[["gsis_id"]] <- as.character(d[["gsis_id"]])
  d[["gsis_id"]][is.na(d[["gsis_id"]])] <- ""
  d[["player_display_name"]] <- as.character(d[["player_display_name"]])
  d[["position"]] <- toupper(as.character(d[["position"]]))
  d[["current_team"]] <- toupper(as.character(d[["current_team"]]))
  d
}

fm3_model_player_frame <- function() {
  dynasty_path <- "output/final_dynasty_rankings.csv"
  season_path <- paste0("output/final_", CURRENT_SEASON, "_rankings.csv")
  if (!file.exists(dynasty_path)) stop("Missing ", dynasty_path, ". Run the base projection pipeline first.")

  d <- readr::read_csv(dynasty_path, show_col_types = FALSE)

  if (file.exists(season_path)) {
    s <- readr::read_csv(season_path, show_col_types = FALSE) |>
      dplyr::select(dplyr::any_of(c("player_id", "age", "is_rookie", "projected_fppg", "confidence", "model_version"))) |>
      dplyr::distinct(.data$player_id, .keep_all = TRUE)

    merge_fields <- setdiff(names(s), "player_id")
    for (nm in merge_fields) names(s)[names(s) == nm] <- paste0(nm, "__season")
    d <- dplyr::left_join(d, s, by = "player_id")

    for (nm in merge_fields) {
      season_nm <- paste0(nm, "__season")
      if (!season_nm %in% names(d)) next
      if (nm %in% names(d)) d[[nm]] <- dplyr::coalesce(d[[season_nm]], d[[nm]]) else d[[nm]] <- d[[season_nm]]
      d[[season_nm]] <- NULL
    }
  }

  defaults <- list(age = NA_real_, is_rookie = 0, projected_fppg = NA_real_, confidence = "medium", model_version = NA_character_)
  for (nm in names(defaults)) if (!nm %in% names(d)) d[[nm]] <- rep(defaults[[nm]], nrow(d))

  # 3.0.8: enrich the model frame from the compact Fantasy Model current-week
  # snapshot prepared before Shiny starts. No Sleeper projections are joined.
  snapshot_path <- if (exists("FM3_APP_WEEKLY_SNAPSHOT_PATH")) FM3_APP_WEEKLY_SNAPSHOT_PATH else "data/processed/app_current_week_snapshot_3_0.csv"
  if (file.exists(snapshot_path)) {
    w <- tryCatch(readr::read_csv(snapshot_path, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
    if (!is.null(w) && nrow(w) && "player_id" %in% names(w)) {
      keep <- intersect(c(
        "player_id", "week", "opponent", "gameday", "projected_weekly_fppg_24",
        "weekly_floor", "weekly_ceiling", "projection_confidence", "expected_abs_error",
        "matchup_grade", "boom_probability", "bust_probability", "injury_status",
        "practice_status", "availability_factor", "projected_targets_30",
        "projected_carries_30", "projected_snap_share_30",
        "role_regime_probability_30", "role_uncertainty_30"
      ), names(w))
      w <- w[, keep, drop = FALSE]
      w <- w[!duplicated(w[["player_id"]]), , drop = FALSE]
      merge_fields <- setdiff(names(w), "player_id")
      for (nm in merge_fields) names(w)[names(w) == nm] <- paste0(nm, "__week")
      d <- dplyr::left_join(d, w, by = "player_id")
      for (nm in merge_fields) {
        wk_nm <- paste0(nm, "__week")
        if (!wk_nm %in% names(d)) next
        if (nm %in% names(d)) {
          # Current-week values take precedence when present.
          if (is.numeric(d[[wk_nm]]) || is.integer(d[[wk_nm]])) {
            x <- suppressWarnings(as.numeric(d[[wk_nm]])); y <- suppressWarnings(as.numeric(d[[nm]]))
            d[[nm]] <- ifelse(is.finite(x), x, y)
          } else {
            x <- as.character(d[[wk_nm]]); y <- as.character(d[[nm]])
            d[[nm]] <- ifelse(!is.na(x) & nzchar(x), x, y)
          }
        } else d[[nm]] <- d[[wk_nm]]
        d[[wk_nm]] <- NULL
      }
    }
  }

  if (!"player_id" %in% names(d)) stop("final_dynasty_rankings.csv is missing required player_id.")
  if (!"player_display_name" %in% names(d)) d[["player_display_name"]] <- rep("", nrow(d))
  if (!"position" %in% names(d)) d[["position"]] <- rep("", nrow(d))
  if (!"current_team" %in% names(d)) d[["current_team"]] <- rep("", nrow(d))

  d[["gsis_id"]] <- as.character(d[["player_id"]])
  d[["normalized_name"]] <- fm3_norm_name(d[["player_display_name"]])
  d[["position"]] <- toupper(as.character(d[["position"]]))
  d[["current_team"]] <- toupper(as.character(d[["current_team"]]))
  tibble::as_tibble(d)
}

fm3_build_identity_map <- function(sleeper_players, model_players = fm3_model_player_frame()) {
  sp <- fm3_identity_sleeper_schema(sleeper_players)
  mp <- fm3_identity_model_schema(model_players)

  sp[["normalized_name"]] <- fm3_norm_name(sp[["full_name"]])
  mp[["normalized_name"]] <- fm3_norm_name(mp[["player_display_name"]])

  # Exact GSIS matches when available.
  sp_exact <- sp[nzchar(sp[["gsis_id"]]), , drop = FALSE]
  mp_exact <- mp[, c("gsis_id", "player_display_name", "position", "current_team"), drop = FALSE]
  names(mp_exact) <- c("gsis_id", "model_name", "model_position", "model_team")
  exact <- dplyr::inner_join(sp_exact, mp_exact, by = "gsis_id")
  if (nrow(exact)) {
    exact <- tibble::tibble(
      sleeper_id = as.character(exact[["sleeper_id"]]),
      gsis_id = as.character(exact[["gsis_id"]]),
      model_name = as.character(exact[["model_name"]]),
      match_method = "gsis",
      match_confidence = 1
    )
  } else {
    exact <- tibble::tibble(sleeper_id = character(), gsis_id = character(), model_name = character(), match_method = character(), match_confidence = numeric())
  }

  remaining <- sp[!sp[["sleeper_id"]] %in% exact[["sleeper_id"]], , drop = FALSE]
  # Keep the model GSIS identifier under a distinct name before the name/position
  # join. `remaining` already has Sleeper's canonical `gsis_id`; if both tables
  # carry that name dplyr suffixes them to `gsis_id.x` / `gsis_id.y`, and the
  # later `[["gsis_id"]]` lookup becomes a zero-length vector.  That was the
  # 506-row/0-row tibble failure seen in the live Sleeper bridge.
  mp_name <- mp[, c("gsis_id", "player_display_name", "normalized_name", "position", "current_team"), drop = FALSE]
  names(mp_name) <- c("model_gsis_id", "model_name", "normalized_name", "model_position", "model_team")

  by_name_pos <- dplyr::inner_join(remaining, mp_name, by = c("normalized_name", "position" = "model_position"))
  if (nrow(by_name_pos)) {
    by_name_pos[["team_match"]] <- as.numeric(as.character(by_name_pos[["team"]]) == as.character(by_name_pos[["model_team"]]))
    by_name_pos <- by_name_pos |>
      dplyr::group_by(.data$sleeper_id) |>
      dplyr::mutate(candidate_n = dplyr::n()) |>
      dplyr::arrange(dplyr::desc(.data$team_match), .by_group = TRUE) |>
      dplyr::slice(1) |>
      dplyr::ungroup()
    by_name_pos <- tibble::tibble(
      sleeper_id = as.character(by_name_pos[["sleeper_id"]]),
      gsis_id = as.character(by_name_pos[["model_gsis_id"]]),
      model_name = as.character(by_name_pos[["model_name"]]),
      match_method = ifelse(by_name_pos[["team_match"]] == 1, "name_position_team", "name_position"),
      match_confidence = ifelse(by_name_pos[["team_match"]] == 1, 0.98, ifelse(by_name_pos[["candidate_n"]] == 1, 0.93, 0.80))
    )
  } else {
    by_name_pos <- tibble::tibble(sleeper_id = character(), gsis_id = character(), model_name = character(), match_method = character(), match_confidence = numeric())
  }

  out <- dplyr::bind_rows(exact, by_name_pos) |>
    dplyr::distinct(.data$sleeper_id, .keep_all = TRUE)

  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(out, "data/processed/sleeper_player_identity_3_0.csv")
  out
}

fm3_attach_model_ids <- function(sleeper_df, identity_map) {
  sleeper_df <- fm3_identity_sleeper_schema(sleeper_df)
  needed <- c("sleeper_id", "gsis_id", "model_name", "match_method", "match_confidence")
  for (nm in needed) if (!nm %in% names(identity_map)) identity_map[[nm]] <- if (nm == "match_confidence") NA_real_ else ""
  dplyr::left_join(sleeper_df, identity_map[, needed, drop = FALSE], by = "sleeper_id", suffix = c("", "__model"))
}
