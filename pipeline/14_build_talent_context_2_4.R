# ============================================================
# FANTASY MODEL 2.4 - TALENT + DEFENSIVE STYLE DATA BUILD
# ============================================================
# New information only. This stage does NOT rebuild the existing PBP/weekly
# feature store. Large college/participation sources are processed one season
# at a time and checkpointed for Posit Cloud memory safety.

source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr", "nflreadr", "slider"))
source("R/weekly_engine.R")
source("R/weekly_engine_22.R")
source("R/weekly_engine_23.R")
source("R/weekly_engine_232.R")
source("R/weekly_engine_24.R")

set.seed(SEED)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create("models", recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL24_COLLEGE_CHECKPOINT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL24_DEFENSE_CHECKPOINT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("\n[2.4 DATA] Building player talent + opponent-style context.\n")
cat("[2.4 DATA] Existing PBP/weekly feature stores are NOT rebuilt.\n")

pos24 <- function(x) {
  z <- toupper(trimws(as.character(x)))
  compact <- gsub("[^A-Z]", "", z)
  z[compact %in% c("QB", "QUARTERBACK", "QUARTERBACKS")] <- "QB"
  z[compact %in% c("RB", "HB", "FB", "TB", "RUNNINGBACK", "RUNNINGBACKS", "HALFBACK", "FULLBACK", "TAILBACK")] <- "RB"
  z[compact %in% c("WR", "SE", "FL", "WIDERECEIVER", "WIDERECEIVERS", "WIDEOUT", "RECEIVER")] <- "WR"
  z[compact %in% c("TE", "TIGHTEND", "TIGHTENDS")] <- "TE"
  z
}

# NCAA stats occasionally represent names as "Last, First" while nflverse
# generally uses "First Last". Prospect matching must also tolerate common
# suffixes (Jr./III/etc.) or an otherwise valid college class can produce zero
# matches. Keep the full remaining name rather than first/last only to reduce
# collisions between different players.
person_key24 <- function(x) {
  raw <- trimws(as.character(x))
  raw <- iconv(raw, to = "ASCII//TRANSLIT")
  raw[is.na(raw)] <- ""
  vapply(raw, function(s) {
    s <- trimws(s)
    if (!nzchar(s)) return("")
    if (grepl(",", s, fixed = TRUE)) {
      pp <- strsplit(s, ",", fixed = TRUE)[[1]]
      pp <- trimws(pp[nzchar(trimws(pp))])
      if (length(pp) >= 2) s <- paste(c(pp[2:length(pp)], pp[1]), collapse = " ")
    }
    s <- tolower(s)
    # Strip generational suffixes only as standalone tokens.
    s <- gsub("(^|[[:space:][:punct:]])(jr|sr|ii|iii|iv|v)([[:space:][:punct:]]|$)", " ", s, perl = TRUE)
    gsub("[^a-z0-9]", "", s)
  }, character(1), USE.NAMES = FALSE)
}

# 2.4.3 relaxed identity helpers. Exact normalized full-name matching remains the
# first choice. For common nickname/legal-name differences (e.g. Gabe/Gabriel),
# a second pass can safely use first initial + normalized last name + position +
# draft-time window. Ambiguous NCAA identities are discarded rather than guessed.
name_tokens24 <- function(x) {
  raw <- trimws(as.character(x))
  raw <- iconv(raw, to = "ASCII//TRANSLIT")
  raw[is.na(raw)] <- ""
  lapply(raw, function(s) {
    s <- trimws(s)
    if (!nzchar(s)) return(character())
    if (grepl(",", s, fixed = TRUE)) {
      pp <- strsplit(s, ",", fixed = TRUE)[[1]]
      pp <- trimws(pp[nzchar(trimws(pp))])
      if (length(pp) >= 2) s <- paste(c(pp[2:length(pp)], pp[1]), collapse = " ")
    }
    s <- tolower(s)
    s <- gsub("(^|[[:space:][:punct:]])(jr|sr|ii|iii|iv|v)([[:space:][:punct:]]|$)", " ", s, perl = TRUE)
    s <- gsub("[^a-z0-9]+", " ", s)
    tok <- strsplit(trimws(s), "[[:space:]]+")[[1]]
    tok[nzchar(tok)]
  })
}
first_initial24 <- function(x) {
  tt <- name_tokens24(x)
  vapply(tt, function(z) if (length(z)) substr(z[1], 1, 1) else "", character(1))
}
last_key24 <- function(x) {
  tt <- name_tokens24(x)
  vapply(tt, function(z) if (length(z)) z[length(z)] else "", character(1))
}

num_alias24 <- function(d, aliases, default = 0) {
  x <- wk24_pick(d, aliases, default)
  z <- suppressWarnings(as.numeric(x)); z[!is.finite(z)] <- default; z
}
char_alias24 <- function(d, aliases, default = "") {
  x <- as.character(wk24_pick(d, aliases, default)); x[is.na(x)] <- default; x
}
height_inches24 <- function(x) {
  raw <- trimws(as.character(x)); out <- suppressWarnings(as.numeric(raw))
  miss <- !is.finite(out)
  if (any(miss)) {
    z <- gsub("[\"']", "-", raw[miss]); z <- gsub("[^0-9-]", "", z)
    parts <- strsplit(z, "-", fixed = TRUE)
    val <- vapply(parts, function(p) {
      p <- p[nzchar(p)]; if (length(p) >= 2) suppressWarnings(as.numeric(p[1]) * 12 + as.numeric(p[2])) else NA_real_
    }, numeric(1))
    out[miss] <- val
  }
  out
}
bool_num_na24 <- function(x) {
  if (is.logical(x)) return(ifelse(is.na(x), NA_real_, as.numeric(x)))
  if (is.numeric(x)) return(ifelse(is.finite(x), as.numeric(x != 0), NA_real_))
  z <- tolower(trimws(as.character(x))); out <- rep(NA_real_, length(z))
  known <- nzchar(z) & !is.na(z)
  out[known] <- as.numeric(z[known] %in% c("1", "true", "t", "yes", "y", "pressure"))
  out
}
contains_num_na24 <- function(x, pattern) {
  z <- tolower(trimws(as.character(x))); out <- rep(NA_real_, length(z))
  known <- nzchar(z) & !is.na(z)
  out[known] <- as.numeric(grepl(pattern, z[known]))
  out
}

# ------------------------------------------------------------
# 1) NFL identity + combine + draft capital
# ------------------------------------------------------------
players_path <- "data/raw/players.csv"
players <- if (file.exists(players_path)) readr::read_csv(players_path, show_col_types = FALSE, progress = FALSE) else tryCatch(nflreadr::load_players(), error = function(e) data.frame())
if (!nrow(players)) stop("2.4 could not obtain nflreadr player metadata.")

player_id <- char_alias24(players, c("gsis_id", "player_id", "nflverse_id", "nfl_id"))
player_name <- char_alias24(players, c("display_name", "full_name", "player_name", "name"))
player_first <- char_alias24(players, c("first_name"))
player_last <- char_alias24(players, c("last_name"))
player_common_first <- char_alias24(players, c("common_first_name"))
player_football_name <- char_alias24(players, c("football_name"))
player_pos <- pos24(char_alias24(players, c("position", "pos", "position_group")))
birth_date <- suppressWarnings(as.Date(char_alias24(players, c("birth_date", "birthdate", "date_of_birth"))))
rookie_year <- num_alias24(players, c("rookie_year", "entry_year", "first_season"), NA_real_)
player_draft_year <- num_alias24(players, c("draft_year", "rookie_year", "entry_year"), NA_real_)
player_draft_number <- num_alias24(players, c("draft_number", "draft_pick", "draft_overall"), NA_real_)
player_draft_round <- num_alias24(players, c("draft_round", "round"), NA_real_)
pfr_id <- char_alias24(players, c("pfr_id", "pfr_player_id"))
college_name_meta <- char_alias24(players, c("college_name", "college", "school"))

nfl_base <- data.frame(
  player_id = player_id, player_name = player_name, name_key_24 = person_key24(player_name),
  position = player_pos, birth_date_24 = birth_date, rookie_year_24 = rookie_year,
  draft_year_meta_24 = player_draft_year, draft_overall_meta_24 = player_draft_number,
  draft_round_meta_24 = player_draft_round, pfr_id_24 = pfr_id,
  college_name_meta_24 = college_name_meta, stringsAsFactors = FALSE
) |>
  dplyr::filter(position %in% POSITIONS, nzchar(player_id), nzchar(name_key_24)) |>
  dplyr::distinct(player_id, .keep_all = TRUE)

# 2.4.2: maintain multiple NFL name aliases for NCAA matching. College feeds and
# NFL feeds frequently disagree on preferred/common names (for example a legal
# first name in college versus a football nickname in the NFL). Matching only
# display_name can therefore make a valid college dataset look completely
# unmatched. All aliases remain tied to the same GSIS player_id and position.
name_alias_sources_24 <- list(
  player_name,
  paste(player_first, player_last),
  paste(player_common_first, player_last),
  # football_name is often only a preferred first name, so pair it with last.
  paste(player_football_name, player_last)
)
nfl_name_alias24 <- dplyr::bind_rows(lapply(name_alias_sources_24, function(v) {
  data.frame(player_id = player_id, position = player_pos,
             name_key_24 = person_key24(v),
             first_initial_24 = first_initial24(v),
             last_key_24 = last_key24(v), stringsAsFactors = FALSE)
})) |>
  dplyr::filter(position %in% POSITIONS, nzchar(player_id), nzchar(name_key_24)) |>
  dplyr::distinct(player_id, position, name_key_24, first_initial_24, last_key_24)

# PFR draft names are another useful canonical alias and already carry GSIS ids.
# Failure to download draft picks never blocks the build.
draft_alias_raw24 <- tryCatch(nflreadr::load_draft_picks(seasons = TRUE), error = function(e) data.frame())
if (nrow(draft_alias_raw24)) {
  draft_name24 <- char_alias24(draft_alias_raw24, c("pfr_player_name", "player_name", "name"))
  draft_alias24 <- data.frame(
    player_id = char_alias24(draft_alias_raw24, c("gsis_id")),
    position = pos24(char_alias24(draft_alias_raw24, c("position", "pos"))),
    name_key_24 = person_key24(draft_name24),
    first_initial_24 = first_initial24(draft_name24),
    last_key_24 = last_key24(draft_name24),
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(position %in% POSITIONS, nzchar(player_id), nzchar(name_key_24)) |>
    dplyr::distinct(player_id, position, name_key_24, first_initial_24, last_key_24)
  nfl_name_alias24 <- dplyr::bind_rows(nfl_name_alias24, draft_alias24) |>
    dplyr::distinct(player_id, position, name_key_24, first_initial_24, last_key_24)
}

combine <- if (isTRUE(MODEL24_USE_COMBINE)) tryCatch(nflreadr::load_combine(), error = function(e) {
  warning("[2.4 COMBINE] nflreadr::load_combine unavailable: ", conditionMessage(e)); data.frame()
}) else data.frame()

if (nrow(combine)) {
  combine_std <- data.frame(
    c_name_key = person_key24(char_alias24(combine, c("player_name", "name", "full_name"))),
    c_position = pos24(char_alias24(combine, c("pos", "position"))),
    c_pfr_id = char_alias24(combine, c("pfr_id", "pfr_player_id")),
    c_school = char_alias24(combine, c("school", "college")),
    c_draft_year = num_alias24(combine, c("draft_year", "season"), NA_real_),
    c_draft_round = num_alias24(combine, c("draft_round", "round"), NA_real_),
    c_draft_overall = num_alias24(combine, c("draft_ovr", "draft_overall", "draft_pick"), NA_real_),
    c_height = char_alias24(combine, c("ht", "height", "height_in"), ""),
    c_weight = num_alias24(combine, c("wt", "weight"), NA_real_),
    c_forty = num_alias24(combine, c("forty", "forty_yard", "forty_time"), NA_real_),
    c_bench = num_alias24(combine, c("bench", "bench_reps"), NA_real_),
    c_vertical = num_alias24(combine, c("vertical", "vertical_jump"), NA_real_),
    c_broad = num_alias24(combine, c("broad_jump", "broad"), NA_real_),
    c_cone = num_alias24(combine, c("cone", "three_cone"), NA_real_),
    c_shuttle = num_alias24(combine, c("shuttle", "short_shuttle"), NA_real_),
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(c_position %in% POSITIONS, nzchar(c_name_key)) |>
    dplyr::arrange(c_draft_year, c_draft_overall)

  # Prefer exact PFR identity when present. Then fill unmatched rows by
  # normalized name + position. The latter is constrained by draft/rookie year.
  by_pfr <- combine_std |> dplyr::filter(nzchar(c_pfr_id)) |> dplyr::distinct(c_pfr_id, .keep_all = TRUE)
  nfl_base <- nfl_base |> dplyr::left_join(by_pfr, by = c("pfr_id_24" = "c_pfr_id"))
  miss <- is.na(nfl_base$c_draft_year) | !is.finite(nfl_base$c_draft_year)
  if (any(miss)) {
    by_name <- combine_std |> dplyr::distinct(c_name_key, c_position, .keep_all = TRUE)
    tmp <- nfl_base[miss, c("player_id", "name_key_24", "position"), drop = FALSE] |>
      dplyr::left_join(by_name, by = c("name_key_24" = "c_name_key", "position" = "c_position"))
    fill_cols <- setdiff(names(tmp), c("player_id", "name_key_24", "position"))
    idx <- match(nfl_base$player_id[miss], tmp$player_id)
    for (nm in fill_cols) if (nm %in% names(nfl_base)) nfl_base[[nm]][miss] <- tmp[[nm]][idx]
  }
} else {
  for (nm in c("c_school", "c_draft_year", "c_draft_round", "c_draft_overall", "c_height", "c_weight", "c_forty", "c_bench", "c_vertical", "c_broad", "c_cone", "c_shuttle")) nfl_base[[nm]] <- NA
}

nfl_base$draft_year_24 <- dplyr::coalesce(suppressWarnings(as.numeric(nfl_base$c_draft_year)), nfl_base$draft_year_meta_24, nfl_base$rookie_year_24)
nfl_base$draft_round_24 <- dplyr::coalesce(suppressWarnings(as.numeric(nfl_base$c_draft_round)), nfl_base$draft_round_meta_24)
nfl_base$draft_overall_24 <- dplyr::coalesce(suppressWarnings(as.numeric(nfl_base$c_draft_overall)), nfl_base$draft_overall_meta_24)
nfl_base$draft_capital_log_24 <- ifelse(is.finite(nfl_base$draft_overall_24), -log1p(pmax(0, nfl_base$draft_overall_24)), -log1p(300))
nfl_base$combine_height_in_24 <- height_inches24(nfl_base$c_height)
nfl_base$combine_weight_24 <- suppressWarnings(as.numeric(nfl_base$c_weight))
nfl_base$combine_forty_24 <- suppressWarnings(as.numeric(nfl_base$c_forty))
nfl_base$combine_vertical_24 <- suppressWarnings(as.numeric(nfl_base$c_vertical))
nfl_base$combine_broad_24 <- suppressWarnings(as.numeric(nfl_base$c_broad))
nfl_base$combine_cone_24 <- suppressWarnings(as.numeric(nfl_base$c_cone))
nfl_base$combine_shuttle_24 <- suppressWarnings(as.numeric(nfl_base$c_shuttle))
nfl_base$combine_bmi_24 <- ifelse(is.finite(nfl_base$combine_weight_24) & is.finite(nfl_base$combine_height_in_24) & nfl_base$combine_height_in_24 > 0,
                                  703 * nfl_base$combine_weight_24 / nfl_base$combine_height_in_24^2, NA_real_)
nfl_base$combine_speed_score_24 <- ifelse(is.finite(nfl_base$combine_weight_24) & is.finite(nfl_base$combine_forty_24) & nfl_base$combine_forty_24 > 0,
                                          nfl_base$combine_weight_24 * 200 / nfl_base$combine_forty_24^4, NA_real_)
nfl_base$age_at_draft_24 <- ifelse(!is.na(nfl_base$birth_date_24) & is.finite(nfl_base$draft_year_24),
                                   as.numeric(as.Date(paste0(as.integer(nfl_base$draft_year_24), "-09-01")) - nfl_base$birth_date_24) / 365.25, NA_real_)
nfl_base$early_declare_heuristic_24 <- ifelse(is.finite(nfl_base$age_at_draft_24), as.numeric(nfl_base$age_at_draft_24 <= 21.75), 0)

# Athleticism standardized within position + combine class. Lower timed drills
# are better, so their signs are reversed before aggregation.
ath_raw <- nfl_base |>
  dplyr::group_by(position, draft_year_24) |>
  dplyr::mutate(
    z_speed = ifelse(is.finite(combine_speed_score_24), as.numeric(scale(combine_speed_score_24)), NA_real_),
    z_vert = ifelse(is.finite(combine_vertical_24), as.numeric(scale(combine_vertical_24)), NA_real_),
    z_broad = ifelse(is.finite(combine_broad_24), as.numeric(scale(combine_broad_24)), NA_real_),
    z_cone = ifelse(is.finite(combine_cone_24), -as.numeric(scale(combine_cone_24)), NA_real_),
    z_shuttle = ifelse(is.finite(combine_shuttle_24), -as.numeric(scale(combine_shuttle_24)), NA_real_)) |>
  dplyr::ungroup()
nfl_base$combine_athleticism_z_24 <- apply(as.data.frame(ath_raw[, c("z_speed", "z_vert", "z_broad", "z_cone", "z_shuttle")]), 1, function(v) {
  v <- suppressWarnings(as.numeric(v)); v <- v[is.finite(v)]; if (length(v)) mean(v) else NA_real_
})
nfl_base$combine_missing_24 <- as.numeric(!is.finite(nfl_base$combine_forty_24) & !is.finite(nfl_base$combine_vertical_24) & !is.finite(nfl_base$combine_broad_24))

cat("[2.4 COMBINE] NFL player profiles: ", nrow(nfl_base), "; combine rows: ", nrow(combine), "\n", sep = "")
rm(players, combine, draft_alias_raw24); if (exists("draft_alias24")) rm(draft_alias24); gc()

# ------------------------------------------------------------
# 2) College production, one season at a time
# ------------------------------------------------------------
ensure_cfbfastR24 <- function() {
  if (requireNamespace("cfbfastR", quietly = TRUE)) return(TRUE)
  if (!isTRUE(MODEL24_AUTO_INSTALL_CFBFASTR)) return(FALSE)
  cat("[2.4 COLLEGE] Installing cfbfastR from SportsDataverse r-universe...\n")
  tryCatch({
    install.packages("cfbfastR", repos = c("https://sportsdataverse.r-universe.dev", "https://cloud.r-project.org"), quiet = TRUE)
  }, error = function(e) warning("cfbfastR install failed: ", conditionMessage(e)))
  requireNamespace("cfbfastR", quietly = TRUE)
}

compact_college24 <- function(raw, yr) {
  if (is.null(raw) || !nrow(raw)) return(data.frame())
  nm <- names(raw)
  season <- num_alias24(raw, c("season", "year"), yr)
  pname <- char_alias24(raw, c("player_name", "athlete_name", "name", "player"))
  ppos <- pos24(char_alias24(raw, c("position", "pos")))
  team <- char_alias24(raw, c("team", "team_name", "team_id", "school", "school_name"))
  game_id <- char_alias24(raw, c("game_id", "contest_id", "espn_game_id", "id", "game"))
  out <- data.frame(
    season = season, player_name = pname, name_key_24 = person_key24(pname), position = ppos,
    college_team_24 = team, game_id_24 = game_id,
    pass_att = num_alias24(raw, c("attempts", "passing_attempts", "pass_attempts", "pass_att")),
    pass_yd = num_alias24(raw, c("passing_yards", "pass_yards", "pass_yds")),
    pass_td = num_alias24(raw, c("passing_tds", "passing_touchdowns", "pass_tds")),
    ints = num_alias24(raw, c("interceptions", "passing_interceptions", "ints")),
    rush_att = num_alias24(raw, c("carries", "rushing_attempts", "rush_attempts", "rush_att")),
    rush_yd = num_alias24(raw, c("rushing_yards", "rush_yards", "rush_yds", "yds_rush", "rush_yds_gained")),
    rush_td = num_alias24(raw, c("rushing_tds", "rushing_touchdowns", "rush_tds")),
    rec = num_alias24(raw, c("receptions", "receiving_receptions", "rec")),
    rec_yd = num_alias24(raw, c("receiving_yards", "rec_yards", "rec_yds")),
    rec_td = num_alias24(raw, c("receiving_tds", "receiving_touchdowns", "rec_tds", "rec_td")), stringsAsFactors = FALSE)
  out <- out |> dplyr::filter(position %in% POSITIONS, nzchar(name_key_24))
  if (!nrow(out)) return(out)
  out |> dplyr::group_by(season, name_key_24, player_name, position, college_team_24) |>
    dplyr::summarise(
      college_games_season_24 = if (any(nzchar(game_id_24))) dplyr::n_distinct(game_id_24[nzchar(game_id_24)]) else NA_integer_,
      pass_att = sum(pass_att, na.rm = TRUE), pass_yd = sum(pass_yd, na.rm = TRUE), pass_td = sum(pass_td, na.rm = TRUE), ints = sum(ints, na.rm = TRUE),
      rush_att = sum(rush_att, na.rm = TRUE), rush_yd = sum(rush_yd, na.rm = TRUE), rush_td = sum(rush_td, na.rm = TRUE),
      rec = sum(rec, na.rm = TRUE), rec_yd = sum(rec_yd, na.rm = TRUE), rec_td = sum(rec_td, na.rm = TRUE), .groups = "drop")
}

college_seasons <- MODEL24_COLLEGE_START:TRAIN_END
college_parts <- list()
if (isTRUE(MODEL24_USE_COLLEGE) && ensure_cfbfastR24()) {
  for (yr in college_seasons) {
    cp <- file.path(MODEL24_COLLEGE_CHECKPOINT_DIR, paste0("college_player_stats_", yr, ".csv"))
    if (file.exists(cp)) {
      cat("[2.4 COLLEGE] Reusing ", yr, " checkpoint.\n", sep = "")
      z <- tryCatch(readr::read_csv(cp, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
    } else {
      cat("[2.4 COLLEGE] ", yr, " player stats...\n", sep = "")
      raw <- tryCatch(cfbfastR::load_ncaa_mfb_player_stats(seasons = yr), error = function(e) {
        warning("College stats unavailable for ", yr, ": ", conditionMessage(e)); data.frame()
      })
      z <- compact_college24(raw, yr)
      if (nrow(z)) readr::write_csv(z, cp)
      rm(raw); gc()
    }
    if (nrow(z)) {
      # 2.4.1: old checkpoints are valid data, but their original name/position
      # keys may predate the robust NCAA↔NFL normalization. Re-key in memory so
      # the expensive college downloads never need to be repeated.
      if ("player_name" %in% names(z)) z$name_key_24 <- person_key24(z$player_name)
      if ("position" %in% names(z)) z$position <- pos24(z$position)
      college_parts[[length(college_parts) + 1]] <- z
    }
  }
} else if (isTRUE(MODEL24_USE_COLLEGE)) {
  warning("[2.4 COLLEGE] cfbfastR unavailable. 2.4 will continue with combine/draft/NFL signals; college_missing_24 will remain 1.")
}
college <- dplyr::bind_rows(college_parts)
if (nrow(college)) {
  college$first_initial_24 <- first_initial24(college$player_name)
  college$last_key_24 <- last_key24(college$player_name)
}

# Optional starts from NCAA rosters. Failure never blocks the build.
college_starts <- data.frame()
if (nrow(college) && requireNamespace("cfbfastR", quietly = TRUE) && exists("load_ncaa_mfb_rosters", envir = asNamespace("cfbfastR"), inherits = FALSE)) {
  start_parts <- list()
  for (yr in college_seasons) {
    cp <- file.path(MODEL24_COLLEGE_CHECKPOINT_DIR, paste0("college_roster_", yr, ".csv"))
    rr <- if (file.exists(cp)) tryCatch(readr::read_csv(cp, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame()) else tryCatch(cfbfastR::load_ncaa_mfb_rosters(seasons = yr), error = function(e) data.frame())
    if (nrow(rr) && !file.exists(cp)) {
      rr2 <- data.frame(season = yr,
        name_key_24 = person_key24(char_alias24(rr, c("player_name", "athlete_name", "name", "player"))),
        position = pos24(char_alias24(rr, c("position", "pos"))),
        college_starts_season_24 = num_alias24(rr, c("starts", "games_started"), 0), stringsAsFactors = FALSE) |>
        dplyr::filter(position %in% POSITIONS, nzchar(name_key_24)) |>
        dplyr::group_by(season, name_key_24, position) |> dplyr::summarise(college_starts_season_24 = max(college_starts_season_24, na.rm = TRUE), .groups = "drop")
      readr::write_csv(rr2, cp); rr <- rr2
    }
    if (nrow(rr)) start_parts[[length(start_parts) + 1]] <- rr[, intersect(c("season", "name_key_24", "position", "college_starts_season_24"), names(rr)), drop = FALSE]
    rm(rr); gc()
  }
  college_starts <- dplyr::bind_rows(start_parts)
}

if (nrow(college)) {
  # Team shares are calculated inside the season/team environment before any
  # NFL matching, preventing NFL outcomes from leaking into prospect features.
  team_tot <- college |> dplyr::group_by(season, college_team_24) |>
    dplyr::summarise(team_pass_att = sum(pass_att, na.rm = TRUE), team_rush_att = sum(rush_att, na.rm = TRUE), team_rec_yd = sum(rec_yd, na.rm = TRUE), .groups = "drop")
  college <- college |> dplyr::left_join(team_tot, by = c("season", "college_team_24")) |>
    dplyr::mutate(pass_share = dplyr::if_else(team_pass_att > 0, pass_att / team_pass_att, 0),
                  rush_share = dplyr::if_else(team_rush_att > 0, rush_att / team_rush_att, 0),
                  rec_yd_share = dplyr::if_else(team_rec_yd > 0, rec_yd / team_rec_yd, 0),
                  ypa = dplyr::if_else(pass_att > 0, pass_yd / pass_att, 0),
                  ypc = dplyr::if_else(rush_att > 0, rush_yd / rush_att, 0),
                  ypr = dplyr::if_else(rec > 0, rec_yd / rec, 0))
  if (nrow(college_starts)) college <- college |> dplyr::left_join(college_starts, by = c("season", "name_key_24", "position"))
} else {
  college <- data.frame()
}

# Match college seasons to NFL identities. Exact normalized full-name + position
# is preferred. 2.4.3 adds a conservative second pass for nickname/legal-name
# differences using first initial + last name + position + draft-time window.
college_profile <- data.frame(player_id = character(), stringsAsFactors = FALSE)
if (nrow(college)) {
  nfl_match_meta24 <- nfl_base |>
    dplyr::select(player_id, birth_date_24, draft_year_24, rookie_year_24, c_school, college_name_meta_24) |>
    dplyr::mutate(anchor_year_24 = dplyr::coalesce(draft_year_24, rookie_year_24))

  exact <- nfl_name_alias24 |>
    dplyr::select(player_id, position, name_key_24) |>
    dplyr::distinct() |>
    dplyr::inner_join(college, by = c("name_key_24", "position")) |>
    dplyr::left_join(nfl_match_meta24, by = "player_id") |>
    dplyr::mutate(anchor_year = dplyr::coalesce(anchor_year_24, as.numeric(season) + 1)) |>
    dplyr::filter(season <= anchor_year, season >= anchor_year - 6)

  exact_ids <- unique(exact$player_id)
  relaxed_alias <- nfl_name_alias24 |>
    dplyr::filter(nzchar(first_initial_24), nzchar(last_key_24), !player_id %in% exact_ids) |>
    dplyr::select(player_id, position, first_initial_24, last_key_24) |>
    dplyr::distinct()

  relaxed <- data.frame()
  if (nrow(relaxed_alias)) {
    relaxed <- relaxed_alias |>
      dplyr::inner_join(college, by = c("position", "first_initial_24", "last_key_24")) |>
      dplyr::left_join(nfl_match_meta24, by = "player_id") |>
      # Relaxed matching is permitted only when an actual NFL draft/rookie anchor
      # exists. This prevents old same-initial/surname collisions from being guessed.
      dplyr::filter(is.finite(anchor_year_24), season <= anchor_year_24, season >= anchor_year_24 - 6) |>
      dplyr::group_by(player_id, season) |>
      dplyr::filter(dplyr::n_distinct(name_key_24) == 1) |>
      dplyr::ungroup() |>
      dplyr::group_by(season, position, name_key_24) |>
      dplyr::filter(dplyr::n_distinct(player_id) == 1) |>
      dplyr::ungroup() |>
      dplyr::mutate(anchor_year = anchor_year_24)
  }

  matched <- dplyr::bind_rows(exact, relaxed) |>
    dplyr::distinct(player_id, season, name_key_24, position, college_team_24, .keep_all = TRUE)

  cat("[2.4.3 COLLEGE] Aggregated player-seasons: ", nrow(college),
      "; exact matched NFL players: ", dplyr::n_distinct(exact$player_id),
      "; relaxed matched NFL players: ", if (nrow(relaxed)) dplyr::n_distinct(relaxed$player_id) else 0,
      "; total matched NFL players: ", dplyr::n_distinct(matched$player_id), "\n", sep = "")
  if (!nrow(matched)) {
    warning("[2.4.3 COLLEGE] College data loaded but no NCAA↔NFL identities matched. Continuing safely with college_missing_24=1; combine/draft/NFL signals remain active.")
  }

  if (nrow(matched)) {
    matched <- matched |> dplyr::group_by(player_id) |> dplyr::mutate(final_college_season = max(season, na.rm = TRUE)) |> dplyr::ungroup()
    final <- matched |> dplyr::filter(season == final_college_season) |> dplyr::group_by(player_id) |>
      dplyr::summarise(
        college_final_pass_att_24 = sum(pass_att), college_final_pass_yd_24 = sum(pass_yd), college_final_pass_td_24 = sum(pass_td), college_final_int_24 = sum(ints),
        college_final_rush_att_24 = sum(rush_att), college_final_rush_yd_24 = sum(rush_yd), college_final_rush_td_24 = sum(rush_td),
        college_final_rec_24 = sum(rec), college_final_rec_yd_24 = sum(rec_yd), college_final_rec_td_24 = sum(rec_td),
        college_final_pass_share_24 = max(pass_share, na.rm = TRUE), college_final_rush_share_24 = max(rush_share, na.rm = TRUE),
        college_final_rec_yd_share_24 = max(rec_yd_share, na.rm = TRUE),
        college_final_ypa_24 = ifelse(sum(pass_att) > 0, sum(pass_yd) / sum(pass_att), 0),
        college_final_ypc_24 = ifelse(sum(rush_att) > 0, sum(rush_yd) / sum(rush_att), 0),
        college_final_ypr_24 = ifelse(sum(rec) > 0, sum(rec_yd) / sum(rec), 0), .groups = "drop")
    career <- matched |> dplyr::group_by(player_id) |>
      dplyr::summarise(
        college_seasons_24 = dplyr::n_distinct(season),
        college_games_24 = sum(dplyr::coalesce(college_games_season_24, 0), na.rm = TRUE),
        college_starts_24 = if ("college_starts_season_24" %in% names(matched)) sum(dplyr::coalesce(college_starts_season_24, 0), na.rm = TRUE) else 0,
        college_career_pass_att_24 = sum(pass_att), college_career_pass_yd_24 = sum(pass_yd), college_career_pass_td_24 = sum(pass_td), college_career_int_24 = sum(ints),
        college_career_rush_att_24 = sum(rush_att), college_career_rush_yd_24 = sum(rush_yd), college_career_rush_td_24 = sum(rush_td),
        college_career_rec_24 = sum(rec), college_career_rec_yd_24 = sum(rec_yd), college_career_rec_td_24 = sum(rec_td),
        college_peak_pass_share_24 = max(pass_share, na.rm = TRUE), college_peak_rush_share_24 = max(rush_share, na.rm = TRUE),
        college_peak_rec_yd_share_24 = max(rec_yd_share, na.rm = TRUE), .groups = "drop")
    breakout <- matched |> dplyr::rowwise() |> dplyr::mutate(
      college_age_season_24 = ifelse(!is.na(birth_date_24), as.numeric(as.Date(paste0(season, "-10-01")) - birth_date_24) / 365.25, NA_real_),
      breakout_hit = dplyr::case_when(position == "QB" ~ pass_share >= 0.50,
                                      position == "RB" ~ rush_share >= 0.25,
                                      position %in% c("WR", "TE") ~ rec_yd_share >= 0.20,
                                      TRUE ~ FALSE)) |> dplyr::ungroup() |>
      dplyr::filter(breakout_hit, is.finite(college_age_season_24)) |>
      dplyr::group_by(player_id) |> dplyr::summarise(college_breakout_age_24 = min(college_age_season_24), .groups = "drop")
    college_profile <- career |> dplyr::left_join(final, by = "player_id") |> dplyr::left_join(breakout, by = "player_id")
  }
  rm(exact, relaxed, matched, nfl_match_meta24); gc()
}

if (!"player_id" %in% names(college_profile)) college_profile$player_id <- character(nrow(college_profile))
college_profile$player_id <- as.character(college_profile$player_id)
profile <- nfl_base |> dplyr::left_join(college_profile, by = "player_id")
for (nm in setdiff(MODEL24_TALENT_FEATURES, names(profile))) profile[[nm]] <- NA_real_
profile$college_missing_24 <- as.numeric(!is.finite(suppressWarnings(as.numeric(profile$college_seasons_24))) | wk24_num(profile$college_seasons_24) <= 0)

# Position-aware production score, then class-position z score. This is a
# descriptive signal only; the rookie talent ridge learns the actual weights.
profile$college_production_raw_24 <- dplyr::case_when(
  profile$position == "QB" ~ 0.45 * wk24_num(profile$college_peak_pass_share_24) + 0.20 * wk24_num(profile$college_final_pass_share_24) + 0.15 * pmin(wk24_num(profile$college_final_ypa_24) / 10, 1.5) + 0.20 * pmin(wk24_num(profile$college_final_rush_yd_24) / 700, 1.5),
  profile$position == "RB" ~ 0.45 * wk24_num(profile$college_peak_rush_share_24) + 0.25 * wk24_num(profile$college_final_rush_share_24) + 0.15 * pmin(wk24_num(profile$college_final_rec_24) / 40, 1.5) + 0.15 * pmin(wk24_num(profile$college_final_ypc_24) / 7, 1.5),
  profile$position %in% c("WR", "TE") ~ 0.50 * wk24_num(profile$college_peak_rec_yd_share_24) + 0.25 * wk24_num(profile$college_final_rec_yd_share_24) + 0.15 * pmin(wk24_num(profile$college_final_rec_24) / 80, 1.5) + 0.10 * pmin(wk24_num(profile$college_final_ypr_24) / 18, 1.5),
  TRUE ~ 0)
profile <- profile |> dplyr::group_by(position, draft_year_24) |>
  dplyr::mutate(college_production_score_z_24 = ifelse(dplyr::n() >= 5 && stats::sd(college_production_raw_24, na.rm = TRUE) > 1e-8,
                                                        as.numeric(scale(college_production_raw_24)), 0)) |>
  dplyr::ungroup()

# Replace nonfinite model feature values with NA, not zero. Ridge preparation
# uses the corresponding missing flags and a stable standardized zero fill.
for (nm in MODEL24_TALENT_FEATURES) if (nm %in% names(profile) && is.numeric(profile[[nm]])) profile[[nm]][!is.finite(profile[[nm]])] <- NA_real_
readr::write_csv(profile, "data/processed/player_talent_raw_2_4.csv")
cat("[2.4 TALENT] Raw talent profiles: ", nrow(profile), "; with college match: ", sum(profile$college_missing_24 == 0, na.rm = TRUE), "\n", sep = "")
rm(college, college_parts, college_profile, college_starts); gc()

# ------------------------------------------------------------
# 3) Rookie NFL outcomes + chronological talent prior
# ------------------------------------------------------------
stat_seasons <- MODEL24_COLLEGE_START:TRAIN_END
stats <- tryCatch(nflreadr::load_player_stats(seasons = stat_seasons, summary_level = "reg"), error = function(e) {
  if (file.exists("data/raw/player_stats.csv")) readr::read_csv("data/raw/player_stats.csv", show_col_types = FALSE, progress = FALSE) else data.frame()
})
if (!nrow(stats)) stop("2.4 could not obtain NFL season player stats for rookie talent targets.")

sid <- char_alias24(stats, c("player_id", "gsis_id"))
sseason <- num_alias24(stats, c("season"), NA_real_)
spos <- pos24(char_alias24(stats, c("position", "pos")))
games <- num_alias24(stats, c("games", "games_played"), 0)
# Use the project's scoring rules rather than nflreadr's full-PPR shortcut so
# the prospect target is aligned with the same half-PPR system as the model.
fp <- SCORING$pass_yd * num_alias24(stats, c("passing_yards")) + SCORING$pass_td * num_alias24(stats, c("passing_tds")) +
  SCORING$interception * num_alias24(stats, c("passing_interceptions", "interceptions")) + SCORING$rush_yd * num_alias24(stats, c("rushing_yards")) +
  SCORING$rush_td * num_alias24(stats, c("rushing_tds")) + SCORING$reception * num_alias24(stats, c("receptions")) +
  SCORING$rec_yd * num_alias24(stats, c("receiving_yards")) + SCORING$rec_td * num_alias24(stats, c("receiving_tds")) +
  SCORING$fumble_lost * num_alias24(stats, c("rushing_fumbles_lost", "receiving_fumbles_lost", "fumbles_lost"), 0)
stat_std <- data.frame(player_id = sid, season = sseason, position = spos, games = games, fantasy_points = fp, stringsAsFactors = FALSE) |>
  dplyr::filter(position %in% POSITIONS, nzchar(player_id), is.finite(season)) |>
  dplyr::group_by(player_id, season, position) |>
  dplyr::summarise(games = max(games, na.rm = TRUE), fantasy_points = sum(fantasy_points, na.rm = TRUE), .groups = "drop")

rookies <- profile |> dplyr::filter(is.finite(draft_year_24), draft_year_24 >= MODEL24_COLLEGE_START, draft_year_24 <= TRAIN_END) |>
  dplyr::select(player_id, player_name, position, draft_year_24, dplyr::all_of(intersect(MODEL24_TALENT_FEATURES, names(profile)))) |>
  dplyr::left_join(stat_std, by = c("player_id", "position", "draft_year_24" = "season")) |>
  dplyr::mutate(games = dplyr::coalesce(games, 0), fantasy_points = dplyr::coalesce(fantasy_points, 0),
                rookie_fppg_24 = dplyr::if_else(games > 0, fantasy_points / games, 0))
readr::write_csv(rookies, "data/processed/rookie_outcomes_2_4.csv")

fit_talent_ridge24 <- function(train, pos) {
  tr <- train |> dplyr::filter(position == pos)
  feats <- intersect(MODEL24_TALENT_FEATURES, names(tr))
  # Drop completely unavailable / constant columns.
  feats <- feats[vapply(feats, function(f) {
    x <- suppressWarnings(as.numeric(tr[[f]])); x[!is.finite(x)] <- 0; stats::sd(x) > 1e-9
  }, logical(1))]
  if (nrow(tr) < MODEL24_TALENT_MIN_TRAIN || length(feats) < 2) return(list(model = NULL, features = feats, lambda = NA_real_))
  years <- sort(unique(as.integer(tr$draft_year_24)))
  if (length(years) < 2) return(list(model = NULL, features = feats, lambda = NA_real_))
  vy <- max(years); a <- tr[tr$draft_year_24 < vy, , drop = FALSE]; b <- tr[tr$draft_year_24 == vy, , drop = FALSE]
  if (nrow(a) < MODEL24_TALENT_MIN_TRAIN || nrow(b) < 8) return(list(model = NULL, features = feats, lambda = NA_real_))
  scored <- list()
  for (lam in MODEL24_TALENT_RIDGE_LAMBDAS) {
    m <- fit_ridge23(a, feats, "rookie_fppg_24", lam)
    if (is.null(m)) next
    p <- pmax(0, predict_ridge23(m, b)); met <- wk24_metrics(b$rookie_fppg_24, p)
    scored[[length(scored) + 1]] <- data.frame(lambda = lam, MAE = met$MAE, RMSE = met$RMSE, correlation = met$correlation)
  }
  sc <- dplyr::bind_rows(scored)
  if (!nrow(sc)) return(list(model = NULL, features = feats, lambda = NA_real_))
  sc <- sc |> dplyr::arrange(MAE, RMSE, dplyr::desc(correlation)); lam <- sc$lambda[1]
  list(model = fit_ridge23(tr, feats, "rookie_fppg_24", lam), features = feats, lambda = lam)
}

# Honest draft-class OOF talent predictions.
talent_oof_rows <- list()
classes <- sort(unique(as.integer(rookies$draft_year_24)))
for (yr in classes) {
  for (p in POSITIONS) {
    tr <- rookies |> dplyr::filter(position == p, draft_year_24 < yr)
    te <- rookies |> dplyr::filter(position == p, draft_year_24 == yr)
    if (!nrow(te)) next
    obj <- fit_talent_ridge24(tr, p)
    fallback <- if (nrow(tr)) mean(tr$rookie_fppg_24, na.rm = TRUE) else 0
    pred <- if (!is.null(obj$model)) pmax(0, predict_ridge23(obj$model, te)) else rep(fallback, nrow(te))
    talent_oof_rows[[length(talent_oof_rows) + 1]] <- data.frame(player_id = te$player_id, position = p, draft_year_24 = yr,
      talent_prior_fppg_24 = pred, rookie_actual_fppg_24 = te$rookie_fppg_24,
      talent_prior_source_24 = ifelse(is.null(obj$model), "position_prior", "chronological_college_combine_ridge"), stringsAsFactors = FALSE)
  }
}
talent_oof <- dplyr::bind_rows(talent_oof_rows)
readr::write_csv(talent_oof, "data/processed/talent_prior_oof_2_4.csv")

# Final 2026/player talent prior models are trained only on completed draft
# classes through 2025. Every active player receives the prior, but its weekly
# weight decays rapidly as real NFL games accumulate.
profile$talent_prior_fppg_24 <- 0
profile$talent_prior_source_24 <- "position_prior"
talent_signal_rows <- list(); talent_coef_rows <- list()
for (p in POSITIONS) {
  tr <- rookies |> dplyr::filter(position == p)
  obj <- fit_talent_ridge24(tr, p)
  saveRDS(obj, paste0("models/player_talent_2_4_", p, ".rds"))
  ix <- profile$position == p
  fallback <- if (nrow(tr)) mean(tr$rookie_fppg_24, na.rm = TRUE) else 0
  profile$talent_prior_fppg_24[ix] <- if (!is.null(obj$model)) pmax(0, predict_ridge23(obj$model, profile[ix, , drop = FALSE])) else fallback
  profile$talent_prior_source_24[ix] <- ifelse(is.null(obj$model), "position_prior", "college_combine_draft_ridge")
  for (f in intersect(MODEL24_TALENT_FEATURES, names(tr))) {
    x <- suppressWarnings(as.numeric(tr[[f]])); y <- tr$rookie_fppg_24
    k <- is.finite(x) & is.finite(y)
    if (sum(k) >= 20 && stats::sd(x[k]) > 1e-9) talent_signal_rows[[length(talent_signal_rows) + 1]] <- data.frame(
      position = p, feature = f, n = sum(k), rookie_fppg_pearson = wk24_safe_cor(x[k], y[k]),
      rookie_fppg_spearman = wk24_safe_cor(x[k], y[k], "spearman"), stringsAsFactors = FALSE)
  }
  if (!is.null(obj$model)) {
    b <- obj$model$beta[-1]; rawb <- b / obj$model$sds
    talent_coef_rows[[length(talent_coef_rows) + 1]] <- data.frame(position = p, feature = obj$model$features,
      standardized_ridge_coefficient = as.numeric(b), raw_unit_slope = as.numeric(rawb), lambda = obj$model$lambda, stringsAsFactors = FALSE)
  }
}
profile$talent_profile_available_24 <- as.numeric(profile$combine_missing_24 == 0 | profile$college_missing_24 == 0 | is.finite(profile$draft_overall_24))
readr::write_csv(profile, "data/processed/player_talent_profiles_2_4.csv")
talent_audit <- dplyr::full_join(dplyr::bind_rows(talent_signal_rows), dplyr::bind_rows(talent_coef_rows), by = c("position", "feature"))
readr::write_csv(talent_audit, "output/rookie_talent_signal_audit_2_4.csv")
rookie_rank <- profile |> dplyr::filter(draft_year_24 == CURRENT_SEASON, position %in% POSITIONS) |>
  dplyr::group_by(position) |> dplyr::arrange(dplyr::desc(talent_prior_fppg_24), .by_group = TRUE) |>
  dplyr::mutate(talent_rank_position_24 = dplyr::row_number()) |> dplyr::ungroup() |>
  dplyr::select(player_id, player_name, position, talent_rank_position_24, talent_prior_fppg_24,
                draft_round_24, draft_overall_24, age_at_draft_24, combine_athleticism_z_24,
                college_production_score_z_24, college_breakout_age_24, combine_missing_24, college_missing_24)
readr::write_csv(rookie_rank, paste0("output/rookie_talent_rankings_", CURRENT_SEASON, ".csv"))
cat("[2.4 TALENT] Rookie outcomes: ", nrow(rookies), "; 2026 rookies ranked: ", nrow(rookie_rank), "\n", sep = "")
rm(stats, stat_std, rookies); gc()

# ------------------------------------------------------------
# 4) Opponent defensive style: blitz / rushers / pressure / coverage
# ------------------------------------------------------------
# Every raw game is aggregated first, then the forecast feature is a lagged
# rolling mean of PRIOR games only. FTN blitz is used when available; otherwise
# 5+ pass rushers from participation is a transparent historical proxy.

schedule_years <- MODEL24_NFL_SIGNAL_START:CURRENT_SEASON
schedules <- tryCatch(nflreadr::load_schedules(seasons = schedule_years), error = function(e) data.frame())
if (!nrow(schedules)) warning("[2.4 DEFENSE] Schedule unavailable; opponent style context will remain empty.")

schedule_std <- if (nrow(schedules)) data.frame(
  season = num_alias24(schedules, c("season"), NA_real_), week = num_alias24(schedules, c("week"), NA_real_),
  game_id = char_alias24(schedules, c("game_id")), home_team = char_alias24(schedules, c("home_team")),
  away_team = char_alias24(schedules, c("away_team")), stringsAsFactors = FALSE) |>
  dplyr::filter(is.finite(season), is.finite(week), nzchar(game_id)) else data.frame()

participation_parts <- list()
if (isTRUE(MODEL24_USE_PARTICIPATION) && nrow(schedule_std)) {
  for (yr in MODEL24_NFL_SIGNAL_START:TRAIN_END) {
    cp <- file.path(MODEL24_DEFENSE_CHECKPOINT_DIR, paste0("participation_style_", yr, ".csv"))
    cached_ok <- FALSE
    if (file.exists(cp)) {
      z <- tryCatch(readr::read_csv(cp, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
      cached_ok <- nrow(z) > 0 && all(c("season", "week", "game_id", "defense") %in% names(z))
    }
    if (cached_ok) {
      cat("[2.4.2 DEFENSE] Reusing valid participation ", yr, ".\n", sep = "")
    } else {
      cat("[2.4.2 DEFENSE] Building participation ", yr, "...\n", sep = "")
      raw <- tryCatch(nflreadr::load_participation(seasons = yr), error = function(e) {
        warning("Participation unavailable for ", yr, ": ", conditionMessage(e)); data.frame()
      })
      if (nrow(raw)) {
        game <- char_alias24(raw, c("nflverse_game_id", "game_id", "old_game_id")); play <- num_alias24(raw, c("nflverse_play_id", "play_id"), NA_real_)
        poss <- char_alias24(raw, c("possession_team", "posteam", "offense_team"))
        rushers <- num_alias24(raw, c("number_of_pass_rushers", "n_pass_rushers"), NA_real_)
        pressure_raw <- wk24_pick(raw, c("was_pressure", "pressure"), NA)
        manzone <- tolower(char_alias24(raw, c("defense_man_zone_type", "man_zone_type")))
        coverage <- tolower(char_alias24(raw, c("defense_coverage_type", "coverage_type")))
        play_level <- data.frame(game_id = game, play_id = play, possession_team = poss, number_of_pass_rushers = rushers,
          pressure = bool_num_na24(pressure_raw), man = contains_num_na24(manzone, "man"), zone = contains_num_na24(manzone, "zone"),
          cover0 = contains_num_na24(coverage, "cover[ -]?0|cov[ -]?0"), cover1 = contains_num_na24(coverage, "cover[ -]?1|cov[ -]?1"),
          cover2 = contains_num_na24(coverage, "cover[ -]?2|cov[ -]?2"), cover3 = contains_num_na24(coverage, "cover[ -]?3|cov[ -]?3"),
          cover4 = contains_num_na24(coverage, "cover[ -]?4|cov[ -]?4"), cover6 = contains_num_na24(coverage, "cover[ -]?6|cov[ -]?6"), stringsAsFactors = FALSE) |>
          dplyr::filter(nzchar(game_id)) |>
          dplyr::left_join(schedule_std |> dplyr::filter(season == yr) |> dplyr::select(game_id, season, week, home_team, away_team), by = "game_id") |>
          dplyr::mutate(defense = dplyr::case_when(possession_team == home_team ~ away_team, possession_team == away_team ~ home_team, TRUE ~ NA_character_)) |>
          dplyr::filter(!is.na(defense))
        keycp <- file.path(MODEL24_DEFENSE_CHECKPOINT_DIR, paste0("participation_defkey_", yr, ".csv"))
        if (nrow(play_level)) readr::write_csv(play_level |> dplyr::select(game_id, play_id, defense) |> dplyr::distinct(), keycp)
        z <- play_level |>
          dplyr::group_by(season, week, game_id, defense) |>
          dplyr::summarise(rushers5_rate = mean(number_of_pass_rushers >= 5, na.rm = TRUE),
                           avg_pass_rushers = mean(number_of_pass_rushers, na.rm = TRUE), pressure_rate = mean(pressure, na.rm = TRUE),
                           man_rate = mean(man, na.rm = TRUE), zone_rate = mean(zone, na.rm = TRUE),
                           cover0_rate = mean(cover0, na.rm = TRUE), cover1_rate = mean(cover1, na.rm = TRUE), cover2_rate = mean(cover2, na.rm = TRUE),
                           cover3_rate = mean(cover3, na.rm = TRUE), cover4_rate = mean(cover4, na.rm = TRUE), cover6_rate = mean(cover6, na.rm = TRUE), .groups = "drop")
        if (nrow(z)) readr::write_csv(z, cp)
        rm(play_level)
      } else z <- data.frame()
      rm(raw); gc()
    }
    if (nrow(z)) participation_parts[[length(participation_parts) + 1]] <- z
  }
}
participation_style <- dplyr::bind_rows(participation_parts)

# FTN charting: compact game/play fields only, then aggregate by defense. If
# current-season FTN is available it can update 2026 after games are charted.
ftn_parts <- list()
if (isTRUE(MODEL24_USE_FTN_CHARTING) && nrow(schedule_std)) {
  ftn_start <- max(2022, MODEL24_NFL_SIGNAL_START)
  for (yr in ftn_start:CURRENT_SEASON) {
    cat("[2.4.2 DEFENSE] FTN ", yr, "...\n", sep = "")
    cp <- file.path(MODEL24_DEFENSE_CHECKPOINT_DIR, paste0("ftn_style_", yr, ".csv"))
    ftn_cached_ok <- FALSE
    if (file.exists(cp) && yr <= TRAIN_END) {
      z <- tryCatch(readr::read_csv(cp, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
      ftn_cached_ok <- nrow(z) > 0 && all(c("season", "week", "game_id", "defense") %in% names(z))
    }
    if (!ftn_cached_ok) {
      raw <- tryCatch(nflreadr::load_ftn_charting(seasons = yr), error = function(e) data.frame())
      if (nrow(raw)) {
        z0 <- data.frame(game_id = char_alias24(raw, c("nflverse_game_id", "game_id")), play_id = num_alias24(raw, c("nflverse_play_id", "play_id"), NA_real_),
          n_blitzers = num_alias24(raw, c("n_blitzers", "number_of_blitzers"), NA_real_),
          n_pass_rushers = num_alias24(raw, c("n_pass_rushers", "number_of_pass_rushers"), NA_real_),
          n_box = num_alias24(raw, c("n_defense_box", "defenders_in_box", "n_box"), NA_real_), stringsAsFactors = FALSE) |>
          dplyr::filter(nzchar(game_id))
        # Determine defense from the compact participation key when historical
        # participation exists. This avoids loading full PBP for every year.
        def_key <- data.frame(game_id = character(), play_id = numeric(), defense = character(), stringsAsFactors = FALSE)
        keycp <- file.path(MODEL24_DEFENSE_CHECKPOINT_DIR, paste0("participation_defkey_", yr, ".csv"))
        if (file.exists(keycp)) def_key <- tryCatch(readr::read_csv(keycp, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
        if (!all(c("game_id", "play_id", "defense") %in% names(def_key))) {
          def_key <- data.frame(game_id = character(), play_id = numeric(), defense = character(), stringsAsFactors = FALSE)
        }
        if (!nrow(def_key)) {
          # Current-season FTN may expose possession team directly. Use it when
          # present; otherwise fall back to one memory-released PBP season.
          poss_ftn <- char_alias24(raw, c("possession_team", "posteam", "offense_team"))
          if (any(nzchar(poss_ftn))) {
            def_key <- data.frame(game_id = char_alias24(raw, c("nflverse_game_id", "game_id")), play_id = num_alias24(raw, c("nflverse_play_id", "play_id"), NA_real_), possession_team = poss_ftn, stringsAsFactors = FALSE) |>
              dplyr::left_join(schedule_std |> dplyr::filter(season == yr) |> dplyr::select(game_id, home_team, away_team), by = "game_id") |>
              dplyr::mutate(defense = dplyr::case_when(possession_team == home_team ~ away_team, possession_team == away_team ~ home_team, TRUE ~ NA_character_)) |>
              dplyr::filter(!is.na(defense)) |> dplyr::select(game_id, play_id, defense) |> dplyr::distinct()
          }
        }
        if (!nrow(def_key)) {
          pbp <- tryCatch(nflreadr::load_pbp(seasons = yr), error = function(e) data.frame())
          if (nrow(pbp)) def_key <- data.frame(game_id = char_alias24(pbp, c("game_id", "nflverse_game_id")), play_id = num_alias24(pbp, c("play_id", "nflverse_play_id"), NA_real_), defense = char_alias24(pbp, c("defteam")), stringsAsFactors = FALSE) |>
            dplyr::filter(nzchar(game_id), nzchar(defense)) |> dplyr::distinct(game_id, play_id, .keep_all = TRUE)
          rm(pbp); gc()
        }
        if (nrow(def_key)) {
          z <- z0 |> dplyr::left_join(def_key, by = c("game_id", "play_id")) |>
            dplyr::left_join(schedule_std |> dplyr::filter(season == yr) |> dplyr::select(game_id, season, week), by = "game_id") |>
            dplyr::filter(!is.na(defense), nzchar(defense)) |>
            dplyr::group_by(season, week, game_id, defense) |>
            dplyr::summarise(blitz_rate_direct = mean(n_blitzers > 0, na.rm = TRUE), avg_box = mean(n_box, na.rm = TRUE), .groups = "drop")
        } else {
          warning("[2.4.2 FTN] No defense key for ", yr, "; skipping FTN style for that season instead of failing.")
          z <- data.frame()
        }
        if (nrow(z) && yr <= TRAIN_END) readr::write_csv(z, cp)
        rm(z0, def_key); gc()
      } else z <- data.frame()
      rm(raw); gc()
    }
    if (nrow(z)) ftn_parts[[length(ftn_parts) + 1]] <- z
  }
}
ftn_style <- dplyr::bind_rows(ftn_parts)
cat("[2.4.2 DEFENSE] Participation game rows: ", nrow(participation_style),
    "; FTN game rows: ", nrow(ftn_style), "\n", sep = "")

observed <- participation_style
if (!nrow(observed) && nrow(ftn_style)) observed <- ftn_style
if (nrow(observed)) {
  if (nrow(ftn_style)) observed <- observed |> dplyr::full_join(ftn_style, by = c("season", "week", "game_id", "defense"))
  if (!"blitz_rate_direct" %in% names(observed)) observed$blitz_rate_direct <- NA_real_
  if (!"avg_box" %in% names(observed)) observed$avg_box <- NA_real_
  if (!"rushers5_rate" %in% names(observed)) observed$rushers5_rate <- NA_real_
  observed$blitz_rate <- ifelse(is.finite(observed$blitz_rate_direct), observed$blitz_rate_direct, observed$rushers5_rate)
}

# Defense game spine includes all schedule games, including 2026 future games.
spine <- if (nrow(schedule_std)) dplyr::bind_rows(
  schedule_std |> dplyr::transmute(season, week, game_id, defense = home_team),
  schedule_std |> dplyr::transmute(season, week, game_id, defense = away_team)) |>
  dplyr::filter(nzchar(defense)) |> dplyr::distinct(season, week, game_id, defense) else data.frame()

style_cols <- c("blitz_rate", "rushers5_rate", "avg_pass_rushers", "pressure_rate", "man_rate", "zone_rate",
                "cover0_rate", "cover1_rate", "cover2_rate", "cover3_rate", "cover4_rate", "cover6_rate", "avg_box")
if (nrow(spine)) {
  obs_keep <- if (nrow(observed)) observed[, intersect(c("season", "week", "game_id", "defense", style_cols), names(observed)), drop = FALSE] else data.frame()
  style <- if (nrow(obs_keep)) spine |> dplyr::left_join(obs_keep, by = c("season", "week", "game_id", "defense")) else spine
  for (nm in setdiff(style_cols, names(style))) style[[nm]] <- NA_real_
  style <- style |> dplyr::arrange(defense, season, week)

  # Prior-season fallback profiles, still strictly pre-kickoff.
  final_prev <- list(); roll_rows <- list()
  for (def in unique(style$defense)) {
    idx <- which(style$defense == def); dd <- style[idx, , drop = FALSE]
    for (i in seq_len(nrow(dd))) {
      yr <- dd$season[i]
      prior_same <- which(dd$season == yr & seq_len(nrow(dd)) < i)
      # only rows with observed style count as prior games
      prior_same <- prior_same[is.finite(dd$pressure_rate[prior_same]) | is.finite(dd$blitz_rate[prior_same]) | is.finite(dd$rushers5_rate[prior_same])]
      use <- tail(prior_same, 4)
      if (!length(use)) {
        prior_prev <- which(dd$season < yr & (is.finite(dd$pressure_rate) | is.finite(dd$blitz_rate) | is.finite(dd$rushers5_rate)))
        use <- tail(prior_prev, 4)
      }
      vals <- list()
      for (nm in style_cols) vals[[nm]] <- if (length(use) && any(is.finite(dd[[nm]][use]))) mean(dd[[nm]][use], na.rm = TRUE) else NA_real_
      roll_rows[[length(roll_rows) + 1]] <- data.frame(season = yr, week = dd$week[i], game_id = dd$game_id[i], defense = def,
        n_prior_games_24 = length(use), blitz_rate_roll4_24 = vals$blitz_rate, rushers5_rate_roll4_24 = vals$rushers5_rate,
        avg_pass_rushers_roll4_24 = vals$avg_pass_rushers, pressure_rate_roll4_24 = vals$pressure_rate,
        man_rate_roll4_24 = vals$man_rate, zone_rate_roll4_24 = vals$zone_rate,
        cover0_rate_roll4_24 = vals$cover0_rate, cover1_rate_roll4_24 = vals$cover1_rate, cover2_rate_roll4_24 = vals$cover2_rate,
        cover3_rate_roll4_24 = vals$cover3_rate, cover4_rate_roll4_24 = vals$cover4_rate, cover6_rate_roll4_24 = vals$cover6_rate,
        avg_box_roll4_24 = vals$avg_box, stringsAsFactors = FALSE)
    }
  }
  style_roll <- dplyr::bind_rows(roll_rows)
  names(style_roll) <- sub("^(blitz|rushers5|avg_pass_rushers|pressure|man|zone|cover0|cover1|cover2|cover3|cover4|cover6|avg_box)_rate_roll4_24$", "\\1_rate_roll4_24", names(style_roll))
  # Rename exactly to the configured opponent feature names.
  style_roll <- style_roll |> dplyr::rename(
    opp_blitz_rate_roll4_24 = blitz_rate_roll4_24,
    opp_rushers5_rate_roll4_24 = rushers5_rate_roll4_24,
    opp_avg_pass_rushers_roll4_24 = avg_pass_rushers_roll4_24,
    opp_pressure_rate_roll4_24 = pressure_rate_roll4_24,
    opp_man_rate_roll4_24 = man_rate_roll4_24,
    opp_zone_rate_roll4_24 = zone_rate_roll4_24,
    opp_cover0_rate_roll4_24 = cover0_rate_roll4_24,
    opp_cover1_rate_roll4_24 = cover1_rate_roll4_24,
    opp_cover2_rate_roll4_24 = cover2_rate_roll4_24,
    opp_cover3_rate_roll4_24 = cover3_rate_roll4_24,
    opp_cover4_rate_roll4_24 = cover4_rate_roll4_24,
    opp_cover6_rate_roll4_24 = cover6_rate_roll4_24,
    opp_avg_box_roll4_24 = avg_box_roll4_24)
  readr::write_csv(style_roll, "data/processed/defense_style_context_2_4.csv")
  readr::write_csv(style_roll |> dplyr::filter(season == CURRENT_SEASON), paste0("data/processed/defense_style_projection_", CURRENT_SEASON, "_2_4.csv"))
  cat("[2.4 DEFENSE] Lagged defense-style rows: ", nrow(style_roll), "\n", sep = "")
} else {
  style_roll <- data.frame()
  readr::write_csv(data.frame(), "data/processed/defense_style_context_2_4.csv")
  warning("[2.4 DEFENSE] No defensive style rows built; signal model will safely omit these features.")
}
rm(participation_parts, participation_style, ftn_parts, ftn_style, observed, schedules); gc()

# ------------------------------------------------------------
# 5) Optional historical expected-fantasy-opportunity signal
# ------------------------------------------------------------
xfp_schema24 <- function() data.frame(
  season = integer(), week = integer(), player_id = character(),
  xfp_24 = numeric(), fpoe_24 = numeric(), xfp_roll3_24 = numeric(),
  xfp_roll5_24 = numeric(), fpoe_roll3_24 = numeric(), fpoe_roll5_24 = numeric(),
  stringsAsFactors = FALSE)

build_xfp_weekly24 <- function(raw, yr) {
  if (is.null(raw) || !nrow(raw)) return(xfp_schema24()[0, c("season", "week", "player_id", "xfp_24", "fpoe_24")])
  # Some ffopportunity releases expose a combined expected-FP field; others
  # expose expected component statistics. Prefer the combined field when present,
  # otherwise reconstruct expected fantasy points with the project's scoring.
  xfp <- num_alias24(raw, c("expected_fantasy_points", "fantasy_points_exp", "fantasy_points_expected", "xFP", "xfp", "weighted_opportunity"), NA_real_)
  if (!any(is.finite(xfp))) {
    xfp <- SCORING$pass_yd * num_alias24(raw, c("pass_yards_gained_exp", "passing_yards_exp", "pass_yards_exp"), 0) +
      SCORING$pass_td * num_alias24(raw, c("pass_touchdown_exp", "passing_touchdowns_exp", "pass_tds_exp"), 0) +
      SCORING$interception * num_alias24(raw, c("interception_exp", "interceptions_exp", "passing_interceptions_exp"), 0) +
      SCORING$rush_yd * num_alias24(raw, c("rush_yards_gained_exp", "rushing_yards_exp", "rush_yards_exp"), 0) +
      SCORING$rush_td * num_alias24(raw, c("rush_touchdown_exp", "rushing_touchdowns_exp", "rush_tds_exp"), 0) +
      SCORING$reception * num_alias24(raw, c("receptions_exp", "rec_exp"), 0) +
      SCORING$rec_yd * num_alias24(raw, c("rec_yards_gained_exp", "receiving_yards_exp", "rec_yards_exp"), 0) +
      SCORING$rec_td * num_alias24(raw, c("rec_touchdown_exp", "receiving_touchdowns_exp", "rec_tds_exp"), 0)
  }
  actual <- num_alias24(raw, c("fantasy_points", "fantasy_points_ppr", "points"), NA_real_)
  if (!any(is.finite(actual))) {
    actual <- SCORING$pass_yd * num_alias24(raw, c("pass_yards_gained", "passing_yards", "pass_yards"), 0) +
      SCORING$pass_td * num_alias24(raw, c("pass_touchdown", "passing_touchdowns", "pass_tds"), 0) +
      SCORING$interception * num_alias24(raw, c("interception", "interceptions", "passing_interceptions"), 0) +
      SCORING$rush_yd * num_alias24(raw, c("rush_yards_gained", "rushing_yards", "rush_yards"), 0) +
      SCORING$rush_td * num_alias24(raw, c("rush_touchdown", "rushing_touchdowns", "rush_tds"), 0) +
      SCORING$reception * num_alias24(raw, c("receptions", "rec"), 0) +
      SCORING$rec_yd * num_alias24(raw, c("rec_yards_gained", "receiving_yards", "rec_yards"), 0) +
      SCORING$rec_td * num_alias24(raw, c("rec_touchdown", "receiving_touchdowns", "rec_tds"), 0)
  }
  out <- data.frame(
    season = as.integer(num_alias24(raw, c("season"), yr)),
    week = as.integer(num_alias24(raw, c("week"), NA_real_)),
    player_id = char_alias24(raw, c("player_id", "gsis_id")),
    xfp_24 = as.numeric(xfp),
    fpoe_24 = ifelse(is.finite(actual) & is.finite(xfp), actual - xfp, NA_real_),
    stringsAsFactors = FALSE) |>
    dplyr::filter(is.finite(week), nzchar(player_id)) |>
    dplyr::group_by(season, week, player_id) |>
    dplyr::summarise(xfp_24 = sum(xfp_24, na.rm = TRUE),
                     fpoe_24 = sum(fpoe_24, na.rm = TRUE), .groups = "drop")
  out
}

xfp_rows <- list()
if (isTRUE(MODEL24_USE_FFOPPORTUNITY)) {
  for (yr in max(2020, TRAIN_START):CURRENT_SEASON) {
    cp <- file.path("data/raw", paste0("ff_opportunity_2_4_", yr, ".csv"))
    cat("[2.4.3 xFP] ", yr, "...\n", sep = "")
    z <- data.frame()
    if (file.exists(cp) && yr <= TRAIN_END) {
      z <- tryCatch(readr::read_csv(cp, show_col_types = FALSE, progress = FALSE), error = function(e) data.frame())
    } else {
      z <- tryCatch({
        raw <- nflreadr::load_ff_opportunity(seasons = yr, stat_type = "weekly")
        zz <- build_xfp_weekly24(raw, yr)
        if (nrow(zz)) readr::write_csv(zz, cp)
        rm(raw); gc(); zz
      }, error = function(e) {
        warning("[2.4.3 xFP] ", yr, " unavailable: ", conditionMessage(e)); data.frame()
      })
    }
    if (nrow(z)) xfp_rows[[length(xfp_rows) + 1]] <- z
  }
}

xfp <- tryCatch(dplyr::bind_rows(xfp_rows), error = function(e) data.frame())
if (nrow(xfp)) {
  # Normalize older checkpoints before rolling calculations.
  for (nm in c("season", "week", "xfp_24", "fpoe_24")) if (!nm %in% names(xfp)) xfp[[nm]] <- NA_real_
  if (!"player_id" %in% names(xfp)) xfp$player_id <- ""
  xfp <- xfp |> dplyr::filter(is.finite(week), nzchar(player_id)) |>
    dplyr::arrange(player_id, season, week) |> dplyr::group_by(player_id, season) |>
    dplyr::mutate(
      xfp_roll3_24 = slider::slide_dbl(dplyr::lag(xfp_24), ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE), .before = 2, .complete = FALSE),
      xfp_roll5_24 = slider::slide_dbl(dplyr::lag(xfp_24), ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE), .before = 4, .complete = FALSE),
      fpoe_roll3_24 = slider::slide_dbl(dplyr::lag(fpoe_24), ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE), .before = 2, .complete = FALSE),
      fpoe_roll5_24 = slider::slide_dbl(dplyr::lag(fpoe_24), ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE), .before = 4, .complete = FALSE)) |>
    dplyr::ungroup()
  readr::write_csv(xfp, "data/processed/expected_opportunity_history_2_4.csv")
  cat("[2.4.3 xFP] Lagged expected-opportunity rows: ", nrow(xfp), "\n", sep = "")
} else {
  # readr cannot reliably serialize a zero-column data.frame. Always write a
  # typed zero-row schema so the next stages can safely discover optional xFP.
  readr::write_csv(xfp_schema24(), "data/processed/expected_opportunity_history_2_4.csv")
  cat("[2.4.3 xFP] Optional ffopportunity rows unavailable. Continuing with structured expected FPPG.\n")
}

cat("\n[2.4 DATA] COMPLETE.\n")
cat("  data/processed/player_talent_profiles_2_4.csv\n")
cat("  data/processed/talent_prior_oof_2_4.csv\n")
cat("  data/processed/defense_style_context_2_4.csv\n")
cat("  data/processed/expected_opportunity_history_2_4.csv\n")
cat("  output/rookie_talent_rankings_", CURRENT_SEASON, ".csv\n", sep = "")
