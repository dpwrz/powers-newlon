# ============================================================
# STEP 8 - HISTORICAL PLAYER COMPS
# ============================================================

source("config.R")
ensure_packages(c("dplyr", "readr", "tidyr"))
current <- readr::read_csv(paste0("output/", CURRENT_SEASON, "_projections.csv"), show_col_types = FALSE)
hist <- readr::read_csv("data/processed/model_table.csv", show_col_types = FALSE)
comp_features <- intersect(COMP_FEATURES, intersect(names(current), names(hist)))

if (length(comp_features) < 6) stop("Not enough comp features are available.")

all_comps <- list()
for (pos in POSITIONS) {
  h <- hist |> dplyr::filter(position == pos, season <= TRAIN_END)
  c <- current |> dplyr::filter(position == pos)
  if (nrow(h) < 20 || nrow(c) == 0) next

  H <- prepare_feature_frame(h, comp_features)
  centers <- vapply(H, stats::median, numeric(1), na.rm = TRUE)
  scales <- vapply(H, stats::sd, numeric(1), na.rm = TRUE)
  scales[!is.finite(scales) | scales < 1e-6] <- 1
  Hs <- sweep(sweep(as.matrix(H), 2, centers, "-"), 2, scales, "/")

  for (i in seq_len(nrow(c))) {
    C <- prepare_feature_frame(c[i, , drop = FALSE], comp_features)
    Cs <- (as.numeric(C[1, ]) - centers) / scales
    dist <- sqrt(rowMeans((Hs - matrix(Cs, nrow = nrow(Hs), ncol = length(Cs), byrow = TRUE))^2))
    dist[h$player_id == c$player_id[i]] <- Inf
    ord <- head(order(dist), 3)

    similarity <- 100 * exp(-dist[ord])
    all_comps[[length(all_comps) + 1]] <- data.frame(
      player_id = c$player_id[i],
      player_display_name = c$player_display_name[i],
      position = pos,
      projected_fppg = c$projected_fppg[i],
      comp_rank = seq_along(ord),
      comp_player_id = h$player_id[ord],
      comp_player = h$player_display_name[ord],
      comp_season = h$season[ord],
      comp_next_season_fppg = h$target_fppg[ord],
      similarity_score = round(similarity, 1)
    )
  }
}

comps <- dplyr::bind_rows(all_comps)
readr::write_csv(comps, paste0("output/player_comps_", CURRENT_SEASON, ".csv"))
message("Player comps created: ", nrow(comps), " comp rows.")
