# Append-only weekly training observations; historical rows are never rewritten.
# Only final-game outcomes may enter training, and every observation retains its
# original pregame feature timestamp and source archive identifier.
fm31_append_training_history <- function(existing, incoming, key = c("season","week","player_id","position")) {
  stopifnot(is.data.frame(existing), is.data.frame(incoming))
  if (!all(key %in% names(incoming))) stop("Missing incoming identity columns")
  if (!all(key %in% names(existing)) && nrow(existing)) stop("Missing existing identity columns")
  if (!"game_final" %in% names(incoming)) stop("Missing final-game indicator")
  incoming <- incoming[!is.na(incoming$game_final) & incoming$game_final %in% TRUE, , drop = FALSE]
  if (!nrow(incoming)) return(existing)
  if (!"pregame_captured_at" %in% names(incoming)) stop("Missing pregame timestamp")
  if (!"kickoff_at" %in% names(incoming)) stop("Missing kickoff timestamp")
  cap <- as.POSIXct(incoming$pregame_captured_at, tz="UTC")
  kick <- as.POSIXct(incoming$kickoff_at, tz="UTC")
  if (any(is.na(cap) | is.na(kick) | cap >= kick)) stop("Invalid pregame evidence")
  k <- function(x) do.call(paste, c(lapply(x[key], as.character), sep="|"))
  if (anyDuplicated(k(incoming))) stop("Duplicate incoming observations")
  if (nrow(existing)) {
    new <- incoming[!k(incoming) %in% k(existing), , drop=FALSE]
  } else new <- incoming
  if (!nrow(new)) return(existing)
  cols <- union(names(existing), names(new))
  for (nm in setdiff(cols,names(existing))) existing[[nm]] <- rep(NA,nrow(existing))
  for (nm in setdiff(cols,names(new))) new[[nm]] <- rep(NA,nrow(new))
  rbind(existing[cols],new[cols])
}
