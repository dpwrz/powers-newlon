# Regression test for 3.0 role-validation target-name masking.
# The historical bug occurred when tibble(target = target, actual = test[[target]])
# caused the newly-created `target` column to mask the scalar target name.
source("config.R")
source("R/role_engine_30.R")
ensure_packages(c("tibble"))

test <- tibble::tibble(
  season = c(2024L, 2024L),
  week = c(1L, 2L),
  player_id = c("a", "b"),
  player_display_name = c("A", "B"),
  position = c("QB", "QB"),
  pass_attempts = c(31, 28)
)
target <- "pass_attempts"
target_name <- as.character(target)[1]
actual_target <- fm30_num(test[[target_name]], NA_real_)
out <- tibble::tibble(target = target_name, actual = actual_target)
stopifnot(nrow(out) == 2L)
stopifnot(identical(out$target, rep("pass_attempts", 2L)))
stopifnot(identical(as.numeric(out$actual), c(31, 28)))
cat("[PASS] Role 3.0 target-name scope regression test.\n")
