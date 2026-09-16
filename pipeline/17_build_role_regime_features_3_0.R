# ============================================================
# STEP 17 - BUILD 3.0 ROLE / REGIME / REDISTRIBUTION FEATURES
# ============================================================
source("config.R")
source("R/role_engine_30.R")
ensure_packages(c("dplyr", "readr"))

input_path <- "data/processed/weekly_model_table_2_3.csv"
if (!file.exists(input_path)) stop("Missing ", input_path, ". Run the weekly 2.3/2.4 data pipeline first.")

cat("[3.0 ROLE] Reading historical weekly table...\n")
weekly <- readr::read_csv(input_path, show_col_types = FALSE)
cat("[3.0 ROLE] Building historical teammate-absence pressure...\n")
injury_pressure <- fm30_build_injury_pressure(weekly)
if (nrow(injury_pressure)) readr::write_csv(injury_pressure, "data/processed/team_injury_pressure_3_0.csv")

cat("[3.0 ROLE] Adding regime, phase, and redistribution features...\n")
enriched <- fm30_enrich_weekly_table(weekly, injury_pressure)
readr::write_csv(enriched, ROLE30_OUTPUT_TABLE)
cat("[3.0 ROLE] Wrote ", ROLE30_OUTPUT_TABLE, " (", nrow(enriched), " rows).\n", sep = "")
