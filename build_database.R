# build_database.R
# Loads the ASA DataFest 2026 CSV files into a local DuckDB database (svh.duckdb)
# that every dashboard in this repo reads from.
#
# Usage: place the DataFest CSV files in this folder, then run this script once.
# The raw data is not included in this repository (see README).

library(duckdb)
library(DBI)

con <- dbConnect(duckdb(), "svh.duckdb")

# DuckDB reads CSVs directly, so each table is created straight from its file
tables <- c("patients", "encounters", "diagnosis", "departments",
            "providers", "social_determinants", "tigercensuscodes")

for (tbl in tables) {
  dbExecute(con, sprintf(
    "CREATE OR REPLACE TABLE %s AS SELECT * FROM read_csv_auto('%s.csv')", tbl, tbl
  ))
}

# Indexes on the keys the analyses filter and join on most
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_enc_patient ON encounters(PatientDurableKey)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_enc_diag    ON encounters(PrimaryDiagnosisKey)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_enc_dept    ON encounters(DepartmentKey)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sd_patient  ON social_determinants(PatientDurableKey)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sd_enc      ON social_determinants(EncounterKey)")

# Quick sanity check: how many encounters does a typical patient have?
print(dbGetQuery(con, "
  SELECT AVG(n) AS avg_encounters, MEDIAN(n) AS median_encounters
  FROM (
    SELECT COUNT(*) AS n
    FROM encounters
    GROUP BY PatientDurableKey
  )
"))

dbDisconnect(con, shutdown = TRUE)
