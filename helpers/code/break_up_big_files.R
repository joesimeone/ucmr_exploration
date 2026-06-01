library(duckdb)
con <- dbConnect(duckdb(), dbdir = "ucmr_shiny.db")

dir.create("export", showWarnings = FALSE)

tables <- c("wgt_tract_contam", "ucmr_og_tract_contam", "pws_wgt_tract_contam")

for (tbl in tables) {
  dbExecute(con, sprintf(
    "COPY %s TO 'export/%s.parquet' (FORMAT PARQUET, COMPRESSION ZSTD)", tbl, tbl
  ))
}

dbExecute(con, "
  COPY (
    SELECT e.PWSID, e.geom_wkb, e.Detailed_Facility_Report, e.Model_Method, u.state_name
    FROM epa_water_boundaries e
    INNER JOIN (SELECT DISTINCT PWSID, state_name FROM ucmr_og_tract_contam) u
      ON e.PWSID = u.PWSID
  ) TO 'export/epa_water_parts'
  (FORMAT PARQUET, COMPRESSION ZSTD, PARTITION_BY (state_name))
")

dbExecute(
  con, "
  COPY (
    SELECT tb.*, wt.state_name
    FROM tract_boundaries tb
    LEFT JOIN (
      SELECT DISTINCT tract_geoid, state_name
      FROM wgt_tract_contam
    ) wt ON tb.GEOID = wt.tract_geoid
  ) TO 'export/tract_boundaries'
  (FORMAT PARQUET, COMPRESSION ZSTD, PARTITION_BY (state_name))"
)

dbDisconnect(con)

