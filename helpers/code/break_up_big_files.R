library(duckdb)
con <- dbConnect(duckdb(), dbdir = "ucmr_shiny.db")

dir.create("export", showWarnings = FALSE)

tables <- c("tract_boundaries", 
             "wgt_tract_contam", "ucmr_og_tract_contam", "pws_wgt_tract_contam")

for (tbl in tables) {
  dbExecute(con, sprintf(
    "COPY %s TO 'export/%s.parquet' (FORMAT PARQUET, COMPRESSION ZSTD)", tbl, tbl
  ))
}

dbExecute(con, "
  COPY epa_water_boundaries TO 'export/epa_water_parts' 
  (FORMAT PARQUET, COMPRESSION ZSTD, PARTITION_BY (Primacy_Agency))
")

dbDisconnect(con)