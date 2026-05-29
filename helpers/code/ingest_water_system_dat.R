library(DBI)
library(duckdb)


# Connections & Pre-reqs -------------------------------------------------
# Connect to the target database
con <- dbConnect(duckdb(), dbdir = "ucmr_shiny.db")

dbExecute(con, "INSTALL httpfs;;")
dbExecute(con, "LOAD httpfs;;")

dbExecute(con, "INSTALL spatial;;")
dbExecute(con, "LOAD httpfs;;")

# Attach the source database
dbExecute(
  con,
  "ATTACH 'C:/git/epa_water_stuff/data/tract_water_system_db.duckdb' AS source_db (READ_ONLY)"
)

dbListTables(con)


# Copy tables from main epa water project to shiny database --------------
src_tbls <-
  c(
    'tract10_5070',
    'hi_tract10_3759',
    'ak_tract10_3338',
    'epa_water_v3_5070',
    'hi_epa_water_v3_3759',
    'ak_epa_water_v3_3338'
  )

for (tbl in src_tbls) {
  dbExecute(
    con,
    sprintf("CREATE TABLE %s AS SELECT * FROM source_db.%s", tbl, tbl)
  )
}

dbExecute(con, "DETACH source_db")

# Unify tables -----------------------------------------------------------
dbExecute(
  con,
  "
  CREATE TABLE epa_water_boundaries AS
  SELECT * FROM epa_water_v3_5070
  UNION ALL
  SELECT * FROM hi_epa_water_v3_3759
  UNION ALL
  SELECT * FROM ak_epa_water_v3_3338
"
)

# Create unified tract boundaries table
dbExecute(
  con,
  "
  CREATE TABLE tract_boundaries AS
  SELECT * FROM tract10_5070
  UNION ALL
  SELECT * FROM hi_tract10_3759
  UNION ALL
  SELECT * FROM ak_tract10_3338
"
)

# Drop the intermediate tables
intermediate <- c(
  "tract10_5070",
  "hi_tract10_3759",
  "ak_tract10_3338",
  "epa_water_v3_5070",
  "hi_epa_water_v3_3759",
  "ak_epa_water_v3_3338"
)

for (tbl in intermediate) {
  dbExecute(con, sprintf("DROP TABLE %s", tbl))
}


# Write weighted results data to separate table in the database ----------

parq_paths <-
  list.files(
    'C:/git/epa_water_stuff/data/results',
    pattern = '.parquet',
    full.names = TRUE
  )

tbl_names <-
  c('ucmr_og_tract_contam', 'pws_wgt_tract_contam','wgt_tract_contam')


purrr::walk2(
  tbl_names,
  parq_paths,
  ~ dbExecute(con, glue::glue("create table {.x} AS SELECT * FROM '{.y}'"))
)

dbListTables(con)
dbDisconnect(con)


dbDisconnect(con)



