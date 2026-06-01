# =============================================================================
#  db_tbl_prep.R  —  DuckDB ↔ Cloudflare R2
#
#  Replaces the GitHub release URL approach with R2.
#  All cleaning/mutation logic is preserved exactly as-is.
#
#  Bucket layout (flat — no partitioning needed given your current structure):
#    s3://your-bucket/
#      tract_boundaries.parquet
#      wgt_tract_contam.parquet
#      ucmr_og_tract_contam.parquet
#      pws_wgt_tract_contam.parquet
#      epa_water_boundaries.parquet   ← was local before, now also on R2
#
#  Credentials in .Renviron:
#    R2_ACCESS_KEY_ID=...
#    R2_SECRET_ACCESS_KEY=...
#    R2_ACCOUNT_ID=...
#    R2_BUCKET=your-bucket-name
# =============================================================================
 
 
# Connect & configure --------------------------------------------------------
 
con <- dbConnect(duckdb())
onStop(function() dbDisconnect(con, shutdown = TRUE))
 
dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
 
r2_account <- Sys.getenv("R2_ACCOUNT_ID")
r2_key     <- Sys.getenv("R2_ACCESS_KEY_ID_RR")
r2_secret  <- Sys.getenv("R2_SECRET_ACCESS_KEY_rr")
r2_bucket  <- Sys.getenv("R2_BUCKET")
 
dbExecute(con, sprintf("
  SET s3_endpoint          = '%s.r2.cloudflarestorage.com';
  SET s3_access_key_id     = '%s';
  SET s3_secret_access_key = '%s';
  SET s3_region            = 'auto';
  SET s3_url_style         = 'path';
", r2_account, r2_key, r2_secret))
 
base_url <- sprintf("s3://%s", r2_bucket)



# Create views from bucket source ----------------------------------------
# Flat files
for (tbl in c("wgt_tract_contam", "ucmr_og_tract_contam", "pws_wgt_tract_contam")) {
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM '%s/stats/%s.parquet'",
    tbl, base_url, tbl
  ))
}

# Hive partitioned
dbExecute(con, sprintf(
  "CREATE VIEW tract_boundaries AS SELECT * FROM read_parquet('%s/tract_boundaries/**/*.parquet', hive_partitioning = true)",
  base_url
))

dbExecute(con, sprintf(
  "CREATE VIEW epa_water_boundaries AS SELECT * FROM read_parquet('%s/epa_water_parts/**/*.parquet', hive_partitioning = true)",
  base_url
))
 


# Prep Tract Table  ------------------------------------------------------
tract_sf_tbl <-
  tbl(con, 'tract_boundaries') |>
  mutate(
    crs_info = case_when(
      st_fips == '02' ~ 3338,
      st_fips == '15' ~ 3759,
      TRUE ~ 5070
    )
  )

tract_contam_tbl <-
  tbl(con, 'wgt_tract_contam') |>
  mutate(
    contaminant_label = case_when(
      # PFAS
      contaminant == "pfoa35" ~ "PFOA",
      contaminant == "pfos35" ~ "PFOS",
      contaminant == "pfna35" ~ "PFNA",
      contaminant == "pfhxs35" ~ "PFHxS",
      contaminant == "pfbs35" ~ "PFBS",
      contaminant == "pfhpa35" ~ "PFHpA",
      contaminant == "pfba" ~ "PFBA",
      contaminant == "pfhxa" ~ "PFHxA",
      contaminant == "pfpea" ~ "PFPEA",
      contaminant == "fts62" ~ "FTS 6:2",
      # PFAS Summary
      contaminant == "pfasany3" ~ "Any PFAS (UCMR3)",
      contaminant == "pfasany5" ~ "Any PFAS (UCMR5, 6 compounds)",
      contaminant == "pfasany5t" ~ "Any PFAS (UCMR5, all)",
      contaminant == "pfospfoa3" ~ "PFOS + PFOA Sum (UCMR3)",
      contaminant == "pfospfoa5" ~ "PFOS + PFOA Sum (UCMR5)",
      # Metals
      contaminant == "chromium" ~ "Chromium (Total)",
      contaminant == "chromium6" ~ "Chromium-6 (Hexavalent)",
      contaminant == "cobalt" ~ "Cobalt",
      contaminant == "molybdenum" ~ "Molybdenum",
      contaminant == "strontium" ~ "Strontium",
      contaminant == "vanadium" ~ "Vanadium",
      contaminant == "lithium" ~ "Lithium",
      contaminant == "germanium" ~ "Germanium",
      contaminant == "manganese" ~ "Manganese",
      # Disinfection Byproducts
      contaminant == "chlorate" ~ "Chlorate",
      contaminant == "haa5" ~ "HAA5",
      contaminant == "haa6br" ~ "HAA6Br",
      contaminant == "haa9" ~ "HAA9",
      # VOCs / Solvents
      contaminant == "dichloroethane11" ~ "1,1-Dichloroethane",
      contaminant == "dioxane14" ~ "1,4-Dioxane",
      contaminant == "HCFC22" ~ "HCFC-22",
      contaminant == "Halon1011" ~ "Halon 1011",
      # Other
      contaminant == "dcpa" ~ "DCPA (Dacthal)",
      contaminant == "perchlorate" ~ "Perchlorate",
      contaminant == "butanol1" ~ "1-Butanol",
      TRUE ~ contaminant
    ),
    group_color = case_when(
      contaminant_group == "Metals" ~ "#54278F",
      contaminant_group == "Disinfection Byproducts" ~ "#A50F15",
      contaminant_group == "VOCs / Solvents" ~ "#08519C",
      contaminant_group == "PFAS (Simplified)" ~ "#006D2C",
      contaminant_group == 'Pesticides, Perchlorate, Other' ~ "#A63603",
      TRUE ~ "#636363"
    ),
    ucmr_round = case_when(
      # UCMR 1
      contaminant %in% c("dcpa", "perchlorate") ~ "UCMR 1",
      # UCMR 3
      contaminant %in%
        c(
          "dichloroethane11",
          "dioxane14",
          "HCFC22",
          "Halon1011",
          "pfbs3",
          "pfhpa3",
          "pfhxs3",
          "pfna3",
          "pfoa3",
          "pfos3",
          "pfasany3",
          "pfospfoa3",
          "chlorate",
          "chromium",
          "chromium6",
          "cobalt",
          "molybdenum",
          "strontium",
          "vanadium"
        ) ~ "UCMR 3",
      # UCMR 4
      contaminant %in%
        c(
          "haa5",
          "haa6br",
          "haa9",
          "butanol1",
          "germanium",
          "manganese"
        ) ~ "UCMR 4",
      # UCMR 5
      contaminant %in%
        c(
          "fts62",
          "pfba",
          "pfbs5",
          "pfhpa5",
          "pfhxa",
          "pfhxs5",
          "pfna5",
          "pfoa5",
          "pfos5",
          "pfpea",
          "pfasany5",
          "pfasany5t",
          "pfospfoa5",
          "lithium"
        ) ~ "UCMR 5",
      # Combined 3 & 5
      contaminant %in%
        c(
          "pfoa35",
          "pfos35",
          "pfna35",
          "pfhxs35",
          "pfbs35",
          "pfhpa35"
        ) ~ "UCMR 3 & 5",
      TRUE ~ "Unknown"
    )
  )

tract_contam_tbl <-
  tract_contam_tbl |>
  mutate(
    grp_palettes = case_when(
      contaminant_group == 'Metals' ~ "Purples",
      contaminant_group == 'Disinfection Byproducts' ~ "Reds",
      contaminant_group == 'VOCs / Solvents' ~ "Blues",
      contaminant_group == 'PFAS (Simplified)' ~ "Greens",
      contaminant_group == 'Pesticides, Perchlorate, Other' ~ "Oranges",
      TRUE ~ 'Greys'
    )
  )


# Prep PWS Table ---------------------------------------------------------



epa_water_sf <-
  tbl(con, 'epa_water_boundaries') |>
  mutate(
    crs_info = case_when(
      state_name == 'Alaska' ~ 3338,
      state_name == 'Hawaii' ~ 3759,
      TRUE ~ 5070
    )
  )


pws_contam_tbl <-
  tbl(con, 'ucmr_og_tract_contam') |>
   inner_join(
    tbl(con, 'epa_water_boundaries') |> distinct(PWSID, Detailed_Facility_Report, Model_Method),
    by = c('PWSID')
  ) |> 
  mutate(
    contaminant_label = case_when(
      # PFAS
      contaminant == "pfoa35" ~ "PFOA",
      contaminant == "pfos35" ~ "PFOS",
      contaminant == "pfna35" ~ "PFNA",
      contaminant == "pfhxs35" ~ "PFHxS",
      contaminant == "pfbs35" ~ "PFBS",
      contaminant == "pfhpa35" ~ "PFHpA",
      contaminant == "pfba" ~ "PFBA",
      contaminant == "pfhxa" ~ "PFHxA",
      contaminant == "pfpea" ~ "PFPEA",
      contaminant == "fts62" ~ "FTS 6:2",
      # PFAS Summary
      contaminant == "pfasany3" ~ "Any PFAS (UCMR3)",
      contaminant == "pfasany5" ~ "Any PFAS (UCMR5, 6 compounds)",
      contaminant == "pfasany5t" ~ "Any PFAS (UCMR5, all)",
      contaminant == "pfospfoa3" ~ "PFOS + PFOA Sum (UCMR3)",
      contaminant == "pfospfoa5" ~ "PFOS + PFOA Sum (UCMR5)",
      # Metals
      contaminant == "chromium" ~ "Chromium (Total)",
      contaminant == "chromium6" ~ "Chromium-6 (Hexavalent)",
      contaminant == "cobalt" ~ "Cobalt",
      contaminant == "molybdenum" ~ "Molybdenum",
      contaminant == "strontium" ~ "Strontium",
      contaminant == "vanadium" ~ "Vanadium",
      contaminant == "lithium" ~ "Lithium",
      contaminant == "germanium" ~ "Germanium",
      contaminant == "manganese" ~ "Manganese",
      # Disinfection Byproducts
      contaminant == "chlorate" ~ "Chlorate",
      contaminant == "haa5" ~ "HAA5",
      contaminant == "haa6br" ~ "HAA6Br",
      contaminant == "haa9" ~ "HAA9",
      # VOCs / Solvents
      contaminant == "dichloroethane11" ~ "1,1-Dichloroethane",
      contaminant == "dioxane14" ~ "1,4-Dioxane",
      contaminant == "HCFC22" ~ "HCFC-22",
      contaminant == "Halon1011" ~ "Halon 1011",
      # Other
      contaminant == "dcpa" ~ "DCPA (Dacthal)",
      contaminant == "perchlorate" ~ "Perchlorate",
      contaminant == "butanol1" ~ "1-Butanol",
      TRUE ~ contaminant
    ),
    group_color = case_when(
      contaminant_group == "Metals" ~ "#54278F",
      contaminant_group == "Disinfection Byproducts" ~ "#A50F15",
      contaminant_group == "VOCs / Solvents" ~ "#08519C",
      contaminant_group == "PFAS (Simplified)" ~ "#006D2C",
      contaminant_group == 'Pesticides, Perchlorate, Other' ~ "#A63603",
      TRUE ~ "#636363"
    ),
    ucmr_round = case_when(
      # UCMR 1
      contaminant %in% c("dcpa", "perchlorate") ~ "UCMR 1",
      # UCMR 3
      contaminant %in%
        c(
          "dichloroethane11",
          "dioxane14",
          "HCFC22",
          "Halon1011",
          "pfbs3",
          "pfhpa3",
          "pfhxs3",
          "pfna3",
          "pfoa3",
          "pfos3",
          "pfasany3",
          "pfospfoa3",
          "chlorate",
          "chromium",
          "chromium6",
          "cobalt",
          "molybdenum",
          "strontium",
          "vanadium"
        ) ~ "UCMR 3",
      # UCMR 4
      contaminant %in%
        c(
          "haa5",
          "haa6br",
          "haa9",
          "butanol1",
          "germanium",
          "manganese"
        ) ~ "UCMR 4",
      # UCMR 5
      contaminant %in%
        c(
          "fts62",
          "pfba",
          "pfbs5",
          "pfhpa5",
          "pfhxa",
          "pfhxs5",
          "pfna5",
          "pfoa5",
          "pfos5",
          "pfpea",
          "pfasany5",
          "pfasany5t",
          "pfospfoa5",
          "lithium"
        ) ~ "UCMR 5",
      # Combined 3 & 5
      contaminant %in%
        c(
          "pfoa35",
          "pfos35",
          "pfna35",
          "pfhxs35",
          "pfbs35",
          "pfhpa35"
        ) ~ "UCMR 3 & 5",
      TRUE ~ "Unknown"
    )
  )

pws_contam_tbl <-
  pws_contam_tbl |>
  mutate(
    grp_palettes = case_when(
      contaminant_group == 'Metals' ~ "Purples",
      contaminant_group == 'Disinfection Byproducts' ~ "Reds",
      contaminant_group == 'VOCs / Solvents' ~ "Blues",
      contaminant_group == 'PFAS (Simplified)' ~ "Greens",
      contaminant_group == 'Pesticides, Perchlorate, Other' ~ "Oranges",
      TRUE ~ 'Greys'
    )
  )
