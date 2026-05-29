# Connect to database. Once done stop doing that -------------------------
# con <- dbConnect(duckdb(), dbdir = "ucmr_shiny.db")
# onStop(function() dbDisconnect(con))
con <- dbConnect(duckdb())
dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

base_url <- "https://github.com/joesimeone/ucmr_exploration/releases/download/v1.0.0"
drop_box_url <- 'https://www.dropbox.com/scl/fi/tv9ohoo8zr6t7o0yslzew/epa_water_boundaries.parquet?rlkey=s7uvdb0k4pjksumm4x4zrpo61&st=6m94s33p&dl=1'

dbExecute(con, glue::glue("CREATE VIEW epa_water_boundaries AS SELECT * FROM '{drop_box_url}'")) 

# Everything else reads from GitHub
for (tbl in c("tract_boundaries", "wgt_tract_contam", "ucmr_og_tract_contam", "pws_wgt_tract_contam")) {
  dbExecute(con, sprintf("CREATE VIEW %s AS SELECT * FROM '%s/%s.parquet'", tbl, base_url, tbl))
}


# Prep Tract Table  ------------------------------------------------------
tract_sf_tbl <-
  tbl(con, 'tract_boundaries') |>
  left_join(
    tbl(con, 'wgt_tract_contam') |> distinct(tract_geoid, state_name),
    by = c('GEOID' = 'tract_geoid')
  ) |>
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
  inner_join(
    tbl(con, 'ucmr_og_tract_contam') |> distinct(PWSID, state_name),
    by = c('PWSID')
  ) |> 
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
