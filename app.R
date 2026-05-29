library(shiny)
library(bslib)
library(duckdb)
library(tidyverse)
library(mapgl)
library(ggiraph)

# Connections & Pre-reqs -------------------------------------------------

source('helpers/code/db_tbl_prep.R')
source('helpers/code/make_chloro_map.R')
source('helpers/code/make_jitter_plot.R')

app_blue <- "#07294D"
app_blue_light <- "#e8edf3"
app_blue_mid <- "#c5d0de"

app_font <- font_google("Inter", wght = "300..600")

app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#2c2c2c",
  primary = app_blue,
  secondary = "#6c7a8d",
  base_font = app_font,
  heading_font = app_font,
  "navbar-bg" = app_blue_light,
  "card-border-color" = app_blue_mid,
  "sidebar-bg" = app_blue_light,
  "sidebar-fg" = app_blue,
  "nav-underline-link-active-color" = app_blue,
  font_scale = 0.95
)

theme_set(
  theme_minimal(base_size = 14, base_family = "Inter") +
    theme(
      text = element_text(color = "#2c2c2c"),
      plot.title = element_text(color = app_blue, size = 13),
      axis.title = element_text(color = app_blue, size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e0e4ea")
    )
)
## =======================================================================
# Make searchable sidebar Options  ----
## =======================================================================
states <- tract_contam_tbl |>
  distinct(state_name) |>
  collect() |>
  pull()

contams <- tract_contam_tbl |>
  distinct(contaminant_group, contaminant_label) |>
  collect()

# Only if you have a separate ID column
# contam_choices <- lapply(
#   split(contams, contams$contaminant_group),
#   function(grp) setNames(grp$contaminant, grp$contaminant_label)
# )

contam_choices <- contams |>
  split(contams$contaminant_group) |>
  lapply(function(x) setNames(x$contaminant_label, x$contaminant_label))

state_search_bar <-
  selectizeInput(
    inputId = 'state_select',
    label = 'Pick a state...',
    choices = states,
    selected = "Pennsylvania"
  )

contam_search_bar <-
  selectizeInput(
    inputId = 'contam_select',
    label = 'Pick a Contaminant...',
    choices = contam_choices,
    selected = "Cobalt"
  )

stat_buttons <-
  radioButtons(
    inputId = 'stat_choice',
    label = 'Median or Maximum?',
    choices = list('Median' = 'median', 'Maximum' = 'max'),
    inline = TRUE
  )

## =======================================================================
# Make Cards  ----
## =======================================================================

pws_cards <-
  list(
    pws_map_card = card(
      id = 'pws_map_card',
      card_header('Original Contaminant Estimates | Modeled Public Water Systems (v3)'),
      full_screen = TRUE,
      maplibreOutput("pws_map"),
    ),
    pws_jitter_card = card(
      id = 'pws_jitter_card',
      max_height = 500,
      card_header('Original Contaminant Estimates | By Water System & Classification'),
      girafeOutput('pws_jitter_plot')
    ),
    pws_hist_card = card(
      id = 'pws_jitter',
      max_height = 300,
      card_header('Original Contaminant Estimates |Indvidual Contaminants'),
      plotOutput('pws_hist'),
    )
  )

tract_cards <-
  list(
    tract_map_card = card(
      id = 'tract_map_card',
      card_header('Weighted Contaminant Estimates | 2010 Census Tracts'),
      full_screen = TRUE,
      maplibreOutput("tract_map"),
      tags$em(
        'Blank space on maps represent areas with no intersecting public water system'
      )
    ),
    tract_jitter_card = card(
      id = 'tract_jitter_card',
      max_height = 500,
      card_header('Weighted Contaminant Estimates | By Tract & Classification'),
      girafeOutput('tract_jitter_plot')
    ),
    tract_hist_card = card(
      id = 'tract_jitter',
      max_height = 300,
      card_header('Weighted Contaminant Estimates |Indvidual Contaminants'),
      plotOutput('tract_hist'),
    )
  )


ui <- page_sidebar(
  title = "UCMR Contaminant Explorer",
  theme = app_theme,
  fillable = TRUE,
  sidebar = sidebar(
    state_search_bar,
    contam_search_bar,
    stat_buttons
  ),

  navset_underline(
    nav_panel(
      title = 'Methods',
      layout_columns(
        col_widths = c(5, 7),
        fill = TRUE,
        gap = "24px",
        card(
          card_body(
            tags$h4("About This Dashboard", style = "color: #07294D;"),
            tags$p(
              "This dashboard explores tract-level exposure estimates for unregulated",
              "contaminants monitored under the EPA's",
              tags$a(
                "Unregulated Contaminant Monitoring Rule (UCMR)",
                href = "https://www.epa.gov/dwucmr",
                target = "_blank"
              ),
              "program. Because contaminant measurements are reported at the public",
              "water system (PWS) level and health outcomes in the MDAC cohort are",
              "identified at the census tract level, we use a population-weighted",
              "areal interpolation to translate water-system concentrations into",
              "tract-level exposure estimates."
            ),
            tags$h4("Tract-Level Estimates", style = "color: #07294D;"),
            tags$p(
              "The Tract Level tab displays reweighted contaminant values for every",
              "2010 census tract in a selected state. Each tract's estimate reflects",
              "the share of its population actually served by each intersecting water",
              "system rather than a simple geographic overlay. Ultimately, tracts will only be included",
              "when more than half their population falls within a modeled",
              "service area, reducing noise from areas primarily on private wells. In this app, however,
              we leave all estimates in regardless of population coverage."
            ),
            tags$h4("Dashboard Navigation", style = "color: #07294D;"),
            tags$p(
              "Use the sidebar to select a state, contaminant, and summary statistic",
              "(median or maximum). The",
              tags$strong("Public Water System"),
              "tab shows the original, unweighted measurements reported by each water",
              "system on a choropleth map alongside jitter and histogram plots. The",
              tags$strong("Tract Level UCMR Contaminants"),
              "tab shows the reweighted estimates derived from the population-weighted",
              "interpolation, letting you compare the derived tract-level values back",
              "to the raw system-level measurements."
            )
          )
        ),
        card(
          card_body(
            tags$div(
              class = "mermaid",
              "flowchart TD
    A[\"Census block groups (2010)\"] --> S1
    B[\"EPA water system boundaries v2\"] --> S1

    S1[\"1. Intersect & normalize areal weights
    w = overlap_area / block_area
    Normalize so weights sum to at most 1
    bg_wgt_pop = pop × w*\"]

    S1 --> S2[\"2. Scale to tract × PWS
    pop_bg_pws = Σ bg_wgt_pop\"]

    S2 --> S3

    C[\"Census tract total population\"] --> S3

    S3[\"3. Sum all people in tract
    served by any water system
    ct_pop_pws = Σ pop_bg_pws\"]

    S3 --> S4[\"4. Create population weight
    w = pop_bg_pws / ct_pop_pws\"]

    S4 --> S5[\"5. Weight contaminants
    c_weighted = c × w\"]

    S5 --> S6[\"6. Summarize to tract & flag coverage
    included = ct_pop_pws / tract_pop > 0.5\"]

    S6 --> OUT[(\"Final tract-level dataset\")]

    style A fill:#e8edf3,stroke:#07294D,color:#07294D
    style B fill:#e8edf3,stroke:#07294D,color:#07294D
    style C fill:#e8edf3,stroke:#07294D,color:#07294D
    style OUT fill:#07294D,stroke:#07294D,color:#fff"
            ),
            tags$script(
              src = "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.min.js"
            ),
            tags$script(HTML("mermaid.initialize({startOnLoad: true, theme: 'base', themeVariables: {primaryColor: '#e8edf3', primaryBorderColor: '#07294D', primaryTextColor: '#07294D', lineColor: '#6c7a8d'}});"))
          )
        )
      )
    ),
    nav_panel(
      title = 'Public Water System',
      layout_columns(
        col_widths = c(6, 6),
        fill = TRUE,
        gap = "8px",
        pws_cards$pws_map_card,
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          gap = "8px",
          pws_cards$pws_jitter_card,
          pws_cards$pws_hist_card
        )
      )
    ),
    nav_panel(
      title = 'Tract Level UCMR Contaminants',
      layout_columns(
        col_widths = c(6, 6),
        fill = TRUE,
        gap = "8px",
        tract_cards$tract_map_card,
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          gap = "8px",
          tract_cards$tract_jitter_card,
          tract_cards$tract_hist_card
        )
      )
    )
  )
)


server <- function(input, output, session) {
  #' Tract level reactive objects
  state_geom <- reactiveVal() # 1. Geometry — same as now, driven by state
  tract_group_data <- reactiveVal() # 2. Group-level data — feeds the jitter plot. All contaminants in the same group, for the selected state + stat
  tract_selected_data <- reactiveVal() # 3. Single-contaminant data joined to geometry — feeds the map + histogram

  #' Water System (PWS reactiv objects)
  pws_geom <- reactiveVal()
  pws_group_data <- reactiveVal()
  pws_selected <- reactiveVal()


# Event 1 | Touch nothing once & until user asks state to change  --------
  observeEvent(input$state_select, {
    selected_state <- input$state_select #' Masks input variable to satisfy {dbplyr} gods

    # Initial State objects pulled in on query -----
    #' Tract Object
    tract_raw <- tract_sf_tbl |>
      filter(state_name == selected_state) |>
      select(GEOID, geom_wkb, crs_info) |>
      collect()

    #' PWS Object
    epa_water_raw <- epa_water_sf |>
      filter(state_name == selected_state) |>
      select(PWSID, geom_wkb, crs_info) |>
      collect()

    # Extract CRS Info from reactive object
    tract_crs <- tract_raw$crs_info[1]
    pws_crs <- epa_water_raw$crs_info[1]

    # Make {sf} objects for mapping
    tract_sf <- tract_raw |>
      mutate(
        geom = sf::st_as_sfc(
          structure(geom_wkb, class = "WKB"),
          crs = tract_crs
        )
      ) |>
      sf::st_as_sf()

    pws_sf <- epa_water_raw |>
      mutate(
        geom = sf::st_as_sfc(
          structure(geom_wkb, class = "WKB"),
          crs = tract_crs
        )
      ) |>
      sf::st_as_sf()

    #' Wrap logic in reactable blankets :)
    state_geom(tract_sf)
    pws_geom(pws_sf)
  })


# Event 2 | If state any of state, contaminant or stat_choice change, repopulate graphs ----
#' But keep as small a footprint as possible 
  observeEvent(c(input$state_select, input$contam_select, input$stat_choice), {
    req(state_geom())

    selected_state <- input$state_select
    selected_contam <- input$contam_select
    selected_stat <- input$stat_choice

    # Look up which group the selected contaminant belongs to
    tract_contam_group <- tract_contam_tbl |>
      filter(contaminant_label == selected_contam) |>
      distinct(contaminant_group) |>
      collect() |>
      pull()

    pws_contam_group <- pws_contam_tbl |>
      filter(contaminant_label == selected_contam) |>
      distinct(contaminant_group) |>
      collect() |>
      pull()

    # JITTER data: all contaminants in that group
    tract_grp <- tract_contam_tbl |>
      filter(
        state_name == selected_state,
        contaminant_group == tract_contam_group,
        stat_clean == selected_stat
      ) |>
      collect()

    pws_grp <- pws_contam_tbl |>
      filter(
        state_name == selected_state,
        contaminant_group == pws_contam_group,
        stat_clean == selected_stat
      ) |>
      collect()

    tract_group_data(tract_grp)
    pws_group_data(pws_grp)

    # MAP + HISTOGRAM data: single contaminant, joined to geometry
    tract_single <- tract_grp |> filter(contaminant_label == selected_contam)
    pws_single <- pws_grp |> filter(contaminant_label == selected_contam)

    tract_selected_data(
      state_geom() |> left_join(tract_single, by = c("GEOID" = "tract_geoid"))
    )

    pws_selected(
      pws_geom() |> left_join(pws_single, by = c("PWSID"))
    )
  })


# Render Outpus ----------------------------------------------------------

#' Maps 
  output$tract_map <- renderMaplibre({
    req(tract_selected_data())
    make_chloro_map(tract_selected_data(), switch_id = 'tract')
  })

  output$pws_map <- renderMaplibre({
    req(pws_selected())
    make_chloro_map(pws_selected(), 
                switch_id = 'pws', 
                bounds =  state_geom())
  })

#' Jitter Plots 
  output$tract_jitter_plot <- renderGirafe({
    req(tract_group_data())
    make_jitter_ggiraph(tract_group_data(), switch_id = 'tract', bubbles = 'tract_geoid')
  })

    output$pws_jitter_plot <- renderGirafe({
    req(pws_group_data())
    make_jitter_ggiraph(pws_group_data(), switch_id = 'pws', bubbles = 'PWSID')
  })

  output$tract_hist <- renderPlot({
    req(tract_selected_data())
    # histogram of the single contaminant's values across tracts
    tract_selected_data() |>
      sf::st_drop_geometry() |>
      ggplot(aes(value, fill = group_color)) +
      geom_histogram(color = app_blue, linewidth = 0.3) +
      scale_fill_identity()
  })

   output$pws_hist <- renderPlot({
    req(pws_selected())
    # histogram of the single contaminant's values across tracts
    pws_selected() |>
      sf::st_drop_geometry() |>
      ggplot(aes(value, fill = group_color)) +
      geom_histogram(color = app_blue, linewidth = 0.3) +
      scale_fill_identity()
  })
}

shinyApp(ui, server)
