make_chloro_map <- function(dat, switch_id = 'tract', bounds = NULL) {
  # Step 1: data prep
  # hmm_colored <- hmm |>
  #   mutate(
  #     fill_color = scales::col_numeric(
  #       palette = unique(na.omit(grp_palettes)),
  #       domain  = range(value, na.rm = TRUE)
  #     )(value)
  #   )
  # Pull out the palette function so we can reuse it for the legend
  pal_fn <- scales::col_numeric(
    palette = unique(na.omit(dat$grp_palettes)),
    domain = range(dat$value, na.rm = TRUE)
  )

  # Build legend break points and their corresponding colors
  val_range <- range(dat$value, na.rm = TRUE)
  legend_breaks <- seq(val_range[1], val_range[2], length.out = 4)

  if (switch_id == 'tract') {
    dat_fin <-
      dat |>
      mutate(
        fill_color = pal_fn(value),
        map_tooltip_text = paste0(
          "<b>Tract:</b> ",
          GEOID,
          "<br>",
          "<b>UCMR Round(s):</b> ",
          ucmr_round,
          "<br>",
          "<b>Value:</b> ",
          round(value, 4),
          " µg/L<br>",
          "<b>Pop:</b> ",
          scales::comma(tract_total_pop),
          "<br>",
          "<b>PWS Coverage:</b> ",
          scales::percent(pws_coverage_proportion, accuracy = 0.1)
        )
      )
    dat_fin
  } else {
    dat_fin <-
      dat |>
      mutate(
    fill_color = pal_fn(value),
        map_tooltip_text = paste0(
          "<b>System Name:</b> ",
          PWS_Name,
          "<br>",
          "<b>System ID:</b> ",
          PWSID,
          "<br>",
          "<b>UCMR Round(s):</b> ",
          ucmr_round,
          "<br>",
          "<b>Value:</b> ",
          round(value, 4),
          " µg/L<br>",
          "<b>Facility Report:</b> ",
          Detailed_Facility_Report,
          " µg/L<br>",
          "<b>Model Method:</b> ",
          Model_Method,
          " µg/L<br>"
        )
      )
    dat_fin
  }

  map_to_check <-
    maplibre(
      style = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json"
    ) |>
    #fit_bounds(dat_fin) |>
    fit_bounds(bounds %||% dat_fin) |> 
    add_fill_layer(
      id = "tract_fill",
      source = dat_fin,
      fill_color = get_column("fill_color"),
      tooltip = 'map_tooltip_text',
      hover_options = list(
        fill_color = '#4a7ab5',
        fill_opacity = 1
      ),
      popup = 'map_tooltip_text'
    ) |>
    add_line_layer(
      id = "block_outline",
      source = dat_fin,
      line_color = "lightgray",
      line_width = 1.2,
      line_opacity = 0.75,
      line_dasharray = c(1.25, 1.25)
    ) |>
    add_legend(
      legend_title = "Measure (µg/L)",
      values = round(legend_breaks, 3),
      colors = pal_fn(legend_breaks),
      type = "continuous",
      position = "bottom-left"
    )

  return(map_to_check)
}
