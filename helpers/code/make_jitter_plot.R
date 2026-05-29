make_jitter_ggiraph <- function(dat, switch_id = 'tract', bubbles) {
  if (switch_id == 'tract') {
    plot_data <-
      dat |>
      mutate(
        tooltip_text = paste0(
          "<b>Tract:</b> ",
          tract_geoid,
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

    plot_data
  } else {
    plot_data <-
      dat |>
      mutate(
        tooltip_text = paste0(
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

    plot_data
  }

  plot <-
    plot_data |>
    ggplot(aes(contaminant_label, value, fill = group_color)) +
    geom_jitter_interactive(
      aes(tooltip = tooltip_text, data_id = .data[[bubbles]]),
      shape = 21,
      color = 'lightgray'
    ) +
    scale_fill_identity() +
    coord_flip() +
    labs(y = "Concentration (µg/L)", x = unique(plot_data$contaminant_group))

  gir <-
    girafe(
      ggobj = plot,
      options = list(
        opts_tooltip(
          css = "background-color:white; padding:8px; border-radius:4px; font-size:12px;"
        ),
        opts_hover(css = "fill:#07294D; stroke:#07294D;"),
        opts_sizing(rescale = TRUE, width = 1)
      )
    )

  return(gir)
}
