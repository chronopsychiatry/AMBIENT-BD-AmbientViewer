#' Plot an Actigram
#'
#' Generate an actigram from the Somnofy epoch data.
#' @param epochs The epochs data frame
#' @return A ggplot object representing the actigram
#' @export
plot_actigram <- function(epochs) {
  epochs <- epochs %>%
    dplyr::mutate(
      timestamp = lubridate::ymd_hms(timestamp),
      date = as.Date(timestamp),
      time = as.numeric(difftime(timestamp, as.Date(timestamp), units = "secs")) / 3600, # Time in hours
      sleep_value = ifelse(sleep_stage %in% c(2, 3, 4), signal_quality_mean, 0)
    ) %>%
    dplyr::mutate(
      day_label = paste0(date, "/", date + 1),
      time_48h = ifelse(time < 24, time, time - 24) + 24 * (time >= 24)
    )

  actigram_plot <- ggplot2::ggplot(epochs, ggplot2::aes(x = time_48h, y = sleep_value)) +
    ggplot2::geom_col(fill = "black", width = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "gray", linetype = "solid") +
    ggplot2::facet_wrap(~day_label, ncol = 1, strip.position = "left") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 48, by = 6),
      labels = c(0, 6, 12, 18, 0, 6, 12, 18, 0)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
    ggplot2::labs(
      x = "Time (hours)",
      y = NULL,
      title = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "gray", linetype = "dashed"), # Keep vertical grid lines
      panel.grid.major.y = ggplot2::element_blank(), # Remove all horizontal grid lines
      panel.grid.minor = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(color = "gray"),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, size = 8), # Day labels on the left
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 8),
      panel.spacing = ggplot2::unit(0.1, "lines"), # Reduce spacing between facets
      strip.placement = "outside", # Place facet labels outside the plot
      strip.background = ggplot2::element_blank(), # Remove background from facet labels
      plot.margin = ggplot2::margin(0, 0, 0, 0) # Remove plot margins
    )

  return(actigram_plot)
}
