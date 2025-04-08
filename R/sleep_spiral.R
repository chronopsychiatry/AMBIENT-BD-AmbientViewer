#' Plot Sleep Spiral
#'
#' @param epochs The epochs dataframe
#' @returns A ggplot object showing the sleep spiral
#' @export
plot_sleep_spiral <- function(epochs) {
  sleep_stage_colors <- c(
    "1" = "orange",  # Awake
    "2" = "#D3B9E6", # Light sleep (light purple)
    "3" = "#6A3A9A", # Deep sleep (dark purple)
    "4" = "#A074C4", # REM sleep (medium purple)
    "5" = "orange",  # No presence
    "NA" = "orange"  # Absence of data
  )

  reference_time <- lubridate::floor_date(min(epochs$timestamp, na.rm = TRUE), unit = "day")

  epochs <- epochs %>%
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      sleep_stage = as.character(sleep_stage)  # Convert sleep_stage to character for mapping
    ) %>%
    tidyr::complete(
      timestamp = seq(min(timestamp), max(timestamp), by = "5 min"),  # Fill gaps with 5-minute intervals
      fill = list(sleep_stage = "NA")  # Mark missing data as "NA"
    ) %>%
    dplyr::mutate(
      time_in_days = as.numeric(difftime(timestamp, reference_time, units = "days")),
      time_in_min = as.numeric(difftime(timestamp, reference_time, units = "mins")),
      angle = (time_in_days %% 1) * 2 * pi,
      radius = time_in_min
    )

  p <- ggplot2::ggplot(epochs, ggplot2::aes(x = angle, y = radius, color = sleep_stage)) +
    ggplot2::geom_point(size = 1, shape = 16) +
    ggplot2::scale_color_manual(values = sleep_stage_colors) +
    ggplot2::coord_polar(theta = "x") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 2 * pi, length.out = 25)[-25],  # Hourly labels (0 to 24)
      labels = 0:23
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      color = NULL,
      title = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 12),
      legend.position = "none"
    )

  return(p)
}