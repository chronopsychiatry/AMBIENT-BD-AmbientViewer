#' Plot Sleep Spiral
#'
#' @param epochs The epochs dataframe
#' @param col_names A list to override default column names. This function uses columns:
#' - `timestamp`
#' - `sleep_stage`
#' @param color_by The variable to color the spiral by. Can be "default" or any other column name in the epochs dataframe.
#' @returns A ggplot object showing the sleep spiral
#' @importFrom rlang .data
#' @export
#' @family plot epochs
plot_sleep_spiral <- function(epochs, color_by = "default", col_names = NULL) {
  col <- get_epoch_colnames(epochs, col_names)

  epochs <- epochs[seq(1, nrow(epochs), by = 5), ] # Downsampling to 5 min intervals to speed up plotting

  reference_time <- epochs[[col$timestamp]] |>
    parse_time() |>
    min() |>
    lubridate::floor_date(unit = "day")

  epochs <- epochs |>
    dplyr::mutate(
      timestamp = parse_time(.data[[col$timestamp]]),
      sleep_stage = as.character(.data[[col$sleep_stage]]),
      sleep_stage = dplyr::if_else(.data$sleep_stage == "5", "4", .data$sleep_stage)
    ) |>
    tidyr::complete(
      timestamp = seq(min(.data$timestamp), max(.data$timestamp), by = "2 min"),
      fill = list(sleep_stage = "4")
    ) |>
    dplyr::mutate(
      time_in_days = as.numeric(difftime(.data$timestamp, reference_time, units = "days")),
      time_in_min = as.numeric(difftime(.data$timestamp, reference_time, units = "mins")),
      angle = (.data$time_in_days %% 1) * 2 * pi,
      radius = .data$time_in_min
    )

  # Set up color mappings
  if (color_by != "default" && color_by %in% names(epochs)) {
    epochs$color_group <- as.factor(epochs[[color_by]])
    color_levels <- levels(epochs$color_group)
    color_map <- stats::setNames(scales::hue_pal()(length(color_levels)), color_levels)
    # Orange for awake, colormap for others
    epochs$plot_color <- ifelse(
      epochs$sleep_stage == "4",
      "Awake",
      as.character(epochs$color_group)
    )
    color_values <- c(stats::setNames("orange", "Awake"), color_map)
    color_aes <- ggplot2::aes(color = .data$plot_color)
    legend_title <- color_by
  } else {
    # Default: original colors
    epochs$plot_color <- epochs$sleep_stage
    color_values <- c(
      "1" = "#6A3A9A",
      "2" = "#D3B9E6",
      "3" = "#A074C4",
      "4" = "orange"
    )
    color_labels <- c("Light Sleep", "REM Sleep", "Deep Sleep", "Awake")
    color_aes <- ggplot2::aes(color = .data$plot_color)
    legend_title <- "Sleep Stage"
  }

  ggplot2::ggplot(epochs, ggplot2::aes(x = .data$angle, y = .data$radius)) +
    ggplot2::geom_point(
      mapping = color_aes,
      size = 1,
      shape = 16
    ) +
    ggplot2::scale_color_manual(
      values = color_values,
      breaks = if (color_by != "default" && color_by %in% names(epochs)) names(color_map) else c("2", "3", "1", "4"),
      labels = if (color_by != "default" && color_by %in% names(epochs)) names(color_map) else color_labels,
      na.value = "grey"
    ) +
    ggplot2::coord_polar(theta = "x") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 2 * pi, length.out = 25),
      labels = 0:24
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      color = legend_title,
      title = NULL
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 4))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 16),
      legend.position = "right"
    )
}
