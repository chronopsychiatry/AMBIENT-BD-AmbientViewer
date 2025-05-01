#' Plot Hypnogram
#'
#' @param epochs The epochs dataframe
#' @returns A ggplot object showing the hypnogram as bars
#' @importFrom rlang .data
#' @export
#' @family plot epochs
#' @seealso [plot_sleep_stages()] to show the proportion of each sleep stage per day
plot_hypnogram <- function(epochs) {
  sleep_stage_labels <- c(
    "1" = "Deep",
    "2" = "Light",
    "3" = "REM",
    "4" = "Awake"
  )
  sleep_stage_colors <- c(  # Colors picked from the VT website
    "Awake" = "#B81722",
    "Light" = "#7DC9D0",
    "Deep" = "#485D78",
    "REM" = "#FAC54E"
  )

  sleep_stage_numeric <- c(
    "Deep" = 1,
    "Light" = 2,
    "REM" = 3,
    "Awake" = 4
  )

  hypnogram_data <- epochs |>
    dplyr::filter(.data$sleep_stage != "5") |>
    dplyr::mutate(
      timestamp = char_to_posixct(.data$timestamp),
      sleep_stage = factor(.data$sleep_stage, levels = names(sleep_stage_labels), labels = sleep_stage_labels),
      sleep_stage_numeric = as.numeric(sleep_stage_numeric[as.character(.data$sleep_stage)])
    ) |>
    dplyr::group_by(timestamp = lubridate::floor_date(.data$timestamp, unit = "minute")) |>
    dplyr::summarize(
      sleep_stage_numeric = dplyr::first(.data$sleep_stage_numeric),
      sleep_stage = dplyr::first(.data$sleep_stage),
      .groups = "drop"
    )

  ggplot2::ggplot(hypnogram_data) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$timestamp,
        xmax = .data$timestamp + lubridate::minutes(1),
        ymin = 0,
        ymax = .data$sleep_stage_numeric,
        fill = .data$sleep_stage
      )
    ) +
    ggplot2::scale_x_datetime(
      date_labels = "%Y-%m-%d\n%H:%M",
    ) +
    ggplot2::scale_y_continuous(
      breaks = sleep_stage_numeric,
      labels = names(sleep_stage_numeric),
      limits = c(0, max(sleep_stage_numeric) + 2)
    ) +
    ggplot2::scale_fill_manual(values = sleep_stage_colors) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = "Sleep Stage",
      title = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 14),
      legend.position = "bottom"
    )
}
