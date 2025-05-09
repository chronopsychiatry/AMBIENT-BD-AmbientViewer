#' Plot Sleep Clock
#'
#' @param sessions The sessions dataframe
#' @param col_names A list to override default column names. This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `night`
#' @returns A ggplot object showing the sleep clock
#' @importFrom rlang .data
#' @export
#' @family plot sessions
plot_sleep_clock <- function(sessions, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)
  sessions <- sessions |>
    dplyr::filter(!.data[[col$time_at_sleep_onset]] == "" & !.data[[col$time_at_wakeup]] == "") |>
    dplyr::mutate(
      sleep_onset_hour = time_to_hours(shift_times_by_12h(.data[[col$time_at_sleep_onset]])),
      wakeup_hour = time_to_hours(shift_times_by_12h(.data[[col$time_at_wakeup]])),
      night = as.factor(.data[[col$night]])
    )

  sleep_onset_data <- sessions |>
    dplyr::select("sleep_onset_hour", "night") |>
    dplyr::mutate(type = "Sleep Onset", color = "purple")

  wakeup_data <- sessions |>
    dplyr::select("wakeup_hour", "night") |>
    dplyr::mutate(type = "Wakeup", color = "orange")

  plot_data <- dplyr::bind_rows(
    sleep_onset_data |> dplyr::rename(hour = "sleep_onset_hour"),
    wakeup_data |> dplyr::rename(hour = "wakeup_hour")
  ) |>
    dplyr::mutate(
      radial_distance = as.numeric(as.factor(.data$night)) / max(as.numeric(as.factor(.data$night)))
    )

  circle_outline <- data.frame(
    hour = seq(0, 24, length.out = 100),
    y = 1
  )

  curve_data <- sessions |>
    dplyr::mutate(
      sleep_onset_radial = as.numeric(as.factor(.data$night)) / max(as.numeric(as.factor(.data$night))),
      wakeup_radial = as.numeric(as.factor(.data$night)) / max(as.numeric(as.factor(.data$night)))
    ) |>
    dplyr::select("sleep_onset_hour", "wakeup_hour", "sleep_onset_radial", "wakeup_radial", "night")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$hour, y = .data$radial_distance, color = .data$type)) +
    ggplot2::geom_path(
      data = circle_outline,
      ggplot2::aes(x = .data$hour, y = .data$y),
      inherit.aes = FALSE,
      color = "grey",
      linewidth = 0.5
    ) +
    ggplot2::geom_segment(
      data = curve_data,
      ggplot2::aes(
        x = .data$sleep_onset_hour,
        y = .data$sleep_onset_radial,
        xend = .data$wakeup_hour,
        yend = .data$wakeup_radial,
        color = .data$night
      ),
      inherit.aes = FALSE,
      linewidth = 0.8
    ) +
    ggplot2::geom_segment(
      data = plot_data |> dplyr::filter(.data$type == "Sleep Onset"),
      ggplot2::aes(
        x = .data$hour,
        y = .data$radial_distance,
        xend = .data$hour,
        yend = 0
      ),
      linewidth = 1,
      color = "purple",
      alpha = 0.8
    ) +
    ggplot2::geom_point(
      data = plot_data |> dplyr::filter(.data$type == "Sleep Onset"),
      ggplot2::aes(
        x = .data$hour,
        y = .data$radial_distance
      ),
      size = 3,
      color = "purple",
      alpha = 0.8
    ) +
    ggplot2::geom_segment(
      data = plot_data |> dplyr::filter(.data$type == "Wakeup"),
      ggplot2::aes(
        x = .data$hour,
        y = .data$radial_distance,
        xend = .data$hour,
        yend = 0
      ),
      linewidth = 1,
      color = "orange",
      alpha = 0.8
    ) +
    ggplot2::geom_point(
      data = plot_data |> dplyr::filter(.data$type == "Wakeup"),
      ggplot2::aes(
        x = .data$hour,
        y = .data$radial_distance
      ),
      size = 3,
      color = "orange",
      alpha = 0.8
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 24),
      breaks = seq(0, 23, by = 1),
      labels = (seq(0, 23, by = 1) + 12) %% 24
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_color_viridis_d(option = "viridis") +
    ggplot2::coord_polar(theta = "x", start = pi) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = NULL,
      color = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 12),
      legend.position = "none"
    )
}
