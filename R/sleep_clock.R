#' Plot Sleep Clock
#'
#' @param sessions The sessions dataframe
#' @returns A ggplot object showing the sleep clock
#' @export
plot_sleep_clock <- function(sessions) {
  # Parse times into numeric hours (0-24)
  sessions <- sessions |>
    dplyr::filter(!is.na(time_at_sleep_onset) & !is.na(time_at_wakeup)) |>
    dplyr::mutate(
      sleep_onset_hour = lubridate::hour(lubridate::ymd_hms(time_at_sleep_onset)) + lubridate::minute(lubridate::ymd_hms(time_at_sleep_onset)) / 60,
      wakeup_hour = lubridate::hour(lubridate::ymd_hms(time_at_wakeup)) + lubridate::minute(lubridate::ymd_hms(time_at_wakeup)) / 60
    )

  sleep_onset_data <- sessions |>
    dplyr::select(sleep_onset_hour) |>
    dplyr::mutate(type = "Sleep Onset", color = "purple")

  wakeup_data <- sessions |>
    dplyr::select(wakeup_hour) |>
    dplyr::mutate(type = "Wakeup", color = "orange")

  plot_data <- dplyr::bind_rows(
    sleep_onset_data |> dplyr::rename(hour = sleep_onset_hour),
    wakeup_data |> dplyr::rename(hour = wakeup_hour)
  )

  circle_outline <- data.frame(
    hour = seq(0, 24, length.out = 100),
    y = 1
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = hour, y = 1, color = type)) +
    ggplot2::geom_path(data = circle_outline, ggplot2::aes(x = hour, y = y), inherit.aes = FALSE, color = "grey", linewidth = 0.5) +
    ggplot2::geom_segment(ggplot2::aes(xend = hour, yend = 0), linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(x = hour, y = 1), size = 3) +
    ggplot2::scale_x_continuous(
      limits = c(0, 24),
      breaks = seq(0, 24, by = 1),
      labels = seq(0, 24, by = 1)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_color_manual(values = c("Sleep Onset" = "purple", "Wakeup" = "orange")) +
    ggplot2::coord_polar(theta = "x") +  # Convert to polar coordinates
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
      legend.position = "bottom"
    )
}