#' Plot Sleep Clock
#'
#' @param sessions The sessions dataframe
#' @returns A ggplot object showing the sleep clock
#' @export
#' @examples
#' plot <- plot_sleep_clock(sessions)
plot_sleep_clock <- function(sessions) {
  # Parse times into numeric hours (0-24)
  sessions <- sessions %>%
    dplyr::filter(!is.na(time_at_sleep_onset) & !is.na(time_at_wakeup)) %>%
    dplyr::mutate(
      sleep_onset_hour = lubridate::hour(ymd_hms(time_at_sleep_onset)) + lubridate::minute(ymd_hms(time_at_sleep_onset)) / 60,
      wakeup_hour = lubridate::hour(ymd_hms(time_at_wakeup)) + lubridate::minute(ymd_hms(time_at_wakeup)) / 60
    )

  # Prepare data for plotting
  sleep_onset_data <- sessions %>%
    dplyr::select(sleep_onset_hour) %>%
    dplyr::mutate(type = "Sleep Onset", color = "purple")

  wakeup_data <- sessions %>%
    dplyr::select(wakeup_hour) %>%
    dplyr::mutate(type = "Wakeup", color = "orange")

  plot_data <- bind_rows(
    sleep_onset_data %>% dplyr::rename(hour = sleep_onset_hour),
    wakeup_data %>% dplyr::rename(hour = wakeup_hour)
  )

  circle_outline <- data.frame(
    hour = seq(0, 24, length.out = 100),
    y = 1
  )

  p <- ggplot2::ggplot(plot_data, aes(x = hour, y = 1, color = type)) +
    geom_path(data = circle_outline, aes(x = hour, y = y), inherit.aes = FALSE, color = "grey", linewidth = 0.5) +
    geom_segment(aes(xend = hour, yend = 0), linewidth = 1) +
    geom_point(aes(x = hour, y = 1), size = 3) +
    scale_x_continuous(
      limits = c(0, 24),
      breaks = seq(0, 24, by = 1),
      labels = seq(0, 24, by = 1)
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = c("Sleep Onset" = "purple", "Wakeup" = "orange")) +
    coord_polar(theta = "x") +  # Convert to polar coordinates
    labs(
      title = NULL,
      x = NULL,
      y = NULL,
      color = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.position = "bottom"
    )
  return(p)
}