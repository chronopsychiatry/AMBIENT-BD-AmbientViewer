#' Plot bedtimes and waketimes
#'
#' @param sessions The sessions dataframe
#' @param groupby The grouping variable for the plot. Can be "night", "workday", or "weekday".
#' @returns A ggplot graph showing the bedtimes and waketimes
#' @importFrom rlang .data
#' @export
plot_bedtimes_waketimes <- function(sessions, groupby = "night") {
  expansion_factor <- switch(
    groupby,
    workday = 1,
    0.05
  )

  plot_data <- sessions |>
    dplyr::filter(!.data$time_at_sleep_onset == "" & !.data$time_at_wakeup == "") |>
    dplyr::mutate(
      time_at_sleep_onset = parse_time(.data$time_at_sleep_onset),
      time_at_wakeup = parse_time(.data$time_at_wakeup)
    ) |>
    dplyr::mutate(
      group = switch(
        groupby,
        night = .data$night,
        workday = factor(
          ifelse(.data$is_workday, "Weekday", "Weekend"),
          levels = c("Weekend", "Weekday")
        ),
        weekday = factor(
          weekdays(.data$time_at_sleep_onset),
          levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday")
        ),
        .data$night
      )
    ) |>
    dplyr::group_by(.data$group) |>
    dplyr::filter(!all(is.na(.data$time_at_sleep_onset)) & !all(is.na(.data$time_at_wakeup))) |>
    dplyr::summarise(
      sleep_start_labels = mean_time(.data$time_at_sleep_onset),
      sleep_end_labels = mean_time(.data$time_at_wakeup),
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      sleep_start = time_to_hours(shift_times_by_12h(.data$sleep_start_labels)),
      sleep_end = time_to_hours(shift_times_by_12h(.data$sleep_end_labels)),
      sleep_start_labels = parse_time(.data$sleep_start_labels),
      sleep_end_labels = parse_time(.data$sleep_end_labels),
      group_numeric = as.numeric(factor(.data$group))
    )

  ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$sleep_start,
        xmax = .data$sleep_end,
        ymin = .data$group_numeric - 0.4,
        ymax = .data$group_numeric + 0.4
      ),
      fill = "blue",
      alpha = 0.6
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = .data$sleep_start,
        y = .data$group_numeric,
        label = format(.data$sleep_start_labels, "%H:%M")
      ),
      hjust = 1.1,
      size = 6,
      color = "black"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = .data$sleep_end,
        y = .data$group_numeric,
        label = format(.data$sleep_end_labels, "%H:%M")
      ),
      hjust = -0.1,
      size = 6,
      color = "black"
    ) +
    ggplot2::scale_y_continuous(
      breaks = unique(plot_data$group_numeric),
      labels = unique(plot_data$group),
      expand = ggplot2::expansion(mult = c(expansion_factor, 0.05))
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.15, 0.15))
    ) +
    ggplot2::labs(
      title = "Average Sleep onset and Wakeup times",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 24),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 16),
    )
}
