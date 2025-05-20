#' Plot Sleep Clock
#'
#' @param sessions The sessions dataframe
#' @param col_names A list to override default column names. This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `night`
#' @param color_by The variable to color the segments by. Can be "default" or any other column name in the sessions dataframe.
#' @returns A ggplot object showing the sleep clock
#' @importFrom rlang .data
#' @export
#' @family plot sessions
plot_sleep_clock <- function(sessions, color_by = "default", col_names = NULL) {
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
    dplyr::mutate(type = "Sleep Onset")

  wakeup_data <- sessions |>
    dplyr::select("wakeup_hour", "night") |>
    dplyr::mutate(type = "Wakeup")

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

  if (color_by != "default" && color_by %in% names(sessions)) {
    # All elements colored by color_by
    plot_data$color_group <- as.factor(sessions[[color_by]])[match(plot_data$night, sessions$night)]
    curve_data$color_group <- as.factor(sessions[[color_by]])[match(curve_data$night, sessions$night)]
    color_scale <- ggplot2::scale_color_manual(
      values = stats::setNames(
        scales::hue_pal()(length(levels(curve_data$color_group))),
        levels(curve_data$color_group)
      )
    )
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$hour, y = .data$radial_distance)) +
      ggplot2::geom_path(
        data = circle_outline,
        ggplot2::aes(x = .data$hour, y = .data$y),
        inherit.aes = FALSE,
        color = "grey",
        linewidth = 0.5
      ) +
      # Curved lines
      ggplot2::geom_segment(
        data = curve_data,
        ggplot2::aes(
          x = .data$sleep_onset_hour,
          y = .data$sleep_onset_radial,
          xend = .data$wakeup_hour,
          yend = .data$wakeup_radial,
          color = .data$color_group
        ),
        linewidth = 0.8,
        show.legend = TRUE
      ) +
      # Sleep Onset straight lines
      ggplot2::geom_segment(
        data = plot_data |> dplyr::filter(.data$type == "Sleep Onset"),
        ggplot2::aes(
          x = .data$hour,
          y = .data$radial_distance,
          xend = .data$hour,
          yend = 0,
          color = .data$color_group
        ),
        linewidth = 1,
        alpha = 0.8,
        show.legend = TRUE
      ) +
      # Wakeup straight lines
      ggplot2::geom_segment(
        data = plot_data |> dplyr::filter(.data$type == "Wakeup"),
        ggplot2::aes(
          x = .data$hour,
          y = .data$radial_distance,
          xend = .data$hour,
          yend = 0,
          color = .data$color_group
        ),
        linewidth = 1,
        alpha = 0.8,
        show.legend = TRUE
      ) +
      # Points
      ggplot2::geom_point(
        data = plot_data,
        ggplot2::aes(color = .data$color_group),
        size = 3,
        alpha = 0.8
      ) +
      color_scale
  } else {
    # Curved lines by night (viridis), straight lines/points by type (purple/orange)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$hour, y = .data$radial_distance)) +
      ggplot2::geom_path(
        data = circle_outline,
        ggplot2::aes(x = .data$hour, y = .data$y),
        inherit.aes = FALSE,
        color = "grey",
        linewidth = 0.5
      ) +
      # Curved lines (first color scale)
      ggplot2::geom_segment(
        data = curve_data,
        ggplot2::aes(
          x = .data$sleep_onset_hour,
          y = .data$sleep_onset_radial,
          xend = .data$wakeup_hour,
          yend = .data$wakeup_radial,
          color = .data$night
        ),
        linewidth = 0.8,
        show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(
        values = stats::setNames(
          grDevices::hcl.colors(length(unique(curve_data$night)), "viridis"),
          unique(curve_data$night)
        )
      ) +
      ggnewscale::new_scale_color() +
      # Sleep Onset straight lines (second color scale)
      ggplot2::geom_segment(
        data = plot_data |> dplyr::filter(.data$type == "Sleep Onset"),
        ggplot2::aes(
          x = .data$hour,
          y = .data$radial_distance,
          xend = .data$hour,
          yend = 0,
          color = .data$type
        ),
        linewidth = 1,
        alpha = 0.8,
        show.legend = FALSE
      ) +
      # Wakeup straight lines
      ggplot2::geom_segment(
        data = plot_data |> dplyr::filter(.data$type == "Wakeup"),
        ggplot2::aes(
          x = .data$hour,
          y = .data$radial_distance,
          xend = .data$hour,
          yend = 0,
          color = .data$type
        ),
        linewidth = 1,
        alpha = 0.8,
        show.legend = FALSE
      ) +
      # Points
      ggplot2::geom_point(
        data = plot_data |> dplyr::filter(.data$type == "Sleep Onset"),
        ggplot2::aes(color = .data$type),
        size = 3,
        alpha = 0.8
      ) +
      ggplot2::geom_point(
        data = plot_data |> dplyr::filter(.data$type == "Wakeup"),
        ggplot2::aes(color = .data$type),
        size = 3,
        alpha = 0.8
      ) +
      ggplot2::scale_color_manual(
        values = c("Sleep Onset" = "#8e44ad", "Wakeup" = "#e67e22")
      )
  }

  p <- p +
    ggplot2::scale_x_continuous(
      limits = c(0, 24),
      breaks = seq(0, 23, by = 1),
      labels = (seq(0, 23, by = 1) + 12) %% 24
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::coord_polar(theta = "x", start = pi) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = NULL,
      color = color_by
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
  p
}
