#' Plot Sleep Bubbles
#'
#' @description This function creates a bubble plot of sleep sessions, where the size and colour of the bubbles represents the sleep duration.
#' @param sessions The sessions dataframe.
#' @param col_names A list to override default column names. This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `night`
#' @returns A ggplot object containing the sleep bubbles graph.
#' @importFrom rlang .data
#' @export
#' @family plot sessions
plot_sleep_bubbles <- function(sessions, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)
  sessions <- sessions |>
    dplyr::filter(sessions[[col$time_at_sleep_onset]] != "" &
                    sessions[[col$time_at_wakeup]] != "") |>
    dplyr::mutate(
      sleep_duration = as.numeric(difftime(
        parse_time(.data[[col$time_at_wakeup]]),
        parse_time(.data[[col$time_at_sleep_onset]]),
        units = "hours"
      )),
      color = suppressWarnings(dplyr::case_when(
        sleep_duration >= 6 & sleep_duration <= 9 ~ scales::col_numeric(
          palette = c("darkblue", "lightgreen"),
          domain = c(6, 9)
        )(sleep_duration),
        TRUE ~ "grey"
      ))
    )

  ggplot2::ggplot(sessions, ggplot2::aes(x = .data$night, y = .data$sleep_duration, color = .data$color)) +
    ggplot2::annotate(
      "rect",
      xmin = min(sessions[[col$night]]) - 1, xmax = max(sessions[[col$night]]) + 1,
      ymin = 6, ymax = 9,
      fill = "lightgrey", alpha = 0.5
    ) +
    ggplot2::geom_point(size = 20, alpha = 0.5) +
    ggplot2::scale_color_identity() +
    ggplot2::labs(
      x = NULL,
      y = "Sleep Duration (hours)",
      title = NULL
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
