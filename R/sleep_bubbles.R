#' Plot Sleep Bubbles
#'
#' @description This function creates a bubble plot of sleep sessions, where the size and colour of the bubbles represents the sleep duration.
#' @param sessions The sessions dataframe.
#' @returns A ggplot object containing the sleep bubbles graph.
#' @export
plot_sleep_bubbles <- function(sessions) {
  sessions <- sessions %>%
    dplyr::filter(sessions$time_at_sleep_onset != "" &
                    sessions$time_at_wakeup != "") %>%
    dplyr::mutate(
      sleep_duration = as.numeric(difftime(
        as.POSIXct(time_at_wakeup, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
        as.POSIXct(time_at_sleep_onset, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
        units = "hours"
      )),
      # Assign colors based on sleep duration
      color = suppressWarnings(dplyr::case_when(
        sleep_duration >= 6 & sleep_duration <= 9 ~ scales::col_numeric(
          palette = c("darkblue", "lightgreen"),
          domain = c(6, 9)
        )(sleep_duration),
        TRUE ~ "grey"
      ))
    )

  # Create the dot plot
  p <- ggplot2::ggplot(sessions, ggplot2::aes(x = night, y = sleep_duration, color = color)) +
    ggplot2::annotate(
      "rect",
      xmin = min(sessions$night)-1, xmax = max(sessions$night)+1,
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

  return(p)
}
