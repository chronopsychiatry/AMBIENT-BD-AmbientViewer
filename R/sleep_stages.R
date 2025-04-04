#' Plot Sleep Stages
#'
#' @param epochs The epochs dataframe
#' @returns A ggplot object showing the proportion of sleep stages for each night
#' @export
#' @examples
#' plot <- plot_sleep_stages(epochs)
plot_sleep_stages <- function(epochs) {
  sleep_stage_labels <- c(
    "1" = "Awake",
    "2" = "Light",
    "3" = "Deep",
    "4" = "REM",
    "5" = "No presence"
  )
  sleep_stage_colors <- c(  # Colors picked from the VT website
    "Awake" = "#B81722",
    "Light" = "#7DC9D0",
    "Deep" = "#485D78",
    "REM" = "#FAC54E",
    "No presence" = "#D2D2D2"
  )

  # Calculate proportions of sleep stages for each night
  stage_proportions <- epochs %>%
    dplyr::group_by(night, sleep_stage) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(night) %>%
    dplyr::mutate(proportion = count / sum(count),
      sleep_stage = factor(sleep_stage, levels = names(sleep_stage_labels), labels = sleep_stage_labels)
    )

  # Make the bar plot
  p <- ggplot2::ggplot(stage_proportions, aes(x = night, y = proportion, fill = factor(sleep_stage))) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent_format()) +
    # scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
    scale_fill_manual(values = sleep_stage_colors) +
    labs(
      x = "",
      y = "",
      fill = "Sleep Stage",
      title = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.position = "bottom"
    )
  return(p)
}