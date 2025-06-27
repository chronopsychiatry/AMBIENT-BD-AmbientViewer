#' Generate a patient sleep report in PDF format
#'
#' This function generates a sleep report in PDF format using an R Markdown template.
#' It is designed to work with Somnofy data, so some values may not be available when using other data sources such as GGIR.
#' @param sessions The sessions dataframe
#' @param col_names A list to override default column names. This function uses columns:
#' - `night`
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_at_midsleep`
#' - `sleep_onset_latency`
#' @param output_file Path for the output PDF. Default is "Sleep_report.pdf"
#' @export
sleep_report <- function(sessions, col_names = NULL, output_file = "Sleep_report.pdf") {
  col <- get_session_colnames(sessions, col_names)

  dates <- format(c(min(sessions[[col$night]]), max(sessions[[col$night]])), "%d/%m/%Y")

  # Stats: Time to fall asleep, sleep efficiency, chronotype, Sleep Regularity (based on midsleep standard deviation)
  stats <- list()
  stats$time_to_fall_asleep <- round(mean(sessions[[col$sleep_onset_latency]], na.rm = TRUE) / 60)
  stats$sleep_efficiency <- round(mean(sessions[[col$sleep_period]], na.rm = TRUE) / mean(sessions[[col$time_in_bed]], na.rm = TRUE) * 100)
  stats$chronotype <- ifelse(mean_time(sessions[[col$time_at_midsleep]], unit = "hour") < 4.25, "Morning Lark", "Evening Owl")
  stats$chronotype_image <- ifelse(stats$chronotype == "Morning Lark",
    "Morning_Lark.jpg",
    "Evening_Owl.JPG"
  )
  stats$chronotype_credit <- ifelse(stats$chronotype == "Morning Lark",
    "Artemy Voikhansky - Own work, CC BY-SA 4.0",
    "Charles J. Sharp - Own work, CC BY-SA 4.0"
  )
  stats$sleep_regularity <- round(100 * (1 - sd_time(sessions[[col$time_at_midsleep]]) / 2))

  clock_plot <- plot_sleep_clock(sessions, col_names = col_names) +
    ggplot2::scale_color_manual(
      values = c("Sleep Onset" = "#8e44ad", "Wakeup" = "#e67e22"),
      labels = c("Sleep Onset" = "Sleep Time", "Wakeup" = "Wakeup Time")
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(color = "white"),
      axis.text.x = ggplot2::element_text(color = "white")
    )

  sleep_times <- plot_bedtimes_waketimes(sessions, col_names = col_names, groupby = "weekday") +
    ggplot2::labs(title = NULL)

  sleep_bubbles <- plot_sleep_bubbles(sessions, col_names = col_names) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  template_path <- system.file("Rmd_templates", package = "AmbientViewer")
  rmarkdown::render(
    paste0(template_path, "/Sleep_report.rmd"),
    output_file = basename(output_file),
    params = list(clock_plot = clock_plot, dates = dates, stats = stats, sleep_times = sleep_times, sleep_bubbles = sleep_bubbles),
    output_dir = dirname(output_file),
    quiet = TRUE,
  )
  unlink(paste0(template_path, "/*.log"))
}
