test_that("plot_sleep_clock handles valid input correctly", {
  # Mock data
  sessions <- data.frame(
    time_at_sleep_onset = c("2025-03-03T11:24:11.524000+00:00",
                            "2025-03-03T14:44:11.524000+00:00",
                            "2025-03-03T18:24:11.524000+00:00",
                            "2025-03-03T00:24:11.524000+00:00",
                            "2025-03-03T23:24:11.524000+00:00"),
    time_at_wakeup = c("2025-03-03T11:24:11.524000+00:00",
                       "2025-03-03T14:44:11.524000+00:00",
                       "2025-03-03T18:24:11.524000+00:00",
                       "2025-03-03T00:24:11.524000+00:00",
                       "2025-03-03T23:24:11.524000+00:00"),
    night = c("2025-03-02", "2025-03-03", "2025-03-03", "2025-03-03", "2025-03-03"),
    .data_type = "somnofy_v2"
  )
  # Generate the plot
  plot <- plot_sleep_clock(sessions)

  # Check that the output is a ggplot object
  expect_s3_class(plot, "ggplot")
})
