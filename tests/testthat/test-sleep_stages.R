test_that("plot_sleep_stages handles valid input correctly", {
  # Mock data
  epochs <- data.frame(
    night = lubridate::as_date(c("2025-03-01", "2025-03-01", "2025-03-02", "2025-03-02", "2025-03-02")),
    sleep_stage = c(1, 2, 3, 1, 4)
  )

  # Generate the plot
  plot <- plot_sleep_stages(epochs)

  # Check that the output is a ggplot object
  expect_s3_class(plot, "ggplot")
})
