source("../../R/plots/timeseries.R")

mock_epochs <- data.frame(
  timestamp = c(
    "2025-03-03T11:00:00", "2025-03-03T23:00:00",
    "2025-03-04T01:00:00", "2025-03-04T11:00:00",
    "2025-03-04T13:00:00", "2025-03-04T23:00:00"
  ),
  temperature_ambient_mean = c(10, 20, 15, 25, 30, 35),
  night = as.Date(c(
    "2025-03-02", "2025-03-03",
    "2025-03-03", "2025-03-03",
    "2025-03-04", "2025-03-04"
  )),
  adjusted_time = c(23, 11, 13, 23, 1, 11)
)

test_that("plot_timeseries returns a ggplot object", {
  # Call the function
  plot <- plot_timeseries(mock_epochs, "temperature_ambient_mean")

  # Check that the result is a ggplot object
  expect_s3_class(plot, "ggplot")
})

test_that("plot_timeseries uses the correct x and y mappings", {
  # Call the function
  plot <- plot_timeseries(mock_epochs, "temperature_ambient_mean")

  # Extract the plot data
  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]

  # Check that the x and y mappings are correct
  expect_equal(plot_data$x, mock_epochs$adjusted_time)
  expect_equal(plot_data$y, mock_epochs$temperature_ambient_mean)
})
