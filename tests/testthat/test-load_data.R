mock_sessions <- data.frame(
  session_id = 1:2,
  start_time = c("2025-03-03T12:00:00", "2025-03-04T12:00:00"),
  end_time = c("2025-03-03T23:59:59", "2025-03-04T23:59:59")
)

mock_epochs <- data.frame(
  timestamp = c(
    "2025-03-03T11:00:00", "2025-03-03T23:00:00",
    "2025-03-04T01:00:00", "2025-03-04T11:00:00"
  ),
  value = c(10, 20, 15, 25)
)

# Write mock CSV files for testing
test_folder <- tempdir()
write.csv(mock_sessions, file.path(test_folder, "2025-03-03_2025-03-04_sessions_reports.csv"), row.names = FALSE)
write.csv(mock_epochs, file.path(test_folder, "2025-03-03_2025-03-04_epoch_data.csv"), row.names = FALSE)

test_that("load_data loads sessions and epochs correctly", {
  result <- load_data(test_folder, "2025-03-03_2025-03-04")

  # Check that sessions are loaded correctly
  expect_equal(nrow(result$sessions), 2)
  expect_equal(result$sessions$session_id, c(1, 2))

  # Check that epochs are loaded correctly
  expect_equal(nrow(result$epochs), 4)
  expect_equal(result$epochs$value, c(10, 20, 15, 25))
})

test_that("group_epochs_by_night correctly groups epochs by night", {
  grouped_epochs <- group_epochs_by_night(mock_epochs)

  # Check that the night column is created correctly
  expect_equal(grouped_epochs$night, as.Date(c("2025-03-03", "2025-03-04", "2025-03-04", "2025-03-04")))

  # Check that the adjusted_time column is calculated correctly
  expect_equal(grouped_epochs$adjusted_time, c(23, 11, 13, 23))
})