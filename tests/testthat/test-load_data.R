mock_sessions <- data.frame(
  session_id = 1:3,
  session_start = c("2025-03-03T09:00:00", "2025-03-03T20:00:00", "2025-03-04T20:00:00"),
  session_end = c("2025-03-03T11:59:59", "2025-03-03T23:59:59", "2025-03-05T06:00:00")
)

mock_epochs <- data.frame(
  timestamp = c(
    "2025-03-03T11:00:00", "2025-03-03T23:00:00",
    "2025-03-04T01:00:00", "2025-03-04T11:00:00"
  ),
  value = c(10, 20, 15, 25)
)

test_folder <- tempdir()
write.csv(mock_sessions, file.path(test_folder, "2025-03-03_2025-03-04_sessions_reports.csv"), row.names = FALSE)
write.csv(mock_epochs, file.path(test_folder, "2025-03-03_2025-03-04_epoch_data.csv"), row.names = FALSE)

test_that("load_sessions loads sessions correctly", {
  sessions <- load_sessions(file.path(test_folder, "2025-03-03_2025-03-04_sessions_reports.csv"))

  expect_equal(nrow(sessions), 3)
  expect_equal(sessions$session_id, c(1, 2, 3))
})

test_that("load_epochs loads epochs correctly", {
  epochs <- load_epochs(file.path(test_folder, "2025-03-03_2025-03-04_epoch_data.csv"))

  expect_equal(nrow(epochs), 4)
  expect_equal(epochs$value, c(10, 20, 15, 25))
})
