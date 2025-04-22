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
write.csv(mock_sessions, file.path(test_folder, "sessions_reports.csv"), row.names = FALSE)
write.csv(mock_epochs, file.path(test_folder, "epoch_data.csv"), row.names = FALSE)
write.table(mock_epochs, file.path(test_folder, "epoch_data_text.txt"), row.names = FALSE)


test_that("list_files finds the correct files", {
  result <- list_files(test_folder)

  expect_true("sessions_reports.csv" %in% result)
  expect_true("epoch_data.csv" %in% result)
  expect_false("epoch_data_text.txt" %in% result)
})
