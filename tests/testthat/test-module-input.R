mock_sessions <- data.frame(
  session_id = 1:3,
  session_start = c("2025-03-03T09:00:00", "2025-03-03T20:00:00", "2025-03-04T20:00:00"),
  session_end = c("2025-03-03T11:59:59", "2025-03-03T23:59:59", "2025-03-05T06:00:00")
)

mock_epochs <- data.frame(
  timestamp = as.POSIXct(c(
    "2025-03-03T11:00:00", "2025-03-03T23:00:00",
    "2025-03-04T01:00:00", "2025-03-04T11:00:00"
  ), tz = "UTC"),
  value = c(10, 20, 15, 25)
)

test_folder <- tempdir()
write.csv(mock_sessions, file.path(test_folder, "sessions_reports.csv"), row.names = FALSE)
write.csv(mock_epochs, file.path(test_folder, "epoch_data.csv"), row.names = FALSE)

test_that("input_module returns correct data", {
  loginfo_mock <- mockery::mock()

  shiny::testServer(input_server, {
    mockery::stub(input_server, "logging::loginfo", loginfo_mock)

    data <- session$getReturned()

    session$setInputs(sessions_file = list(name = "sessions_reports.csv", datapath = file.path(test_folder, "sessions_reports.csv")))
    session$setInputs(epochs_file = list(name = "epoch_data.csv", datapath = file.path(test_folder, "epoch_data.csv")))

    expect_equal(class(data$sessions()), "data.frame")
    expect_equal(class(data$epochs()), "data.frame")
    expect_equal(ncol(data$sessions()), 4)
    expect_equal(ncol(data$epochs()), 3)
    expect_equal(nrow(data$sessions()), 3)
    expect_equal(nrow(data$epochs()), 4)
  })
})

test_that("check_csv_column does its job", {
  showNotification_mock <- mockery::mock()
  mockery::stub(check_csv_column, "shiny::showNotification", showNotification_mock)

  expect_true(check_csv_column(file.path(test_folder, "sessions_reports.csv"), "session_start", "unexpected error"))
  expect_false(check_csv_column(file.path(test_folder, "sessions_reports.csv"), "not_a_column", "expected error"))

  expect_true(check_csv_column(file.path(test_folder, "epoch_data.csv"), "timestamp", "unexpected error"))
  expect_false(check_csv_column(file.path(test_folder, "epoch_data.csv"), "not_a_column", "expected error"))
})
