test_folder <- tempdir()
write.csv(example_sessions, file.path(test_folder, "sessions_reports.csv"), row.names = FALSE)
write.csv(example_epochs, file.path(test_folder, "epoch_data.csv"), row.names = FALSE)

test_that("input_module returns correct data", {
  loginfo_mock <- mockery::mock()

  shiny::testServer(input_server, {
    mockery::stub(input_server, "logging::loginfo", loginfo_mock)

    data <- session$getReturned()

    session$setInputs(sessions_file = list(name = "sessions_reports.csv", datapath = file.path(test_folder, "sessions_reports.csv")))
    session$setInputs(epochs_file = list(name = "epoch_data.csv", datapath = file.path(test_folder, "epoch_data.csv")))

    expect_equal(class(data$sessions()), "data.frame")
    expect_equal(class(data$epochs()), "data.frame")
    expect_equal(ncol(data$sessions()), 69)
    expect_equal(ncol(data$epochs()), 17)
    expect_equal(nrow(data$sessions()), 124)
    expect_equal(nrow(data$epochs()), 18755)
  })
})

test_that("check_csv_column does its job", {
  show_notification_mock <- mockery::mock()
  mockery::stub(check_csv_column, "shiny::showNotification", show_notification_mock)

  expect_true(check_csv_column(file.path(test_folder, "sessions_reports.csv"), "session_start", "unexpected error"))
  expect_false(check_csv_column(file.path(test_folder, "sessions_reports.csv"), "not_a_column", "expected error"))

  expect_true(check_csv_column(file.path(test_folder, "epoch_data.csv"), "timestamp", "unexpected error"))
  expect_false(check_csv_column(file.path(test_folder, "epoch_data.csv"), "not_a_column", "expected error"))
})
