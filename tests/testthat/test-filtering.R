sessions <- data.frame(
  id = c("A", "B", "C", "D", "E"),
  subject_id = c("sub_A", "sub_A", "sub_A", "sub_B", "sub_C"),
  device_serial_number = c("VT_001", "VT_001", "VT_001", "VT_002", "VT_003"),
  session_start = c(
    "2025-03-10T19:30:00.000000+00:00",
    "2025-03-11T23:45:00.000000+00:00",
    "2025-03-12T01:15:00.000000+00:00",
    "2025-03-12T03:00:00.000000+00:00",
    "2025-03-13T18:00:00.000000+00:00"
  ),
  time_in_bed = c(8 * 60 * 60, 6 * 60 * 60, 7 * 60 * 60, 0.5 * 60 * 60, 1 * 60 * 60),
  sleep_period = c(1736, 0, 0, 26364, 0),
  night = c(
    "2025-03-10",
    "2025-03-11",
    "2025-03-11",
    "2025-03-11",
    "2025-03-13"
  ),
  .data_type = "somnofy_v2"
)

sessions_v1 <- data.frame(
  session_id = c("A", "B", "C", "D", "E"),
  subject_id = c("sub_A", "sub_A", "sub_A", "sub_B", "sub_C"),
  birth_year = lubridate::year(Sys.Date()) - c(11, 12, 14, 20, 30),
  sex = c("Male", "Female", "Other", "Female", "Male"),
  session_start = c(
    "2025-03-10T19:30:00.000000+00:00",
    "2025-03-11T23:45:00.000000+00:00",
    "2025-03-12T01:15:00.000000+00:00",
    "2025-03-12T03:00:00.000000+00:00",
    "2025-03-13T18:00:00.000000+00:00"
  ),
  time_in_bed = c(8 * 60 * 60, 6 * 60 * 60, 7 * 60 * 60, 0.5 * 60 * 60, 1 * 60 * 60),
  sleep_period = c(1736, 0, 0, 26364, 0),
  night = c(
    "2025-03-10",
    "2025-03-11",
    "2025-03-11",
    "2025-03-11",
    "2025-03-13"
  ),
  .data_type = "somnofy_v1"
)

epochs <- data.frame(
  session_id = c("A", "A", "A", "I", "I", "C", "D", "E", "F", "G", "H", "B", "J"),
  epoch_data = c(1, 10, 20, 2, 3, 4, 5, 6, 7, 8, 9, 10, 5),
  .data_type = "somnofy_v2"
)

test_that("filter_epochs_from_sessions works", {
  filtered_epochs <- filter_epochs_from_sessions(epochs, sessions)
  expect_equal(nrow(filtered_epochs), 7)
})

test_that("filter_epochs_from_sessions shows warning if tables don't overlap", {
  expect_warning(
    filter_epochs_from_sessions(epochs, data.frame(id = c("X", "Y", "Z"), .data_type = "somnofy_v2")),
    "None of the epochs match the selected sessions"
  )
})

test_that("filter_epochs_from_sessions creates flags", {
  filtered_epochs <- filter_epochs_from_sessions(epochs, sessions, flag_only = TRUE)
  expect_equal(sum(filtered_epochs$display), 7)
})

test_that("filter_by_night_range works", {
  filtered_sessions <- filter_by_night_range(sessions, "2025-03-11", "2025-03-12")
  expect_equal(nrow(filtered_sessions), 3)
})

test_that("filter_by_night_range flagging works", {
  filtered_sessions <- filter_by_night_range(sessions, "2025-03-11", "2025-03-12", flag_only = TRUE)
  expect_equal(sum(filtered_sessions$display), 3)
})

test_that("filter_by_age_range works", {
  filtered_sessions <- filter_by_age_range(sessions_v1, 11, 18)
  expect_equal(nrow(filtered_sessions), 3)
})

test_that("filter_by_age_range flagging works", {
  filtered_sessions <- filter_by_age_range(sessions_v1, 11, 18, flag_only = TRUE)
  expect_equal(sum(filtered_sessions$display), 3)
})

test_that("filter_by_sex works", {
  filtered_sessions <- filter_by_sex(sessions_v1, "Male")
  expect_equal(nrow(filtered_sessions), 2)
})

test_that("filter_by_sex flagging works", {
  filtered_sessions <- filter_by_sex(sessions_v1, "Male", flag_only = TRUE)
  expect_equal(sum(filtered_sessions$display), 2)
})

test_that("filter_by_sex works with multiple inputs", {
  filtered_sessions <- filter_by_sex(sessions_v1, c("Female", "Other"))
  expect_equal(nrow(filtered_sessions), 3)
})

test_that("select_subjects works", {
  selected_sessions <- select_subjects(sessions, c("sub_A", "sub_B"))
  expect_equal(nrow(selected_sessions), 4)
})

test_that("select_subjects flagging works", {
  selected_sessions <- select_subjects(sessions, c("sub_A", "sub_B"), flag_only = TRUE)
  expect_equal(sum(selected_sessions$display), 4)
})

test_that("select_subjects shows warning if no subjects are found", {
  expect_warning(
    select_subjects(sessions, c("X", "Y", "Z")),
    "None of the subject IDs were found"
  )
})

test_that("select_devices works", {
  selected_sessions <- select_devices(sessions, c("VT_001", "VT_003"))
  expect_equal(nrow(selected_sessions), 4)
})

test_that("select_devices flagging works", {
  selected_sessions <- select_devices(sessions, c("VT_001", "VT_003"), flag_only = TRUE)
  expect_equal(sum(selected_sessions$display), 4)
})

test_that("select_devices shows warning if no devices are found", {
  expect_warning(
    select_devices(sessions, c("X", "Y", "Z")),
    "None of the device IDs were found"
  )
})
