ggir_sessions <- data.frame(
  ID = "A",
  calendar_date = as.Date("2023-01-01") + 0:4,
  start_end_window = c("07:44:10-07:06:55",
                       "08:23:35-10:02:50",
                       "10:02:55-08:11:05",
                       "08:11:10-08:15:30",
                       "07:25:00-07:09:25"),
  sleeponset_ts = c("23:59:10", "22:38:10", "22:51:40", 
                    "23:35:10", "23:08:35"),
  wakeup_ts = c("07:07:00", "10:02:55", "08:11:10", 
                "08:15:35", "07:25:00"),
  dur_spt_sleep_min = c(379, 570.417, 478.917, 457.333, 416.25),
  daytype = c("WD", "WE", "WE", "WD", "WD")
)

ggir_epochs <- data.frame(
  timenum = c(1663829050, 1663829055, 1663829060, 1663829065, 1663829070),
  class_id = c(12, 0, 0, 12, 12),
  .data_type = "ggir"
)

test_that("clean_ggir_sessions works", {
  result <- clean_ggir_sessions(ggir_sessions)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_equal(result$session_start[1], as.POSIXct("2023-01-01 07:44:10"))
  expect_equal(class(result$is_workday), "logical")
  expect_true("time_in_bed" %in% names(result))
})

test_that("clean_epochs works for ggir format", {
  result <- clean_epochs(ggir_epochs)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_equal(result$timenum[1], as.POSIXct("2022-09-22 07:44:10", tz = "Europe/London"))
  expect_true("is_asleep" %in% names(result))
  expect_equal(result$is_asleep[1], 0)
})

test_that("clean_epochs works for somnofy_v2 format", {
  result <- clean_epochs(example_epochs)
  expect_equal(nrow(result), nrow(example_epochs))
  expect_equal(unique(result$is_asleep), c(0, 1))
})
