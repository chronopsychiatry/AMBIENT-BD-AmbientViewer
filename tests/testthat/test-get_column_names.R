sessions <- data.frame(
  id = 1:5,
  custom_id = 11:15,
  .data_type = "somnofy_v2",
  stringsAsFactors = FALSE
)

epochs <- data.frame(
  session_id = 1:5,
  custom_session_id = 11:15,
  .data_type = "somnofy_v2",
  stringsAsFactors = FALSE
)

test_that("get_session_colnames returns default values if col_names is NULL", {
  result <- get_session_colnames(sessions, col_names = NULL)
  expect_equal(result$id, "id")
})

test_that("get_session_colnames overrides default values if col_names is provided", {
  col_names <- list(id = "custom_id")
  result <- get_session_colnames(sessions, col_names)
  expect_equal(result$id, "custom_id")
})

test_that("get_epochs_colnames returns default values if col_names is NULL", {
  result <- get_epoch_colnames(epochs, col_names = NULL)
  expect_equal(result$session_id, "session_id")
})

test_that("get_epochs_colnames overrides default values if col_names is provided", {
  col_names <- list(session_id = "custom_session_id")
  result <- get_epoch_colnames(epochs, col_names)
  expect_equal(result$session_id, "custom_session_id")
})
