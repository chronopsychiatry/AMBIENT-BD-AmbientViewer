sessions <- shiny::reactive(example_sessions |> dplyr::mutate(display = TRUE, annotation = ""))

test_that("compliance module works", {
  shiny::testServer(
    compliance_server,
    args = list(sessions = sessions, sessions_colnames = shiny::reactive(get_session_colnames(sessions()))),
    {
      session$flushReact()
      expect_equal(
        compliance_table(),
        get_compliance_table(sessions(), col_names = sessions_colnames())
      )
    }
  )
})

test_that("get_compliance_table output is correct", {
  result <- get_compliance_table(example_sessions |>
                                   dplyr::mutate(display = TRUE, annotation = ""), col_names = get_session_colnames(example_sessions))

  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 123)
  expect_equal(ncol(result), 9)
  expect_length(unique(result$night), 14)
})

test_that("make_sessions_display_table output is correct", {
  result <- make_sessions_display_table(example_sessions |>
                                          dplyr::mutate(annotation = ""), col_names = get_session_colnames(example_sessions))

  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 124)
  expect_equal(ncol(result), 9)
  expect_length(unique(result$night), 15)
})
