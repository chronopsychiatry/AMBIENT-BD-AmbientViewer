sessions <- shiny::reactive(example_sessions |> dplyr::mutate(display = TRUE, annotation = ""))
epochs <- shiny::reactive(example_epochs)

test_that("sleep_regularity module works", {
  expect_error(
    shiny::testServer(
      sleep_regularity_server,
      args = list(
        sessions = sessions,
        epochs = epochs,
        sessions_colnames = shiny::reactive(get_session_colnames(sessions())),
        epochs_colnames = shiny::reactive(get_epoch_colnames(epochs()))
        ),
      { session$flushReact() }
    ),
    NA
  )
})
