sessions <- shiny::reactive(example_sessions |> dplyr::mutate(display = TRUE))
epochs <- shiny::reactive(example_epochs |> dplyr::mutate(display = TRUE))

test_that("summary module works", {
  shiny::testServer(
    summary_server,
    args = list(sessions = sessions,
                epochs = epochs,
                sessions_colnames = shiny::reactive(get_session_colnames(sessions())),
                epochs_colnames = shiny::reactive(get_epoch_colnames(epochs()))),
    {
      session$flushReact()
      expect_equal(
        sessions_summary_table(),
        get_sessions_summary(sessions())
      )
      expect_equal(
        epochs_summary_table(),
        get_epochs_summary(epochs())
      )
    }
  )
})
