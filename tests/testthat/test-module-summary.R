test_that("summary module works", {
  shiny::testServer(
    summary_server,
    args = list(sessions = shiny::reactive(example_sessions),
                epochs = shiny::reactive(example_epochs)),
    {
      session$flushReact()
      expect_equal(
        sessions_summary_table(),
        get_sessions_summary(example_sessions)
      )
      expect_equal(
        epochs_summary_table(),
        get_epochs_summary(example_epochs)
      )
    }
  )
})
