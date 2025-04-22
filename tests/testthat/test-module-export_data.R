test_that("export_data module works", {
  shiny::testServer(
    export_data_server,
    args = list(sessions = shiny::reactive(example_sessions),
                epochs = shiny::reactive(example_epochs)),
    {
      session$flushReact()
      expect_true(endsWith(output$download_sessions, ".csv"))
      expect_true(endsWith(output$download_epochs, ".csv"))
    }
  )
})
