test_that("timeseries_sessions module works", {
  shiny::testServer(
    timeseries_sessions_server,
    args = list(sessions = shiny::reactive(example_sessions), sessions_colnames = shiny::reactive(get_session_colnames(example_sessions))),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png",
                        variable = "session_start",
                        exclude_zero = TRUE)

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})
