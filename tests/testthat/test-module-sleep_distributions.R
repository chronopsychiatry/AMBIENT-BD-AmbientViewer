test_that("sleep distribution module works", {
  shiny::testServer(
    sleep_distributions_server,
    args = list(sessions = shiny::reactive(example_sessions), sessions_colnames = shiny::reactive(get_session_colnames(example_sessions))),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png",
                        plot_type = "Boxplot")

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})
