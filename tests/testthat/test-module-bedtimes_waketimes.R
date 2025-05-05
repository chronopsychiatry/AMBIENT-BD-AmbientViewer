test_that("bedtimes_waketimes module works", {
  shiny::testServer(
    bedtimes_waketimes_module_server,
    args = list(sessions = shiny::reactive(remove_sessions_no_sleep(example_sessions))),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png")
      session$setInputs(groupby = "weekday")

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})
