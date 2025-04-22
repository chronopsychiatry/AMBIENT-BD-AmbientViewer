test_that("sleep_clock module works", {
  shiny::testServer(
    sleep_clock_module_server,
    args = list(sessions = shiny::reactive(remove_sessions_no_sleep(example_sessions))),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png")

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})
