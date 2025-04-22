test_that("sleep_bubbles module works", {
  shiny::testServer(
    sleep_bubbles_module_server,
    args = list(sessions = shiny::reactive(example_sessions)),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png")

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})
