test_that("sleep_stages module works", {
  shiny::testServer(
    sleep_stages_module_server,
    args = list(epochs = shiny::reactive(example_epochs)),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png")

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})
