test_that("hypnogram module works", {
  shiny::testServer(
    hypnogram_module_server,
    args = list(epochs = shiny::reactive(example_epochs)),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png")

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})
