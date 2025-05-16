test_that("timeseries module works", {
  shiny::testServer(
    timeseries_module_server,
    args = list(epochs = shiny::reactive(example_epochs), epochs_colnames = shiny::reactive(get_epoch_colnames(example_epochs))),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png",
                        variable = "light_ambient_mean",
                        exclude_zero = TRUE)

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})
