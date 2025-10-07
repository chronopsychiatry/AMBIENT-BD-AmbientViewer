MB <- 1024^2
UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB * MB)

# Source helper functions
source("plot_helpers.R")
source("download_handlers.R")

# Source all app modules
modules <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
for (m in modules) source(m)

# Initialise common
source(system.file("shiny", "common.R", package = "AmbientViewer"))
common <- common_class$new()
