# Reads the example sessions data from a CSV file and saves it as an R data object

library(AmbientViewer)

example_sessions <- load_sessions("data-raw/example_sessions.csv")

usethis::use_data(example_sessions, overwrite = TRUE)
