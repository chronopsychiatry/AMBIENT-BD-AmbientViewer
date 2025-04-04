# AmbientViewer

AmbientViewer provides tools to filter and visualise sleep data acquired with a Somnofy device (VitalThings) and downloaded using the [Ambient Downloader package]().

## Interactive interface
The AmbientViewer app is currently under development. You can however run it locally by following these steps:

1. Clone the AmbientViewer repository
```
git clone 
```

2. In R, from the cloned repository, load the shiny library and start the app
``` r
setwd("AmbientViewer")  # Replace with the path to the cloned repository
library(shiny)
runApp("app")
```

## Command line interface
AmbientViewer functions can be used in R to manipulate sessions and epochs data and generate the same plots as in the interactive version.

First install AmbientViewer:

``` r
install.packages("pak")
pak::
```

...description of available functions to be added...

## Example

This is a basic example which shows you how to load data, apply some filters and generate a sleep clock plot:

``` r
library(AmbientViewer)

# Load data
data <- load_data("downloaded_data/zone/subject/data", "2025-03-03_2025-04-04")
sessions <- data$sessions
epochs <- data$epochs

# Filter the data
filtered_sessions <- sessions %>%
  remove_sessions_no_sleep() %>%
  set_min_time_in_bed(2) %>%
  set_session_start_time_range("20:00", "06:00")

# Generate the plot
plot <- plot_sleep_clock(filtered_sessions)
print(plot)
```
