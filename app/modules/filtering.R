source("../R/compliance.R")
source("../R/filtering.R")

filtering_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::sliderTextInput(
      inputId = ns("time_range"),
      label = "Session Start Time:",
      choices = c(
                  "13", "14", "15", "16", "17",
                  "18", "19", "20", "21", "22", "23",
                  "00", "01", "02", "03", "04", "05",
                  "06", "07", "08", "09", "10", "11",
                  "12"),
      selected = c("13", "12"),
      grid = TRUE
    ),
    shiny::sliderInput(
      ns("min_time_in_bed"),
      "Minimum Time in Bed:",
      min = 0, max = 12, value = 0, step = 1, post = "h",
      ticks = FALSE
    )
    # shiny::uiOutput(ns("subject_selector")),
    # shiny::uiOutput(ns("device_selector"))
  )
}

filtering_server <- function(id, sessions) {
  shiny::moduleServer(id, function(input, output, session) {
    output$subject_selector <- shiny::renderUI({
      subject_selector(
        inputId = session$ns("selected_subjects"),
        sessions = sessions
      )
    })

    output$device_selector <- shiny::renderUI({
      device_selector(
        inputId = session$ns("selected_devices"),
        sessions = sessions
      )
    })

    filtered_sessions <- shiny::reactive({
      shiny::req(sessions())

      # Convert time range to "HH:MM" format
      from_time <- sprintf("%02d:00", as.numeric(input$time_range[1]) %% 24)
      to_time <- sprintf("%02d:00", as.numeric(input$time_range[2]) %% 24)

      # Apply filters
      filtered <- remove_sessions_no_sleep(sessions())
      filtered <- set_min_time_in_bed(filtered, input$min_time_in_bed)
      filtered <- set_session_start_time_range(filtered, from_time, to_time)

      # if (!is.null(input$selected_subjects)) {
      #   filtered <- select_subjects(filtered, input$selected_subjects)
      # }

      return(filtered)
    })

    return(filtered_sessions)
  })
}

# subject_selector <- function(inputId, sessions) {
#   shiny::req(sessions())
#   subject_ids <- unique(sessions()$subject_id)
#   shinyWidgets::pickerInput(
#     inputId = inputId,
#     label = "Subjects:",
#     choices = subject_ids,
#     selected = subject_ids,  # Default to all subjects selected
#     multiple = TRUE,
#     options = list(
#       `actions-box` = TRUE,
#       `live-search` = TRUE
#     )
#   )
# }

# device_selector <- function(inputId, sessions) {
#   shiny::req(sessions())
#   device_ids <- unique(sessions()$device_serial_number)
#   shinyWidgets::pickerInput(
#     inputId = inputId,
#     label = "Devices:",
#     choices = device_ids,
#     selected = device_ids,  # Default to all devices selected
#     multiple = TRUE,
#     options = list(
#       `actions-box` = TRUE,
#       `live-search` = TRUE
#     )
#   )
# }