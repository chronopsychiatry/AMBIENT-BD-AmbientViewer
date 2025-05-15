annotation_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("annotation_table"))
  )
}

annotation_server <- function(id, sessions, sessions_colnames) {
  shiny::moduleServer(id, function(input, output, session) {

    annotation_table <- shiny::reactiveVal()
    shiny::observe({
      req(sessions(), sessions_colnames())
      annotation_table(make_annotation_table(sessions(), sessions_colnames()))
    })

    output$annotation_table <- DT::renderDT({
      DT::datatable(
        annotation_table,
        editable = list(target = "cell", columns = "annotation")
      )
    })

    shiny::observeEvent(input$annotation_table_cell_edit, {
      info <- input$annotation_table_cell_edit
      tbl <- annotation_table()
      tbl[info$row, info$col] <- info$value
      annotation_table(tbl)
    })

    updated_sessions <- shiny::reactive({
      req(sessions(), annotation_table())
      s <- sessions()
      s$annotation <- annotation_table()$annotation
      s
    })

    updated_sessions
  })
}

make_annotation_table <- function(sessions, sessions_colnames) {
  col <- get_session_colnames(sessions, sessions_colnames)
  sessions |>
    dplyr::mutate(
      annotation = "",
      start_time = parse_time(.data[[col$session_start]]) |> format("%Y-%m-%d %H:%M"),
      sleep_onset = parse_time(.data[[col$time_at_sleep_onset]]) |> format("%H:%M"),
      wakeup_time = parse_time(.data[[col$time_at_wakeup]]) |> format("%H:%M"),
      end_time = parse_time(.data[[col$session_end]]) |> format("%Y-%m-%d %H:%M"),
      session_duration_h = difftime(parse_time(.data[[col$session_end]]),
                                    parse_time(.data[[col$session_start]]),
                                    units = "hours"),
      night = format(.data[[col$night]], "%Y-%m-%d"),
      time_in_bed_h = .data[[col$time_in_bed]] / 60 / 60
    ) |>
    dplyr::select(
      "annotation",
      col$id,
      col$night,
      "start_time",
      "sleep_onset",
      "wakeup_time",
      "end_time",
      "session_duration_h",
      "time_in_bed_h"
    )
}
