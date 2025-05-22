annotation_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("annotations_text")),
    shiny::fluidRow(
      shiny::column(4, shiny::textInput(ns("annotation_text"), NULL, placeholder = "Annotation")),
      shiny::column(8, shiny::actionButton(ns("apply_annotation"), "Apply"))
    ),
    DT::DTOutput(ns("annotation_table"))
  )
}

annotation_server <- function(id, sessions, sessions_colnames, annotations) {
  shiny::moduleServer(id, function(input, output, session) {

    output$annotations_text <- shiny::renderUI({
      shiny::HTML(paste0(
        "<br/><p>Select sessions to annotate by clicking on the table below.</p>",
        "<p>Type your annotation in the text box and click 'Apply' to save it.</p>"
      ))
    })

    shiny::observe({
      shiny::req(sessions())
      col <- sessions_colnames()
      current_sessions <- sessions()
      ann <- annotations()
      new_ids <- setdiff(current_sessions[[col$id]], ann$id)
      if (length(new_ids) > 0) {
        ann <- rbind(ann, data.frame(id = new_ids, annotation = "", stringsAsFactors = FALSE))
      }
      annotations(ann)
    })

    annotation_table <- shiny::reactive({
      shiny::req(sessions(), annotations(), sessions_colnames())
      make_annotation_table(sessions(), annotations(), sessions_colnames())
    })

    output$annotation_table <- DT::renderDT({
      DT::datatable(
        annotation_table(),
        rownames = FALSE,
        selection = "multiple",
        options = list(dom = "t", pageLength = 100)
      )
    })

    shiny::observeEvent(input$apply_annotation, {
      col <- sessions_colnames()
      ann <- annotations()
      selected <- input$annotation_table_rows_selected
      if (length(selected) > 0) {
        selected_ids <- sessions()[sessions()$display, ][[col$id]][selected]
        ann$annotation[match(selected_ids, ann$id)] <- input$annotation_text
        annotations(ann)
      }
    })

    updated_sessions <- shiny::reactive({
      req(sessions(), annotations())
      col <- sessions_colnames()
      s <- sessions()
      ann <- annotations()
      s$annotation <- ann$annotation[match(s[[col$id]], ann$id)]
      s
    })

    updated_sessions
  })
}

make_annotation_table <- function(sessions, annotations, sessions_colnames) {
  col <- sessions_colnames
  sessions |>
    dplyr::filter(.data$display) |>
    dplyr::mutate(
      annotation = annotations$annotation[match(.data[[col$id]], annotations$id)],
      start = parse_time(.data[[col$session_start]]) |> format("%Y-%m-%d %H:%M"),
      sleep_onset = parse_time(.data[[col$time_at_sleep_onset]]) |> format("%H:%M"),
      wakeup = parse_time(.data[[col$time_at_wakeup]]) |> format("%H:%M"),
      end = parse_time(.data[[col$session_end]]) |> format("%Y-%m-%d %H:%M"),
      session_duration_h = round(difftime(parse_time(.data[[col$session_end]]),
                                          parse_time(.data[[col$session_start]]),
                                          units = "hours"), 2),
      night = format(.data[[col$night]], "%Y-%m-%d"),
      time_in_bed_h = if (!is.null(col$time_in_bed)) round(.data[[col$time_in_bed]] / 60 / 60, 2) else NA
    ) |>
    dplyr::select(
      "annotation",
      "start",
      "sleep_onset",
      "wakeup",
      "end",
      "session_duration_h",
      "time_in_bed_h"
    )
}

annotate_epochs_from_sessions <- function(sessions, epochs, session_colnames, epoch_colnames) {
  if (nrow(epochs) == 0) {
    return(epochs)
  }
  scol <- session_colnames
  ecol <- epoch_colnames

  annotation_map <- stats::setNames(sessions$annotation, sessions[[scol$id]])

  epochs$annotation <- annotation_map[as.character(epochs[[ecol$session_id]])]
  epochs$annotation[is.na(epochs$annotation)] <- ""

  epochs
}
