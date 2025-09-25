#' Convert EDF files in a folder to a single CSV
#'
#' This function scans a specified folder (and its subdirectories) for EDF files
#' whose filenames contain "BRP" (case-insensitive), reads their headers to extract
#' start times and durations, and compiles this information into a single CSV file.
#' @param folder_in The input folder containing EDF files. Default is the current directory.
#' @param file_out The name of the output CSV file. Default is "edf_summary.csv".
#' @return A CSV file summarizing the start times and durations of the EDF files.
#' @importFrom rlang .data
#' @importFrom utils write.csv
#' @export
#' @family file conversion
edfs_to_csv <- function(folder_in = ".", file_out = "edf_summary.csv") {

  all_edf <- list.files(
    path = folder_in,
    pattern = "(?i)\\.edf$", # case-insensitive .edf
    recursive = TRUE,
    full.names = TRUE
  )

  edf_files <- all_edf[grepl("BRP", basename(all_edf), ignore.case = TRUE)]

  rows <- lapply(edf_files, function(f) {
    hdr <- edfReader::readEdfHeader(f)
    data.frame(
      device_id = stringr::str_extract(hdr$recordingId, "(?<=SRN=)\\d+"),
      startTime = as.POSIXct(hdr$startTime),
      sleep_period = as.numeric(hdr$recordedPeriod),  # seconds
      endTime = as.POSIXct(hdr$startTime) + as.numeric(hdr$recordedPeriod),
      midsleep = as.POSIXct(hdr$startTime) + as.numeric(hdr$recordedPeriod) / 2,
      is_workday = weekdays(hdr$startTime) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, rows) |>
    dplyr::filter(.data$sleep_period > 0) |>
    dplyr::mutate(session_id = dplyr::row_number())
  write.csv(df, file = paste0(folder_in, "/edf_summary.csv"), row.names = FALSE)
}
