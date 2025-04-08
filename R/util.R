#' Check if a column contains datetime data in ISO 8601 format
#'
#' @param column A vector of character strings to check
#' @returns TRUE if all non-NA values are in ISO 8601 format, FALSE otherwise
#' @export
is_iso8601_datetime <- function(column) {
  column <- column[!is.na(column) & column != ""]
  parsed <- suppressWarnings(lubridate::ymd_hms(column, quiet = TRUE, tz = "UTC"))
  return(all(!is.na(parsed)))
}
