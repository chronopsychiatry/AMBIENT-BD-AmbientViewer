library(later)

clear_log_file <- function(log_file) {
  # Delete previous old log
  old_log <- paste0(log_file, ".old")
  if (file.exists(old_log)) {
    file.remove(old_log)
  }

  # Store current log as .old
  if (file.exists(log_file)) {
    file.rename(log_file, old_log)
  }

  # Start new current log
  logging::loginfo("Log file cleared.")
}

schedule_log_clearing <- function(log_file) {
  now <- Sys.time()
  # Calculate next midnight with full precision
  tomorrow <- as.Date(now) + 1
  midnight <- as.POSIXct(paste0(tomorrow, " 00:00:00"), tz = Sys.timezone())
  delay <- as.numeric(difftime(midnight, now, units = "secs"))
  # Add a small buffer to avoid scheduling at exactly the same time
  delay <- delay + 0.01

  later::later(function() {
    clear_log_file(log_file)
    schedule_log_clearing(log_file)
  }, delay)
}
