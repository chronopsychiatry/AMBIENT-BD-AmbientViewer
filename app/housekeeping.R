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
  # Calculate the time until midnight
  now <- Sys.time()
  midnight <- as.POSIXct(format(now + 86400, "%Y-%m-%d 00:00:00"), tz = Sys.timezone())
  delay <- as.numeric(difftime(midnight, now, units = "secs"))

  # Schedule the task
  later::later(function() {
    clear_log_file(log_file)
    schedule_log_clearing(log_file) # Reschedule for the next day
  }, delay)
}
