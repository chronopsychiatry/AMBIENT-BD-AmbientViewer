clear_log_file <- function(log_file) {
  old_log <- paste0(log_file, ".old")
  if (file.exists(old_log)) {
    file.remove(old_log)
  }

  if (file.exists(log_file)) {
    file.rename(log_file, old_log)
  }

  logging::loginfo("Log file cleared.")
}
