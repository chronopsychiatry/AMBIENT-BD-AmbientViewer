.sessions_col_somnofy_v2 <- list(
  id = "id",
  subject_id = "subject_id",
  sex = NULL,
  birth_year = NULL,
  device_id = "device_serial_number",
  session_start = "session_start",
  session_end = "session_end",
  time_at_sleep_onset = "time_at_sleep_onset",
  time_at_wakeup = "time_at_wakeup",
  sleep_period = "sleep_period",
  time_in_bed = "time_in_bed",
  is_workday = "is_workday",
  night = "night"
)

.sessions_col_somnofy_v1 <- list(
  id = "session_id",
  subject_id = "user_id",
  sex = "sex",
  birth_year = "birth_year",
  device_id = NULL,
  session_start = "session_start",
  session_end = "session_end",
  time_at_sleep_onset = "time_at_sleep_onset",
  time_at_wakeup = "time_at_wakeup",
  sleep_period = "sleep_period",
  time_in_bed = "time_in_bed",
  is_workday = "is_workday",
  night = "night"
)

.epochs_col_somnofy_v2 <- list(
  timestamp = "timestamp",
  session_id = "session_id",
  signal_quality = "signal_quality_mean",
  sleep_stage = "sleep_stage",
  night = "night"
)

.epochs_col_somnofy_v1 <- list(
  timestamp = "timestamp",
  session_id = NULL,
  signal_quality = "signal_quality_mean",
  sleep_stage = "sleep_stage",
  night = "night"
)
