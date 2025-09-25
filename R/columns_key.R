# Sessions ----

.sessions_col_somnofy_v2 <- list(
  id = "id",
  subject_id = "subject_id",
  sex = NULL,
  birth_year = NULL,
  device_id = "device_serial_number",
  session_start = "session_start",
  session_end = "session_end",
  time_at_sleep_onset = "time_at_sleep_onset",
  time_at_midsleep = "time_at_midsleep",
  time_at_wakeup = "time_at_wakeup",
  sleep_onset_latency = "sleep_onset_latency",
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
  time_at_midsleep = "time_at_midsleep",
  time_at_wakeup = "time_at_wakeup",
  sleep_onset_latency = "sleep_onset_latency",
  sleep_period = "sleep_period",
  time_in_bed = "time_in_bed",
  is_workday = "is_workday",
  night = "night"
)

.sessions_col_ggir <- list(
  id = "window_number",
  subject_id = "ID",
  sex = NULL,
  birth_year = NULL,
  device_id = NULL,
  session_start = "session_start",
  session_end = "session_end",
  time_at_sleep_onset = "sleeponset_ts",
  time_at_midsleep = "midsleep_ts",
  time_at_wakeup = "wakeup_ts",
  sleep_onset_latency = NULL,
  sleep_period = "sleep_period",
  time_in_bed = "time_in_bed",
  is_workday = "is_workday",
  night = "calendar_date"
)

.sessions_col_edf <- list(
  id = "session_id",
  subject_id = NULL,
  sex = NULL,
  birth_year = NULL,
  device_id = "device_id",
  session_start = "startTime",
  session_end = "endTime",
  time_at_sleep_onset = "startTime",
  time_at_midsleep = "midsleep",
  time_at_wakeup = "endTime",
  sleep_onset_latency = NULL,
  sleep_period = "sleep_period",
  time_in_bed = "sleep_period",
  is_workday = "is_workday",
  night = "night"
)

.sessions_col_none <- list(
  id = "id_default",
  subject_id = NULL,
  sex = NULL,
  birth_year = NULL,
  device_id = NULL,
  session_start = NULL,
  session_end = NULL,
  time_at_sleep_onset = NULL,
  time_at_midsleep = NULL,
  time_at_wakeup = NULL,
  sleep_onset_latency = NULL,
  sleep_period = NULL,
  time_in_bed = NULL,
  is_workday = NULL,
  night = NULL
)

# Epochs ----

.epochs_col_somnofy_v2 <- list(
  timestamp = "timestamp",
  session_id = "session_id",
  signal_quality = "signal_quality_mean",
  sleep_stage = "sleep_stage",
  night = "night",
  is_asleep = "is_asleep"
)

.epochs_col_somnofy_v1 <- list(
  timestamp = "timestamp",
  session_id = "session_id",
  signal_quality = "signal_quality_mean",
  sleep_stage = "sleep_stage",
  night = "night",
  is_asleep = "is_asleep"
)

.epochs_col_ggir <- list(
  timestamp = "timenum",
  session_id = "window",
  signal_quality = NULL,
  sleep_stage = "class_id",
  night = "night",
  is_asleep = "is_asleep"
)

.epochs_col_none <- list(
  timestamp = NULL,
  session_id = NULL,
  signal_quality = NULL,
  sleep_stage = NULL,
  night = NULL,
  is_asleep = NULL
)
