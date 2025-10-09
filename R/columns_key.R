# Long names ----
.sessions_long <- list(
  id = "Session ID",
  subject_id = "Subject ID",
  sex = "Sex",
  birth_year = "Birth Year",
  device_id = "Device Serial Number",
  session_start = "Session Start",
  session_end = "Session End",
  time_at_sleep_onset = "Time at Sleep Onset",
  time_at_midsleep = "Time at Midsleep",
  time_at_wakeup = "Time at Wakeup",
  sleep_onset_latency = "Sleep Onset Latency",
  sleep_period = "Sleep Period",
  time_in_bed = "Time in Bed",
  is_workday = "Workdays/Free Days",
  night = "Night"
)

.epochs_long <- list(
  timestamp = "Timestamp",
  session_id = "Session ID",
  signal_quality = "Signal Quality",
  sleep_stage = "Sleep Stage",
  night = "Night",
  is_asleep = "Asleep/Awake"
)

# Help tooltip texts ----
.sessions_help <- list(
  id = "Unique identifier for each session.",
  subject_id = "Unique identifier for each subject.",
  sex = NULL,
  birth_year = NULL,
  device_id = "Unique identifier for the recording device.",
  session_start = paste0("Start time of the session (YYYY-MM-DD HH:MM:SS).",
                         " If \"night\" is not provided, it will be calculated from session start."),
  session_end = "End time of the session (YYYY-MM-DD HH:MM:SS).",
  time_at_sleep_onset = "Time at Sleep Onset (YYYY-MM-DD HH:MM:SS).",
  time_at_midsleep = "Time at Midsleep (YYYY-MM-DD HH:MM:SS).",
  time_at_wakeup = "Time at Wakeup (YYYY-MM-DD HH:MM:SS).",
  sleep_onset_latency = "Time between session start and sleep onset (in seconds).",
  sleep_period = "Total time spent asleep during the session (in seconds).",
  time_in_bed = "Total time spent in bed during the session (in seconds).",
  is_workday = "Logical variable indicating if the session is on a workday (TRUE) or weekend (FALSE).",
  night = "Night (from 12pm to 12pm) the session belongs to (YYYY-MM-DD). If not provided, it will be calculated from session_start."
)

.epochs_help <- list(
  timestamp = "Timestamp of the epoch (YYYY-MM-DD HH:MM:SS). If \"night\" is not provided, it will be calculated from timestamp.",
  session_id = "Identifier linking the epoch to a session in the sessions dataframe.",
  signal_quality = "Signal quality of the epoch.",
  sleep_stage = "Sleep stage of the epoch.",
  night = "Night (from 12pm to 12pm) the epoch belongs to (YYYY-MM-DD). If not provided, it will be calculated from timestamp.",
  is_asleep = "Logical variable indicating if the epoch is classified as asleep (TRUE) or awake (FALSE)."
)

# Session formats ----

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
  subject_id = "subject_id",
  sex = NULL,
  birth_year = NULL,
  device_id = NULL,
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
  id = "session_id",
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

# Epoch formats ----

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
