# Ambient Viewer Changelog

## 0.0.2 (in dev)

### New

- All functions that accept sessions or epochs dataframes now take an optional `col_names` argument that can be used to override the default column names. Example:

```r
filtered_sessions <- example_sessions |> remove_sessions_no_sleep(col_names = list(sleep_period = "time_asleep"))
```

- Added filtering options for:
  - Age
  - Sex
  - Subject ID
- These sliders are shown dynamically: they will only appear in the app if the relevant column is available in the data
- The corresponding R filtering functions are:
  - `filter_by_age_range`
  - `select_subjects`
  - `filter_by_sex`

## 0.0.1 (06/05/2025)

### New

- Updated the Sleep clock to show the duration of sleep sessions
- Added "Sleep onset & wakeup", a horizontal bar graph that shows sleep onset and wakeup times aggregated either by night, days of the week, or weekend/weekday.
- Added an option to export plots in PDF format

### Fixed

- Defined better presets for dimensions of exported plots
- Fixed a timezone issue that was sometimes causing sleep onset and wakeup from sessions to be shifted by 1 hour
