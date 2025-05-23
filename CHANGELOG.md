# Ambient Viewer Changelog

## 0.0.3 (in dev)

### New

- Summary outputs from GGIR (part 5) in csv format can be loaded as Sessions

### Fixed

- Setting column names with "Set Session Columns" and "Set Epoch Columns" work correctly
- Loading a CSV file that doesn't fit the pre-set formats doesn't crash the app anymore

## 0.0.2.1 (19/05/2025)

Patch to fix sleep clock display on the shiny server. No visible changes for users.

## 0.0.2 (16/05/2025)

### New

#### Annotation tab

The "Annotation" tab (next to Summary, Compliance and Filtering) allows the user to assign a custom tag to each session. This could be used for example to highlight specific episodes from a sleep journal. The custom annotations will be displayed in the different data tables, and can be used in the figures (see next section).

#### Changable colormaps for the figures

Most figures now have a "Colour by" drop-down menu. Use this menu to change the colours on the figure, for example to colour data according to the custom annotations ("annotation" variable), or by work days vs. weekends ("is_workday").

#### More filtering options

Added filtering options for:

- Age
- Sex
- Subject ID

These sliders are shown dynamically: they will only appear in the app if the relevant column is available in the data.

The corresponding R filtering functions are:

- `filter_by_age_range`
- `filter_by_sex`
- `select_subjects`

#### Manage column names

The app now has "Set session columns" and "Set epoch columns" menus (in the Data input module) that allow setting different column names to the default.

All functions that accept sessions or epochs dataframes now take an optional `col_names` argument that can be used to override the default column names. Example:

```r
filtered_sessions <- example_sessions |> remove_sessions_no_sleep(col_names = list(sleep_period = "time_asleep"))
```

#### New example data

Added example data for the v1 of the Somnofy API (data generated before 2025). Once AmbientViewer is loaded, they are directly accessible via the variables:

- `example_sessions_v1`
- `example_epochs_v1`

## 0.0.1 (06/05/2025)

### New

- Updated the Sleep clock to show the duration of sleep sessions
- Added "Sleep onset & wakeup", a horizontal bar graph that shows sleep onset and wakeup times aggregated either by night, days of the week, or weekend/weekday.
- Added an option to export plots in PDF format

### Fixed

- Defined better presets for dimensions of exported plots
- Fixed a timezone issue that was sometimes causing sleep onset and wakeup from sessions to be shifted by 1 hour
