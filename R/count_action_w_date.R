count_action_w_date <- function(d = data_separated_filtered, idvar = idvisit) {
  # count rows per visit (WIDE format) plus the date/time of the start of this visit:

  d |>
    mutate(date_time = ymd_hms(value)) |>
    group_by({{ idvar }}) |>
    summarise(
      nr_max = max(nr),
      date_time_start = min(date_time, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      month_of_visit = month(date_time_start, label = TRUE),
      month_date = floor_date(date_time_start, unit = "month"),
      week_date = floor_date(date_time_start, unit = "week"),
      week_of_visit = week(date_time_start),
      year_of_visit = year(date_time_start),
      hour_of_visit_start = hour(date_time_start),
    )
}
