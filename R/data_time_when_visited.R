when_visited <- function(data, idvar) {
  data |>
    dplyr::filter(type == "timestamp") |>
    dplyr::group_by({{ idvar }}) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      type = as.factor(type),
      date_time = lubridate::parse_date_time(value, "ymd HMS"),
      dow = lubridate::wday(date_time, week_start = 1),
      hour = lubridate::hour(date_time)
    ) |>
    dplyr::select({{ idvar }}, dow, hour, date_time) |>
    ungroup()
  # dplyr::group_by({{idvar}}, dow, hour)
}
