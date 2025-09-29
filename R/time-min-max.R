time_min_max <- function(data) {
  data |>
    filter(type == "timestamp") |>
    group_by(idvisit) |>
    mutate(type = as.character(type)) |>
    mutate(time = parse_date_time(value, "ymd HMS")) |>
    summarise(
      time_min = min(time),
      time_max = max(time),
      time_diff = time_max - time_min
    )
}
