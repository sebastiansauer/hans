diff_time <- function(data, idvar = idvisit) {
  # compute time variables per visit (WIDE format data):
  data |>
    filter(type == "timestamp") |>
    select({{ idvar }}, value) |>
    group_by({{ idvar }}) |>
    mutate(time = parse_date_time(value, "ymd HMS")) |>
    summarise(
      time_diff = max(time) - min(time),
      time_min = min(time),
      time_max = max(time)
    )
}

# Note: The data set is still grouped by idvisit!
