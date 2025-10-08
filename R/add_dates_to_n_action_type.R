add_dates_to_n_action_type <- function(
  data = n_action_type,
  data_time = time_visit_wday
) {
  n_action_type_per_month <-
    data |>
    select(nr, idvisit, category) |>
    ungroup() |>
    left_join(data_time |> ungroup()) |>
    select(-c(dow, hour, nr)) |>
    drop_na() |>
    #mutate(month_start = floor_date(date_time, "month")) |>
    count(month_start, category)
}
