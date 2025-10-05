compute_prop_eventcategory_per_uni_course <- function(
  data_separated_filtered_date_uni_course,
  group_var = university
) {
  data_separated_filtered_date_uni_course |>
    select(
      university,
      course,
      type,
      value,
      date_time = actiondetails_0_timestamp
    ) |>
    filter(type == "eventcategory") |>
    drop_na() |>

    mutate(month = lubridate::floor_date(date_time, unit = "month")) |>

    # Count all events by university and value
    group_by({{ group_var }}, value) |>
    summarise(n = n(), .groups = "drop") |>

    # Calculate total events per university for proper proportions
    group_by({{ group_var }}) |>
    mutate(total_n = sum(n)) |>

    # Keep only top 5 most frequent values per university
    slice_max(n = 5, order_by = n, with_ties = FALSE) |>

    # Calculate correct proportions using the total
    mutate(prop = n / total_n) |>
    ungroup() |>

    # Clean up factors and ordering
    mutate(
      value = fct_drop(factor(value))
    )
}
