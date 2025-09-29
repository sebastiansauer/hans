count_mc_answers <- function(data_separated_filtered, idvar = idvisit) {
  multiple_choice_answer_selected <-
    data_separated_filtered |>
    # the number of rows is the the sum of how often "Multiple choice answer selected" appeared:
    filter(str_detect(value, "Multiple choice answer selected"))

  data_separated_filtered_timestamp <-
    data_separated_filtered |>
    filter(str_detect(type, "timestamp")) |>
    select(-type) |>
    mutate(timestamp = ymd_hms(value)) |>
    group_by({{ idvar }}) |>
    filter(timestamp == min(timestamp)) |>
    slice_head(n = 1) |>
    ungroup()

  # merge with timestamps the "miltiple choice ...":
  multiple_choice_answer_selected_with_timestamp <-
    data_separated_filtered_timestamp |>
    select({{ idvar }}, timestamp) |>
    full_join(multiple_choice_answer_selected, by = "idvisit") |>
    drop_na() |>
    select({{ idvar }}, timestamp, nr)

  multiple_choice_answer_selected_with_timestamp
}
