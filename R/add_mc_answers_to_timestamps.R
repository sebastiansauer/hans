add_timestamps_to_mc_answers <- function(data) {
  multiple_choice_answer_selected_with_timestamp <-
    data |>
    select(idvisit, timestamp) |>
    left_join(n_mc_answers_selected, by = "idvisit") |>
    drop_na() |>
    select(idvisit, timestamp, nr)

  multiple_choice_answer_selected_with_timestamp
}
