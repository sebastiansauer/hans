count_llm_interactions_add_context <- function(
  data_separated_filtered,
  time_spent_w_course_university
) {
  d_n_interactions_w_llm_course <-
    data_separated_filtered |>
    filter(type == "eventcategory") |>
    filter(str_detect(value, "llm")) |>
    group_by(idvisit) |>
    slice_head(n = 1)

  d_n_interactions_w_llm_course_date_course_uni <-
    d_n_interactions_w_llm_course |>
    left_join(
      time_spent_w_course_university |> mutate(idvisit = as.integer(idvisit)),
      by = "idvisit"
    )
}
