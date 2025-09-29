count_visitor_interaction_with_llm <- function(d, idvar = idvisit) {
  d |>
    mutate(has_llm = str_detect(value, "llm")) |>
    group_by({{ idvar }}) |>
    mutate(uses_llm = any(has_llm == TRUE)) |>
    filter(type == "timestamp") |>
    add_dates() |>
    filter(date_time == min(date_time)) |>
    ungroup() |>
    select(-c(has_llm))
}
