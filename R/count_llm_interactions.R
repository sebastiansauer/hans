count_llm_interactions <- function(d) {
  d |>
    filter(type == "eventcategory" | type == "timestamp") |>
    add_dates() |>
    group_by(year_month) |>
    count(llm_interaction = str_detect(value, "llm"))
}
