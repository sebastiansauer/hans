compute_time_per_course_uni <- function(data, course_and_uni, idvar) {
  data |>
    mutate({{ idvar }} := as.factor({{ idvar }})) |>
    left_join(course_and_uni, by = rlang::as_name(rlang::enquo(idvar))) |>
    extract_date_components(time_min)
}
