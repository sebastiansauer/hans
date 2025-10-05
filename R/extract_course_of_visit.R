extract_course_role_university_of_visit <- function(d) {
#' Extract course, role, and university from visit data
  #' param d Data frame containing visit data with URLs
  #' return Data frame with extracted course, role, and university information

  d |> # WIDE data such as "data_wide_slim"
    select(
      idvisit,
      fingerprint,
      actiondetails_0_url,
      actiondetails_0_timestamp
    ) |>
    mutate(
      course = str_extract(
        actiondetails_0_url,
        "(?<=\\.student\\.)[a-zA-Z0-9]+"
      )
    ) |>
    mutate(
      university = str_extract(actiondetails_0_url, "(?<=%40)[a-z0-9-]+")
    ) |>
    mutate(role = str_extract(actiondetails_0_url, "(?<=role=)[a-z]+")) |>
    mutate(actiondetails_0_timestamp = ymd_hms(actiondetails_0_timestamp)) |>
    select(-actiondetails_0_url)
}
