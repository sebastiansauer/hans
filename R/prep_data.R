prep_data <- function(d) {
  d |>
    transform_to_true_NAs() |>
    remove_empty(which = c("rows", "cols")) |>
    repair_dttm_cols() |>
    remove_constant(na.rm = TRUE) |>
    # select(-contains("svg")) |>
    # select(-contains("icon")) |>
    filter(!str_detect(actiondetails_0_url, "=admin|=developer|=lecturer")) |>
    filter(
      !str_detect(actiondetails_1_subtitle, "=admin|=developer|=lecturer")
    ) |>
    select(
      -c(
        contains("idpageview"),
        contains("pretty"),
        contains("pageviewPosition"),
        contains("pageid"),
        contains("icon"),
        contains("pageTitle"),
        contains("pageIdAction"),
        contains("idpageview"),
        contains("pageLoadTime"),
        contains("_title"),
        contains("_type"), # z.B. "actionDetails_0_type"
        contains("_timeSpent")
      )
    ) |>
    # assign own IDs, as the original ID are *not* unique:
    mutate(idvisit_old = idvisit, idvisit = 1:n()) |>
    select(idvisit, everything())
}
