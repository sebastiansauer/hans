#' Get timestamp (first) for each idvisit

idvar_timestamp <- function(data, idvar = idvisit) {
  #' param data data,long
  #' return df with n_unique(idivisit) rows, vars: idivisit, timestamp

  data_separated_filtered_timestamp <-
    data |>
    filter(str_detect(type, "timestamp")) |>
    select(-type) |>
    mutate(timestamp = ymd_hms(value)) |>
    group_by({{ idvar }}) |>
    filter(timestamp == min(timestamp)) |>
    slice_head(n = 1) |>
    ungroup()

  data_separated_filtered_timestamp
}

