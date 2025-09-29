import_and_bind_data <- function(d) {
  d |>
    map(import_data) |>
    rbindlist(fill = TRUE)
}
