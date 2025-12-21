#' Select and prepare the relevant columns

slimify_nona_data <- function(data_long) {
  data_separated <-
    data_long |>
    # head(1e4) |>
    # prepare to count the number of things a user does:
    select(variable, value, idvisit, fingerprint) |>
    separate(variable, sep = "_", into = c("constant", "nr", "type")) |>
    select(-c(constant)) |>
    mutate(
      nr = as.integer(nr),
      idvisit = as.integer(idvisit),
      type = as.factor(type),
      value = as.factor(value)
    ) |>
    # ungroup() |>
    arrange(idvisit, nr)

  return(data_separated)
}
