longify_data_tidyverse <- function(data, no.na = TRUE) {
  out <-
    data |>
    select(idvisit, starts_with("actiondetails_")) |>
    pivot_longer(-idvisit, names_to = "variable")

  # optional - rm missing values ("no NA"):
  if (no.na) {
    out <-
      out %>%
      filter(complete.cases(.)) |>
      filter(value != "")
  }

  return(out)
}
