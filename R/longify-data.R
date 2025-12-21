longify_data <- function(data, no.na = TRUE) {
  #' pivot the data longer
  #' params data the input data (wider format)
  #' returns data frame in long format

  assert_that(length(data$idvisit) == length(unique(data$idvisit)))

  vars_to_pivot <- grep("actiondetails_", names(data), value = TRUE)

  DT <- as.data.table(data)
  out <- melt(DT,
    id.vars = c("idvisit", "fingerprint"),
    measure.vars = vars_to_pivot,
    variable.name = "variable",
    value.name = "value"
  )

  # optional - rm missing values ("no NA"):
  if (no.na && "value" %in% names(out)) {
    out <- out[complete.cases(out) & value != ""]
  }

  tibble::as_tibble(out)
}
