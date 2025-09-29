longify_data <- function(data, no.na = TRUE) {
  #' returns long dataframe with three columns: idvar, variable value

  assert_that(length(data$idvisit) == length(unique(data$idvisit)))

  # data.table for speed:
  dt <- as.data.table(data)
  data_long_dt <- melt(
    dt,
    id.vars = c("idvisit", "fingerprint"), # two id variables
    measure.vars = patterns("^actiondetails_"),
    variable.name = "variable",
    value.name = "value"
  )
  data_long <- tibble(data_long_dt)

  # optional - rm missing values ("no NA"):
  if (no.na) {
    data_long <- data_long[complete.cases(data_long) & data_long$value != "", ]
  }

  data_long$variable <- as.factor(data_long$variable)

  return(data_long)
}
