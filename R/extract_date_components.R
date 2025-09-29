extract_date_components <- function(d, date_time_var) {
  # Convert column name to symbol for proper evaluation
  date_col <- enquo(date_time_var)

  # Check if the specified column exists
  if (!rlang::as_name(date_col) %in% names(d)) {
    stop("Column '", rlang::as_name(date_col), "' not found in data frame")
  }

  # Check if the column is POSIXct
  if (!inherits(d[[rlang::as_name(date_col)]], "POSIXct")) {
    stop("Column '", rlang::as_name(date_col), "' must be a POSIXct object")
  }

  # Extract components
  result <-
    d |>
    mutate(
      month = month(!!date_col),
      week = week(!!date_col),
      floor_date_week = floor_date(!!date_col, "week"),
      floor_date_month = floor_date(!!date_col, "month"),
      year = year(!!date_col)
    )

  return(result)
}
