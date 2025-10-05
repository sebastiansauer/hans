add_date_uni_course_to_long_data <- function(long_data, date_uni_course) {
  setDT(long_data)
  setDT(date_uni_course)
  setkey(date_uni_course, "idvisit")

  # 1. Convert the factor column (x.idvisit) to an integer.
  # Factors store underlying values as integers, so we must first convert it to character
  # to avoid getting the factor level index, and then to integer.
  long_data[, idvisit := as.integer(as.character(idvisit))]
  date_uni_course[, idvisit := as.integer(idvisit)]

  # Join: [i, j]
  # DT[i] means join DT with i.
  # Left join syntax: A[B] where B is the "lookup" table (with the key set)

  # This performs the join:
  data_joined <- date_uni_course[
    long_data,
    on = "idvisit"
  ]

  return(data_joined)
}
