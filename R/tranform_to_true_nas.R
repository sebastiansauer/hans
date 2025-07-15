transform_to_true_NAs <- function(df) {
  df[df == ""] <- NA
  return(df)
}
