check_unique_ids <- function(df) {
  all_ids_unique <- length(df$idvisit) == length(unique(df$idvisit))
  if (!all_ids_unique) {
    abort("ids are not unique")
  }
  df
}
