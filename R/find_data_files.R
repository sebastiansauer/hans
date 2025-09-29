find_data_files <- function(config) {
  list.files(
    path = config$data, # all SEMESTERs
    full.names = TRUE,
    pattern = config$data_raw_pattern,
    recursive = TRUE
  )
}
