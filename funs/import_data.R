



import_data <- function(file, verbose = TRUE){
  
  print(paste0("Now processing: ", file))
  
  d_raw <- switch(file_suffix(file),
        "csv" = fread(file, sep = ","),
        "tsv" = fread(file, sep = "\t"),
        "json" = jsonlite::fromJSON(file),
        stop("invalid file suffix"))
  
  names(d_raw) <- tolower(names(d_raw))
  
  date_cols <- names(d_raw)[str_detect(names(d_raw), "date")]
  time_cols <- names(d_raw)[str_detect(names(d_raw), "time")]
  
  cols_as_chr <- c("operatingSystemVersion", "idsite") |> tolower()

  
  out <-
    d_raw |>
    #map_df(as.character) |> 
    mutate(file_id = basename(file)) |> 
    mutate(across(one_of(date_cols), as.character)) |> 
    mutate(across(one_of(time_cols), as.character)) |> 
    mutate(across(one_of(cols_as_chr), as.character)) |> 
    mutate(across(where(is.numeric), as.character))
}



# dttm    (1): lastActionDateTime
# date    (1): serverDate
# time    (3): serverTimePretty, serverTimePrettyFirstActio...