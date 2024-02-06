
# 
# import_data <- function(file){
#   
#   if(file_suffix(file) == "csv") out <- readr::read_csv(file)
#   if(file_suffix(file) == "tsv") out <- readr::read_tsv(file)
#   if(file_suffix(file) == "json") out <- jsonlite::fromJSON(file)
# }



import_data <- function(file){
  
  d_raw <- switch(file_suffix(file),
        "csv" = readr::read_csv(file),
        "tsv" = readr::read_tsv(file),
        "json" = jsonlite::fromJSON(file),
        stop("invalid file suffix"))
  
  out <-
    d_raw |>
    map_df(as.character) |> 
    mutate(file_id = basename(file))
  
}



# dttm    (1): lastActionDateTime
# date    (1): serverDate
# time    (3): serverTimePretty, serverTimePrettyFirstActio...