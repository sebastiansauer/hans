# import data

library(yaml)
library(tidyverse)
library(jsonlite)

Sys.setenv("VROOM_CONNECTION_SIZE" = 1e6)



paths <- read_yaml("config.yaml")


data_files_all <- list.files(path = paths$data_23ws,
                             full.names = TRUE,
                             recursive = TRUE)
# names(data_files_all) <- 
#   str_extract_all(data_files_all,
#                   pattern = "[^/]+$") |> 
#   unlist()






d_raw <- 
  data_files_wo_dups |> 
  map(import_data) |> 
  list_rbind()


d_raw2 <-
  d_raw |> 
  deselect_empty_cols()


d_raw3 <-
  d_raw2 |> 
  mutate(across(everything(), parse_number))

d_raw3 <-
  d_raw |> 
  select(1:100)

d_raw_23ws <- d_raw3

save(d_raw_23ws, file = "d_raw-23ws.rda")

out <-
d_raw_23ws |> 
  select(where(is.numeric))



