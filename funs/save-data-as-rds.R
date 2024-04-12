
save_data_as_rds <- function(data_list){
  map(names(data_list),
      ~saveRDS(data_list[[.x]], 
               paste0(read_yaml("config.yaml")$data_out, "/", .x, ".rds")))
}