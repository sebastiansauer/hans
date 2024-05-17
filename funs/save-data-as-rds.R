
save_data_as_rds <- function(data_list, config_file){
  
  map(names(data_list),
      ~saveRDS(data_list[[.x]], 
               paste0(read_yaml(config_file)$data_out, "/", .x, ".rds")))
}