library(targets)

# packages available for all targets:
tar_option_set(packages = c("dplyr", "purrr", "stringr", "readr"))

# source funs:
funs_files <- list.files(path = "funs", pattern = "\\.R", full.names = TRUE)
lapply(X = funs_files, FUN = source)

# targets, ie., steps to be computed:
list(
  tar_target(data_path_raw, yaml::read_yaml("config.yaml")$data_23ws),
  tar_target(data_files_list, list.files(path = data_path_raw,
                                         full.names = TRUE,
                                         recursive = TRUE), format = "file"),
  tar_target(data_files_dupes_excluded, exclude_dupes(data_files_list)),
  tar_target(data_imported, 
             data_files_dupes_excluded |> 
               map(import_data) |> 
               list_rbind()),
  tar_target(data_wo_empty_cols, 
             deselect_empty_cols(data_imported)),
  tar_target(data_w_num_cols,
             data_wo_empty_cols |> mutate(across(everything(), parse_number)))
  
)