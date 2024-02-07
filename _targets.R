library(targets)

# packages available for all targets:
tar_option_set(packages = c("dplyr", "purrr", "readr"))

# source funs:
funs_files <- list.files(path = "funs", pattern = "\\.R", full.names = TRUE)
lapply(X = funs_files, FUN = source)

# targets, ie., steps to be computed:
list(
  tar_target(data_path_raw, read_yaml("config.yaml")$data_23ws, 
             packages = "yaml"),
  tar_target(data_files_list, list.files(path = data_path_raw,
                                         full.names = TRUE,
                                         recursive = TRUE), format = "file"),
  tar_target(data_files_dupes_excluded, exclude_dupes(data_files_list)),
  tar_target(data_files_no_json, exclude_filetype(data_files_dupes_excluded, "json")),
  tar_target(data_imported, 
             data_files_no_json |> 
               map(import_data) |> 
               list_rbind(), packages = c("lubridate", "stringr")),
  tar_target(data_wo_empty_cols, 
             remove_empty(data_imported, which = c("rows", "cols")), 
             packages = "janitor"),
  tar_target(data_dttm_cols_repaired,
             repair_dttm_cols(data_wo_empty_cols),
             packages = c("lubridate", "dplyr"))  

)