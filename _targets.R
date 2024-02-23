library(targets)
library(dplyr)
library(tarchetypes)

# packages available for all targets:
tar_option_set(packages = c("dplyr", "purrr", "readr", "tidyr"))

# source funs:
funs_files <- list.files(path = "funs", pattern = "\\.R", full.names = TRUE)
lapply(X = funs_files, FUN = source)

# targets, ie., steps to be computed:
list(
  # read data path:
  tar_target(data_path_raw, read_yaml("config.yaml")$data_23ws, 
             packages = "yaml"),
  tar_target(data_files_list, list.files(path = data_path_raw,
                                         full.names = TRUE,
                                         recursive = TRUE), format = "file"),
  
  # exclude duplicate data files:
  tar_target(data_files_dupes_excluded, exclude_dupes(data_files_list)),
  
  # exclude json files:
  tar_target(data_files_no_json, exclude_filetype(data_files_dupes_excluded, "json")),
  
  # import data files and bind in one df:
  tar_target(data_imported, 
             data_files_no_json |> 
               map(import_data) |> 
               list_rbind(), packages = c("lubridate", "stringr")),
  
  # remove empty cols:
  tar_target(data_wo_empty_cols, 
             remove_empty(data_imported, which = c("rows", "cols")), 
             packages = "janitor"),
  
  # repair "broken" cols:
  tar_target(data_dttm_cols_repaired,
             repair_dttm_cols(data_wo_empty_cols),
             packages = c("lubridate", "dplyr")),
  
  # rm constant cols:
  tar_target(data_wo_constant_cols, 
             remove_constant(data_dttm_cols_repaired, na.rm = TRUE),
             packages = "janitor"),
  
  # rm image cols:
  tar_target(data_no_img_cols,
             data_wo_constant_cols |> 
               select(-contains("svg")) |> 
               select(-contains("icon"))),
  
  # transform all columns to character:
  tar_target(data_all_chr, 
             data_no_img_cols |> mutate(across(everything(), as.character))),
  
  # transform all cols to factor: 
  tar_target(data_all_fct,
             data_all_chr |> mutate(across(everything(), factor))),
  
  # exclude non-participants:
  tar_target(data_users_only,
             data_all_chr |> 
               filter(!str_detect(actiondetails_0_url, "=admin|=developer|=lecturer")) |> 
               filter(!str_detect(actiondetails_1_subtitle, "=admin|=developer|=lecturer"))),
  
  # prepare little data set for quick checking:
  tar_target(data_little,
             data_users_only[1:50, 1:50] |> write_csv("obj/data_little.csv")),
  
  tar_target(data_user1,
             data_all_chr |> 
             filter(row_number() == 1)),
  
  # tinify data set for quicker debugging:
  tar_target(data_little_long,
             data_little |> 
               get_vars(vars = c("idvisit", grep("actiondetails_", names(data_little), value = TRUE))) |> 
               pivot(ids = "idvisit",
                     how = "longer",
                     check.dups = TRUE, 
                     factor = FALSE),
             packages = "collapse"),
 
  tar_target(data_user1_long,
             longify_data(data_user1),
             packages = "collapse"),
  
  
  # pivot longer to get a handle on the number of cols per login:
  tar_target(data_long,
             data_all_chr |> 
               get_vars(vars = c("idvisit", grep("actiondetails_", names(data_all_chr),
                                                 value = TRUE))) |> 
               pivot(ids = "idvisit",
                     how = "longer",
                     check.dups = TRUE, 
                     factor = FALSE),
             packages = "collapse"),

  # slimify and separate:
  tar_target(data_slim,
             slimify_nona_data(data_long), 
             packages = c("dplyr", "tidyr", "collapse")),
  tar_target(data_user1_long_slim,
              slimify_nona_data(data_user1_long), 
             packages = c("dplyr", "tidyr", "collapse")),

  # count rows per visit:
  tar_target(count_action,
             data_slim |>
               group_by(idvisit) |>
               summarise(n_max = n())),
  
  tar_target(time_spent,
             data_slim |> diff_time(),
             packages = "lubridate"),
  
  tar_target(count_action_type,
             count_user_action_type(data_slim), packages = "stringr"),
  
  # render report:
  tar_quarto(report, "report.qmd")

)
