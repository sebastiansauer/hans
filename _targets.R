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
  # read data path as saved in config.yaml:
  tar_target(paths, read_yaml("config.yaml"), packages = "yaml"),
  tar_target(data_path_raw, paths$data_24ss, 
             packages = "yaml"),
  tar_target(data_files_list, list.files(path = data_path_raw,
                                         full.names = TRUE,
                                         recursive = TRUE), format = "file"),
  
  # exclude duplicate data files:
  tar_target(data_files_dupes_excluded, 
             exclude_dupes(data_files_list)),
  
  # exclude json files:
  tar_target(data_files_no_json, 
             exclude_filetype(data_files_dupes_excluded, "json")),
  
  # import data files and bind in one df:
  tar_target(data_imported, 
             data_files_no_json |> 
               map(import_data) |> 
               list_rbind(), packages = c("lubridate", "stringr", "data.table")),
  
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
               filter(!str_detect(actiondetails_0_url, 
                                  "=admin|=developer|=lecturer")) |> 
               filter(!str_detect(actiondetails_1_subtitle, 
                                  "=admin|=developer|=lecturer"))),
  
  # prepare little data set for quick checking:
  tar_target(data_little,
             data_users_only[1:50, 1:50]),
  
  tar_target(data_user1,
             data_all_chr |> 
             filter(row_number() == 1)),
  
  # tinify data set for quicker debugging:
  tar_target(data_little_long,
             data_little |> longify_data(),  # transform into long format
             packages = "collapse"),
 
  tar_target(data_user1_long,  # transform into long format
             longify_data(data_user1),
             packages = "collapse"),
  
  # pivot longer to get a handle on the number of cols per login:
  tar_target(data_long,
             data_all_chr |> longify_data(),
             packages = "collapse"),
  
  tar_target(data_long_nona,
             data_long |> drop_na()),  # drop rows with missing data

  # slimify and separate:
  tar_target(data_slim,
             slimify_nona_data(data_long_nona), 
             packages = c("dplyr", "tidyr", "collapse")),
  tar_target(data_user1_long_slim,
              slimify_nona_data(data_user1_long), 
             packages = c("dplyr", "tidyr", "collapse")),
  tar_target(data_slim_head,
             data_slim[1:1e5,]),

  # count rows per visit:
  tar_target(count_action,
             data_slim |>
               group_by(idvisit) |>
               summarise(n_max = max(nr))),
  
  # compute time spent per visit:
  tar_target(time_spent,
             data_slim |> diff_time(),
             packages = "lubridate"),
  tar_target(time_minmax,
             data_slim |> time_min_max(),
             packages = "lubridate"),
  
  # count action categories per visit:
  tar_target(count_action_type,
             count_user_action_type(data_slim), packages = "stringr"),
  
  # count time of visit per weekday:
  tar_target(time_visit_wday,
             data_slim |> when_visited(), 
             packages = c("collapse", "lubridate")),
  
  # count AI transcript clicks per month:
  tar_target(ai_transcript_clicks_per_month,
             data_slim |> 
               filter(type == "subtitle" | type == "timestamp") |> 
               filter(!is.na(value) & value != "")  |> 
               ftransform(date_time = parse_date_time(value, "ymd HMS")) |> 
               add_dates() |> 
               group_by(year_month) |> 
               count(click_transcript_word = str_detect(value, "click_transcript_word")),
             packages = c("lubridate", "collapse", "stringr")),
  
  
  
  # render report:
  tar_quarto(report01, "report01.qmd"),
  
  
  # export processed data to disk as RDS file:
  tar_target(data_to_be_exported, list(time_spent = time_spent, 
                                       data_slim = data_slim, 
                                       count_action = count_action,
                                       data_little = data_little,
                                       data_all_chr = data_all_chr,
                                       data_user1 = data_user1,
                                       data_user1_long = data_user1_long,
                                       data_little_long = data_little_long,
                                       data_slim_head = data_slim_head)),
  tar_target(export_data, save_data_as_rds(data_to_be_exported),
             packages = c("purrr", "yaml"))
            
)
