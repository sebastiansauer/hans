# MASTER PIPELINE HANS LOG DATA ANALYSIS
# author: Sebastian Sauer


# setup -------------------------------------------------------------------
library(targets)
library(dplyr)
library(tarchetypes)
#library(crew)

# packages available for all targets:
tar_option_set(
  packages = c("dplyr", "purrr", "readr", "tidyr")
  )

# set options:
options(lubridate.week.start = 1)

# source funs:
funs_files <- list.files(
  path = "funs", pattern = "\\.R", full.names = TRUE)
lapply(X = funs_files, FUN = source)


# import data -------------------------------------------------------------

# targets, ie., steps to be computed:
list(
  # read data path as saved in config.yaml:
  tar_target(config_file, "config.yaml", 
             format = "file"),  # watch config file for changes
  tar_target(config, read_yaml(config_file), 
             packages = "yaml"),
  tar_target(data_files_list, 
             list.files(path = config$data_24ss,  # THIS SEMSTER
                        full.names = TRUE,
                        pattern = config$data_raw_pattern,
                        recursive = TRUE), 
             format = "file"),  # watch data source files
  
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
               rbindlist(fill = TRUE), 
             packages = c("lubridate", "stringr", "dplyr", "data.table")),
  
  
  

# prep data ---------------------------------------------------------------
  
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
             data_no_img_cols |> mutate(across(everything(), as.character)),
             packages = "collapse"),
  
  # transform all cols to factor: 
  tar_target(data_all_fct,
             data_all_chr |> mutate(across(everything(), as.factor)),
             packages = "collapse"),
  
  # exclude non-participants:
  tar_target(data_users_only,
             data_all_chr |> 
               filter(!str_detect(actiondetails_0_url, 
                                  "=admin|=developer|=lecturer")) |> 
               filter(!str_detect(actiondetails_1_subtitle, 
                                  "=admin|=developer|=lecturer")),
             packages = "stringr"),




# tinify data -------------------------------------------------------------

  tar_target(data_less_cols,
             data_users_only |> 
               select(-c(contains("idpageview"), 
                         contains("pretty"),
                         contains("pageviewposition"),
                         contains("pageid"),
                         #select(-matches("\\w+_(?!0)\\d+_timestamp")),
                         contains("timespent")))),


  
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

tar_target(data_wide_slim,
           data_less_cols |> 
           get_vars(vars = c("idvisit", 
                             grep("actiondetails_", names(data_less_cols),
                                  value = TRUE))),
           packages = "collapse"),



# pivot longer ------------------------------------------------------------

  tar_target(data_long,
             data_wide_slim |> 
               pivot(ids = "idvisit",
                     how = "longer",
                     check.dups = TRUE, 
                     factor = FALSE),
             packages = "collapse"),
  
  # pivot longer to get a handle on the number of cols per login:
  # tar_target(data_long2,
  #            data_all_chr |> longify_data(),
  #            packages = "collapse"),
  
  tar_target(data_long_nona,
             data_long |> drop_na()),  # drop rows with missing data

  # slimify and separate:
  tar_target(data_slim,
             slimify_nona_data(data_long_nona), 
             packages = c("dplyr", "tidyr", "collapse")),



# Mini slim data for debugging --------------------------------------------




  tar_target(data_user1_long_slim,
              slimify_nona_data(data_user1_long), 
             packages = c("dplyr", "tidyr", "collapse")),
  tar_target(data_slim_head,
             data_slim[1:1e5,]),





# count stuff per visit -------------------------------------------------

  # count rows per visit (n):
  tar_target(count_action,
             data_slim |>
               group_by(idvisit) |>
               # "nr" is the id of the action of this visit:
               summarise(n_max = max(nr))), 
  
  # compute time variables per visit:
  tar_target(time_spent,
             data_slim |> diff_time(),
             packages = "lubridate"),
  tar_target(time_minmax,
             data_slim |> time_min_max(),
             packages = "lubridate"),
  tar_target(time_duration,
             data_all_chr %>% 
               select(idvisit, visitduration) %>% 
               mutate(visitduration_sec = as.numeric(visitduration)) %>% 
               select(-visitduration)),

  # count time of visit per weekday:
  tar_target(time_visit_wday,
             data_slim |> when_visited(), 
             packages = c("collapse", "lubridate")),
  tar_target(time_since_last_visit,
             data_all_fct |> 
               select(idvisit, dayssincelastvisit)
             ),
  
  # count action categories per visit:
  tar_target(count_action_type,
             count_user_action_type(data_slim), packages = "stringr"),
  
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




# render report in Quarto -------------------------------------------------


  
  # render report:
  tar_quarto(report01, "report01.qmd")
  
  # export processed data to disk as RDS file:
  # tar_target(data_to_be_exported, 
  #            list(
  #              time_spent = time_spent, 
  #              data_slim = data_slim, 
  #              count_action = count_action,
  #              data_little = data_little,
  #              data_all_chr = data_all_chr,
  #              data_all_fct = data_all_fct,
  #              data_user1 = data_user1,
  #              data_user1_long = data_user1_long,
  #              data_little_long = data_little_long,
  #              data_slim_head = data_slim_head)),
  # tar_target(export_data, 
  #            save_data_as_rds(
  #              data_to_be_exported, config_file),
  #            packages = c("purrr", "yaml"))
            
)
