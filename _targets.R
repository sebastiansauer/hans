# MASTER PIPELINE HANS LOG DATA ANALYSIS
# author: Sebastian Sauer


# setup -------------------------------------------------------------------
library(targets)
library(dplyr)
library(tarchetypes)


# packages available for all targets:
tar_option_set(
  packages = c("dplyr", "purrr", "readr", "tidyr", "collapse")  )

# set options:
options(lubridate.week.start = 1)
#options(collapse_mask = "all") # use collapse for all dplyr operations

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
             find_data_files(config), 
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
             data_imported |> 
               transform_to_true_NAs() |> 
               remove_empty(which = c("rows", "cols")), 
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
             data_no_img_cols |> mutate(across(everything(), as.factor)),
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
  # it appears that "idivisit" is NOT unique :-(


  # make sure the ID is now unique
  tar_target(data_unique_id,
             data_less_cols |> 
               mutate(idvisit_old = idvisit,
                      idvisit = 1:nrow(data_less_cols)) |> 
               select(idvisit, everything())),


  tar_target(test_unique_idvisit,
             check_unique_ids(data_unique_id)),


  
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
 
  tar_target(data_user1_long,  # transform into long format:
             data_user1 |> longify_data(),
             packages = "collapse"),

tar_target(data_wide_slim,
           data_unique_id |> 
             get_vars(vars = c("idvisit", 
                               # get only the "actiondetails_XXX" variables:
                               grep("actiondetails_", names(data_unique_id),
                                    value = TRUE))),
           packages = "collapse"),

  tar_target(data_wide_slim_head,
             data_unique_id[1:100, ]),
  # time tags are still okay: starting with 2023
  


# pivot longer ------------------------------------------------------------

# pivot longer to get a handle on the number of cols per login:
  tar_target(data_long,
             data_unique_id |> longify_data(),
             packages = "collapse"),
  
 
  # tidyverse appears to come the same object, but better double check:
  tar_target(data_long_tidyverse,
             data_wide_slim |> 
               longify_data_tidyverse()),
  
  
  # drop rows with missing data:
  # time tags appear to be wrong! It should start in 2023, not in 2024!
  tar_target(data_long_nona,
             data_long |> drop_na() |> filter(value != "")),  # drop rows with missing data

  # slimify and separate:
  tar_target(data_slim,
             slimify_nona_data(data_long_nona), 
             packages = c("dplyr", "tidyr", "collapse")),


  # filter unneeded rows:
  tar_target(data_slim_filtered,
             data_slim |> 
               filter(!type %in% c("pageloadtime", 
                                  "pageloadtimemilliseconds",
                                  "title",
                                  "type",
                                  "url"))),



# Mini slim data for debugging --------------------------------------------




  tar_target(data_user1_long_slim,
              slimify_nona_data(data_user1_long), 
             packages = c("dplyr", "tidyr", "collapse")),
  tar_target(data_slim_head,
             data_slim[1:1e5,]),
  tar_target(data_slim_filtered_head,
             data_slim_filtered[1:1e5,]),



# count stuff per visit -------------------------------------------------

  # count rows per visit (n):
  tar_target(count_action,
             data_slim_filtered |>
               group_by(idvisit) |>
               # "nr" is the id of the action of this visit:
               summarise(nr_max = max(nr))), 
  
  # compute time variables per visit:
  tar_target(time_spent,
             data_slim_filtered |> diff_time(),
             packages = "lubridate"),
  tar_target(time_minmax,
             data_slim_filtered |> time_min_max(),
             packages = "lubridate"),
  tar_target(time_duration,
             data_all_chr %>% 
               select(idvisit, visitduration) %>% 
               mutate(visitduration_sec = as.numeric(visitduration)) %>% 
               select(-visitduration)),

  # count time of visit per weekday:
  tar_target(time_visit_wday,
             data_slim_filtered |> when_visited(), 
             packages = c("collapse", "lubridate")),
  tar_target(time_since_last_visit,
             data_all_fct |> 
               select(idvisit, dayssincelastvisit)
             ),
  
  # count action categories per visit:
  tar_target(count_action_type,
             count_user_action_type(data_slim_filtered), packages = "stringr"),
  
  # count AI transcript clicks per month:
  tar_target(ai_transcript_clicks_per_month,
             data_slim_filtered |> 
               mutate(clicks_transcript = str_detect(value, "click_transcript_word"))  |> 
               group_by(idvisit) |> 
               mutate(clicks_transcript_any = any(clicks_transcript == TRUE)) |> 
               filter(type == "timestamp") |> 
               add_dates()  |> 
               filter(date_time == min(date_time)) |> 
               ungroup() |> 
               select(-c(clicks_transcript)),
             packages = c("lubridate", "collapse", "stringr")),

  # count interactions with LLM per month:
  tar_target(ai_llm_per_months,
             data_slim_filtered |> 
               filter(type == "eventcategory" | type == "timestamp") |> 
               add_dates() |> 
               group_by(year_month) |> 
               count(llm_interaction = str_detect(value, "llm")),
             packages = c("lubridate", "collapse", "stringr")),


  # count how many visitors interact with the LLM:
  tar_target(idvisit_has_llm, 
             data_slim_filtered |> 
               mutate(has_llm = str_detect(value, "llm"))  |> 
               group_by(idvisit) |> 
               mutate(uses_llm = any(has_llm == TRUE)) |> 
               filter(type == "timestamp") |> 
               add_dates()  |> 
               filter(date_time == min(date_time)) |> 
               ungroup() |> 
               select(-c(has_llm)),
             packages = c("lubridate", "collapse", "stringr")),




# Glotzdauer --------------------------------------------------------------

tar_target(
  data_slim_distinct_slice1,
  data_slim |> 
    distinct(.keep_all = TRUE) |> 
    group_by(nr, type, idvisit) |> 
    slice(1) |>
    ungroup()),

tar_target(glotzdauer,
           data_slim_distinct_slice1 |> 
             glotzdauer_playpause(),
           packages = c("collapse", "lubridate", "dplyr", "stringr"))



# END OF PIPELINE ---------------------------------------------------------


            
)
