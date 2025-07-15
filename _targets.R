# MASTER PIPELINE HANS LOG DATA ANALYSIS
# author: Sebastian Sauer


# setup -------------------------------------------------------------------
library("data.table")
library("targets")
library("tarchetypes")



# packages available for all targets:
tar_option_set(
  packages = c("data.table", "dplyr", "purrr", "readr", "tidyr", "collapse", "stringr", "lubridate"))

# set options:
options(lubridate.week.start = 1)
#options(collapse_mask = "all") # use collapse for all dplyr operations

# source funs:
# funs_files <- list.files(
#   path = "funs", pattern = "\\.R", full.names = TRUE)
# lapply(X = funs_files, FUN = source)
tar_source()


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
               import_and_bind_data(), 
             packages = c("data.table", "stringr")),
  

  

# prep data ---------------------------------------------------------------
  
  # prep data:
  tar_target(data_prepped, 
             data_imported |> prep_data(),
             packages = "janitor"),
  

  # transform all columns to character:
  # but why?
  tar_target(data_all_chr, 
             data_prepped |> mutate(across(everything(), as.character))),
  
  # transform all cols to factor: 
  tar_target(data_all_fct,
             data_prepped |> mutate(across(everything(), as.factor)),
             packages = "collapse"),
  # 
  # # exclude non-participants:
  # tar_target(data_users_only,
  #            data_all_chr ,
  #            packages = "stringr"),




# tinify data -------------------------------------------------------------

  # tar_target(data_less_cols,
  #            data_users_only |> 
  #              select(-c(contains("idpageview"), 
  #                        contains("pretty"),
  #                        contains("pageviewposition"),
  #                        contains("pageid"),
  #                        #select(-matches("\\w+_(?!0)\\d+_timestamp")),
  #                        contains("timespent")))),
  # # it appears that "idivisit" is NOT unique :-(


  # # make sure the ID is now unique
  # tar_target(data_unique_id,
  #            data_all_chr |> 
  #              mutate(idvisit_old = idvisit,
  #                     idvisit = 1:nrow(data_less_cols)) |> 
  #              select(idvisit, everything())),


  tar_target(test_unique_idvisit,
             check_unique_ids(data_prepped)),

  
  # prepare little data set for quick checking:
  # tar_target(data_little,
  #            data_prepped[1:50, 1:50]),
  # 
  # tar_target(data_user1,
  #            data_all_chr |> 
  #            filter(row_number() == 1)),
  # 
  # # tinify data set for quicker debugging:
  # tar_target(data_little_long,
  #            data_little |> longify_data(),  # transform into long format
  #            packages = "collapse"),
  # 
  # tar_target(data_user1_long,  # transform into long format:
  #            data_user1 |> longify_data(),
  #            packages = "collapse"),

tar_target(data_wide_slim,
           data_all_chr |> 
             get_vars(vars = c("idvisit", 
                               # get only the "actiondetails_XXX" variables:
                               grep("actiondetails_", names(data_all_chr),
                                    value = TRUE))),
           packages = "collapse"),

  # tar_target(data_wide_slim_head,
  #            data_wide_slim[1:100, ]),
  # time tags are still okay: starting with 2023


  tar_target(course_and_uni_per_visit,
             data_wide_slim |> extract_course_role_university_of_visit(),
             packages = c("stringr", "lubridate")),
  


# pivot longer ------------------------------------------------------------

# pivot longer to get a handle on the number of cols per login:
  tar_target(data_long,
             data_all_chr |> longify_data(),
             packages = c("collapse", "assertthat")),
  
 
  # # tidyverse appears to come the same object, but better double check:
  # tar_target(data_long_tidyverse,
  #            data_wide_slim |> 
  #              longify_data_tidyverse()),
  
  
  # # drop rows with missing data:
  # tar_target(data_long_nona,
  #            data_long |> drop_na() |> filter(value != "")),  # drop rows with missing data

  # slimify and separate:
  tar_target(data_separated,
             slimify_nona_data(data_long), 
             packages = c("dplyr", "tidyr", "collapse")),


  # filter unneeded rows:
  tar_target(data_separated_filtered,
             data_separated |> 
               filter(!type %in% c("pageloadtime", 
                                  "pageloadtimemilliseconds",
                                  "title",
                                  "type",
                                  "url"))),



# Mini slim data for debugging --------------------------------------------




  # tar_target(data_user1_long_slim,
  #           slimify_nona_data(data_user1_long), 
  #            packages = c("dplyr", "tidyr", "collapse")),
  # tar_target(data_separated_head,
  #            data_separated[1:1e5,]),
  # tar_target(data_separated_filtered_head,
  #            data_separated_filtered[1:1e5,]),



# count stuff per visit -------------------------------------------------

  # count rows per visit (n, WIDE formatted data):
  tar_target(n_action,
             data_separated_filtered |>
               group_by(idvisit) |>
               # column "nr" is the id of the action of this visit:
               # we just count the rows per idivist:
               summarise(nr_max = max(nr))), 

  # count rows per visit (WIDE format) plus the date/time of the start of this visit:
  tar_target(n_action_w_date,
             data_separated_filtered |> 
               count_action_w_date(),
             packages = "lubridate"),

  # # count courses:
  # tar_target(count_courses,
  #            data_separated_filtered )

  
  # compute time variables per visit (WIDE format data):
  tar_target(time_spent,
             data_separated_filtered |> diff_time(),
             packages = "lubridate"),
  # tar_target(time_minmax,
  #            data_separated_filtered |> time_min_max(),
  #            packages = "lubridate"),


  tar_target(time_spent_w_course_university,
             time_spent |> 
               mutate(idvisit = as.character(idvisit)) |> 
               left_join(course_and_uni_per_visit, by = "idvisit") |> 
               extract_date_components(time_min)),


  tar_target(time_duration,
             data_all_chr %>% 
               select(idvisit, visitduration) %>% 
               mutate(visitduration_sec = as.numeric(visitduration)) %>% 
               select(-visitduration)),

  # count time of visit per weekday:
  tar_target(time_visit_wday,
             data_separated_filtered |> when_visited(), 
             packages = c("collapse", "lubridate")),
  tar_target(time_since_last_visit,
             data_all_chr |>  # changed from "fct" to "chr" 
               select(idvisit, dayssincelastvisit)
             ),
  
  # count action categories per visit:
  tar_target(n_action_type,
             count_user_action_type(data_separated_filtered), packages = "stringr"),


  tar_target(llm_response_text,
             n_action_type |> 
               filter(str_detect(value, "llm_response")) |> 
               select(value) |> 
               mutate(lang = str_extract(value, "llm_response_([\\w]+)", group = 1),
                      tokens_n = lengths(tokenize_words(value))),
             packages = c("tokenizers", "stringr")),

  # count AI transcript clicks per month:
  tar_target(ai_transcript_clicks_per_month,
             data_separated_filtered |> 
               count_ai_transcripts_per_month(),
             packages = c("lubridate", "collapse", "stringr")),

  # count interactions with LLM per month:
  tar_target(ai_llm_per_months,
             data_separated_filtered |> 
               count_llm_interactions(),
             packages = c("lubridate", "collapse", "stringr")),


  # count how many visitors interact with the LLM:
  tar_target(idvisit_has_llm, 
             data_separated_filtered |> 
               count_visitor_interaction_with_llm(),
             packages = c("lubridate", "collapse", "stringr")),




# Glotzdauer --------------------------------------------------------------

tar_target(
  data_separated_distinct_slice1,
  data_separated |> 
    compute_glotzdauer(),
  packages = c("collapse", "lubridate", "dplyr", "stringr"))



# END OF PIPELINE ---------------------------------------------------------


            
)
