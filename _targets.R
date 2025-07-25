# MASTER PIPELINE HANS LOG DATA ANALYSIS
# author: Sebastian Sauer


# setup -------------------------------------------------------------------
library("targets")
library("tarchetypes")

tar_option_set(
  packages = c("data.table", "dplyr", "purrr", "readr", "tidyr", 
               "collapse", "stringr", "lubridate"))

options(lubridate.week.start = 1)

tar_source()

# START OF PIPELINE -------------------------------------------------------

## import data -------------------------------------------------------------
list(
  tar_target(config_file, "config.yaml", 
             format = "file"), 
  
  tar_target(config, read_yaml(config_file), 
             packages = "yaml"),
  
  tar_target(data_files_list, 
             find_data_files(config), 
             format = "file"),
  
  tar_target(data_files_dupes_excluded, 
             exclude_dupes(data_files_list)),
  
  tar_target(data_imported, 
             data_files_dupes_excluded |>  
               import_and_bind_data()),
  
  ## prep data ---------------------------------------------------------------
  tar_target(data_prepped, 
             data_imported |> prep_data(),
             packages = "janitor"),
  
  tar_target(data_all_chr, 
             data_prepped |> mutate(across(everything(), as.character))),
  
  # Why BOTH fct and chr???
  tar_target(data_all_fct,
             data_prepped |> mutate(across(everything(), as.factor)),
             packages = "collapse"),
  
  ## tinify data -------------------------------------------------------------
  tar_target(test_unique_idvisit,
             check_unique_ids(data_prepped)),
  
  tar_target(data_wide_slim,
             data_all_chr |> 
               get_vars(vars = c("idvisit", 
                                 grep("actiondetails_", names(data_all_chr),
                                      value = TRUE)))),

  tar_target(course_and_uni_per_visit,
             data_wide_slim |> extract_course_role_university_of_visit()),
  
  ## pivot longer ------------------------------------------------------------
  tar_target(data_long,
             data_all_chr |> longify_data(),
             packages = c("collapse", "assertthat")),
  
  tar_target(data_separated,
             slimify_nona_data(data_long), 
             packages = c("dplyr", "tidyr", "collapse")),
  
  tar_target(data_separated_filtered,
             data_separated |> 
               filter(!type %in% c("pageloadtime", 
                                   "pageloadtimemilliseconds",
                                   "title",
                                   "type",
                                   "url"))),
  
  ## count stuff per visit -------------------------------------------------
  tar_target(n_action,
             data_separated_filtered |>
               group_by(idvisit) |>
               summarise(nr_max = max(nr))), 
  
  tar_target(n_action_w_date,
             data_separated_filtered |> 
               count_action_w_date()),

  tar_target(time_spent,
             data_separated_filtered |> diff_time()),

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
  
  tar_target(time_visit_wday,
             data_separated_filtered |> when_visited()),
  
  tar_target(time_since_last_visit,
             data_all_chr |>  # changed from "fct" to "chr" 
               select(idvisit, dayssincelastvisit)),

  tar_target(n_action_type,
             count_user_action_type(data_separated_filtered)),
  
  tar_target(llm_response_text,
             n_action_type |> 
               get_llm_response_text(),
             packages = c("tokenizers")),

  tar_target(ai_transcript_clicks_per_month,
             data_separated_filtered |> 
               count_ai_transcripts_per_month()),
  
  tar_target(ai_llm_per_months,
             data_separated_filtered |> 
               count_llm_interactions()),
  
  tar_target(idvisit_has_llm, 
             data_separated_filtered |> 
               count_visitor_interaction_with_llm()),

  ## Glotzdauer --------------------------------------------------------------
  tar_target(
    data_separated_distinct_slice,
    data_separated |> 
      compute_glotzdauer())
  
  # END OF PIPELINE ---------------------------------------------------------
)



