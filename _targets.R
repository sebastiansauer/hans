# MASTER PIPELINE HANS LOG DATA ANALYSIS
# author: Sebastian Sauer

# setup -------------------------------------------------------------------
library("targets")
library("tarchetypes")

tar_option_set(
  packages = c(
    "data.table",
    "dplyr",
    "purrr",
    "readr",
    "tidyr",
    "collapse",
    "stringr",
    "lubridate"
  )
)

options(lubridate.week.start = 1)

tar_source()

# START OF PIPELINE -------------------------------------------------------

## import data -------------------------------------------------------------
list(
  tar_target(config_file, "config.yaml", format = "file"),

  tar_target(config, read_yaml(config_file), packages = "yaml"),

  tar_target(data_files_list, find_data_files(config), format = "file"),

  tar_target(data_files_dupes_excluded, exclude_dupes(data_files_list)),

  tar_target(
    data_imported,
    data_files_dupes_excluded |>
      import_and_bind_data()
  ),

  ## prep data ---------------------------------------------------------------
  tar_target(data_prepped, data_imported |> prep_data(), packages = "janitor"),

  # tar_target(data_all_chr,
  #            data_prepped |> mutate(across(everything(), as.character))),

  # Why BOTH fct and chr???
  tar_target(
    data_all_fct,
    data_prepped |> mutate(across(everything(), as.factor)),
    packages = "collapse"
  ),

  tar_target(test_unique_idvisit, check_unique_ids(data_prepped)),

  tar_target(
    data_wide_slim,
    data_all_fct |>
      get_vars(
        vars = c(
          "idvisit",
          "fingerprint",
          grep("actiondetails_", names(data_all_fct), value = TRUE)
        )
      )
  ),

  tar_target(
    course_and_uni_per_visit,
    data_wide_slim |> extract_course_role_university_of_visit()
  ),

  ## pivot longer ------------------------------------------------------------
  tar_target(
    data_long,
    data_all_fct |> longify_data(),
    packages = c("data.table", "assertthat", "tibble")
  ),

  # tar_target(data_long_fingerprint,
  #  data_all_fct |> longify_data(id_var = "fingerprint"),
  #  packages = c("collapse", "assertthat")),

  tar_target(
    data_separated,
    slimify_nona_data(data_long),
    packages = c("dplyr", "tidyr", "collapse")
  ),

  # tar_target(data_separated_fingerprint,
  #            slimify_nona_data(data_long, idvar = "fingerprint"),
  #            packages = c("dplyr", "tidyr", "collapse")),

  tar_target(
    data_separated_filtered,
    data_separated |>
      filter_column_type()
  ),

  tar_target(
    data_separated_filtered_date_uni_course,
    add_date_uni_course_to_long_data(
      data_separated_filtered,
      course_and_uni_per_visit
    )
  ),

  ## count stuff per visit -------------------------------------------------
  # count number of actions per visit:
  tar_target(
    n_action,
    data_separated_filtered |>
      group_by(idvisit) |>
      summarise(nr_max = max(nr))
  ),
  tar_target(
    n_action_fingerprint,
    data_separated_filtered |>
      group_by(fingerprint) |>
      summarise(nr_max = max(nr))
  ),
  tar_target(n_action_lt_500,
  n_action |>
  filter(nr_max != 499)),

  tar_target(n_action_lt_500_fingerprint,
  n_action_fingerprint |>
  filter(nr_max != 499)),


  # count number of actions per visit and adds date of visit:
  tar_target(
    n_action_w_date,
    data_separated_filtered |>  # one row is one visit
      count_action_w_date()
  ),
  tar_target(
    n_action_w_date_fingerprint,
    data_separated_filtered |>
      count_action_w_date(idvar = fingerprint)
  ),

  # compute how much time was spent per visit:
  tar_target(time_spent, data_separated_filtered |> diff_time()),
  tar_target(
    time_spent_fingerprint,
    data_separated_filtered |> diff_time(idvar = fingerprint)
  ),

  # compute how much time was spent per course/per university and date:
  # one row is one visit
  tar_target(
    time_spent_w_course_university,
    compute_time_per_course_uni(
      data = time_spent,
      course_and_uni = course_and_uni_per_visit,
      idvar = idvisit
    )
  ),
  tar_target(
    time_spent_w_course_university_fingerprint,
    compute_time_per_course_uni(
      data = time_spent_fingerprint,
      course_and_uni = course_and_uni_per_visit,
      idvar = fingerprint
    )
  ),

  # compute how much time was spent per visit:
  # different computation to "timpe_spent", but same goal.
  tar_target(
    time_duration,
    data_all_fct %>%
      select(idvisit, fingerprint, visitduration) %>%
      mutate(visitduration_sec = as.numeric(visitduration)) %>%
      select(-visitduration)
  ),

  # compute when the site was visited:
  tar_target(time_visit_wday, data_separated_filtered |> when_visited()),
  tar_target(
    time_visit_wday_fingerprint,
    when_visited_fingerprint(data = data_separated_filtered)
  ),

  # compute time since last visit:
  tar_target(
    time_since_last_visit,
    data_all_fct |>
      select(idvisit, fingerprint, dayssincelastvisit)
  ),

  # count the type of things users did:
  tar_target(n_action_type, count_user_action_type(data_separated_filtered)),

  # get timestamps for each idvisits:
  tar_target(timestamps_added_to_idvisits,
    data_separated_filtered |> idvar_timestamp()
  ),
  tar_target(
    timestamps_added_to_fingerprints,
    data_separated_filtered |> idvar_timestamp(idvar = fingerprint)
  ),

  tar_target(
    n_action_type_per_month,
    add_dates_to_n_action_type(data = n_action_type, data_time = time_spent_w_course_university),
    packages = c("lubridate", "data.table", "dplyr")
  ),


  ## MC answers and LLM -----------------------------------------------------
  # count "Multiple choice answer selected" (only for idvisits, not for fingerprints):
  tar_target(
    n_mc_answers_selected,
    data_separated_filtered |>
      count_mc_answers() |>
      group_by(idvisit) |>
      count()
  ),

  # add timestamps to the idvisits with "MC choice answer selected":
  tar_target(
    mc_answers_with_timestamps,
    timestamps_added_to_idvisits |>
      select(idvisit, timestamp) |>
      left_join(n_mc_answers_selected, by = "idvisit") |>
      drop_na() |>
      select(idvisit, timestamp, n)
  ),

  # get LLM responses to user:
  tar_target(
    llm_response_text,
    n_action_type |>
      get_llm_response_text(),
    packages = c("tokenizers")
  ),
  # add date, course, university to LLM responses:
  tar_target(
    llm_response_text_date_course_uni,
    time_spent_w_course_university |> 
      mutate(idvisit = as.integer(idvisit)) |>
      left_join(llm_response_text, by = "idvisit") |>
      select(-any_of(c("type", "value"))),
  ),


  # count AI transcripts:
  tar_target(
    ai_transcript_clicks_per_month,
    data_separated_filtered |>
      count_ai_transcripts_per_month()
  ),

  # count LLM interactions:
  tar_target(
    ai_llm_per_months,
    data_separated_filtered |>
      count_llm_interactions()
  ),

  # count how many visitors used LLM:
  tar_target(
    idvisit_has_llm,
    data_separated_filtered |>
      count_visitor_interaction_with_llm()
  ),
  tar_target(
    idvisit_has_llm_fingerprint,
    data_separated_filtered |>
      count_visitor_interaction_with_llm(idvar = fingerprint)
  ),

  ## prompt length ----------------------------------------------------------
  # compute prompt length in tokens:
  # token itself is not saved, only length
  tar_target(
    prompt_length,
    data_separated_filtered |>
      compute_prompt_length(),
    packages = c("tokenizers", "stringr")
  ),
  tar_target(
    prompt_length_date_uni_course, 
    time_spent_w_course_university |> 
      mutate(idvisit = as.integer(idvisit)) |>
      left_join(prompt_length, by = "idvisit") |>
      select(-any_of(c("type", "value"))),
    packages = c("dplyr", "lubridate")
  ),

  tar_target(
    prompts_texts_date_course_uni,
    data_separated_filtered |>
      compute_prompt_length(no_prompt_text = FALSE) |> 
      left_join(time_spent_w_course_university |> mutate(idvisit = as.integer(idvisit)), 
        by = "idvisit"),        
    packages = c("tokenizers", "stringr")
  ),


  ## Glotzdauer --------------------------------------------------------------
  # compute time while watching vidoes:
  # too slow, change pivot to data.table
  tar_target(
    data_separated_distinct_slice,
    data_separated |>
      compute_glotzdauer(),
    packages = c("data.table", "dplyr", "lubridate")
  )

  # END OF PIPELINE ---------------------------------------------------------
)
