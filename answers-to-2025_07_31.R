
######################## Count MC



multiple_choice_answer_selected <- 
data_separated_filtered |> 
  filter(str_detect(value, "Multiple choice answer selected"))


multiple_choice_answer_selected |> 
  group_by(idvisit) |> 
  count()

data_separated_filtered_timestamp <-
data_separated_filtered |> 
  filter(str_detect(type, "timestamp")) |>
  select(-type) |> 
  mutate(timestamp = ymd_hms(value)) |> 
  group_by(idvisit) |> 
  filter(timestamp == max(timestamp)) |> 
  slice_head(n = 1)

# merge with timestamps the "miltiple choice ..."
multiple_choice_answer_selected_with_timestamp <- 
data_separated_filtered_timestamp |> 
  select(idvisit, timestamp) |> 
  left_join(multiple_choice_answer_selected, by = "idvisit") |> 
  drop_na() |> 
  select(idvisit, timestamp, nr)



### Count Fingerprints

tar_load(data_all_fct)
temp <-
  data_all_fct |> 
               get_vars(vars = c("idvisit", "fingerprint",
                                 grep("actiondetails_", names(data_all_fct),
                                      value = TRUE)))

out <- 
    data_all_fct |> 
    # fast "select":
    get_vars(vars = c("fingerprint", "idvisit", 
                      grep("actiondetails_", names(data_all_fct),
                           value = TRUE)
                      )
             ) 

out2 <- 
out |> 
  head(1000) |> 
    # fast "pivot_longer":
    pivot(ids = c("idvisit", "fingerprint"),
          how = "longer",
          check.dups = TRUE, 
          factor = TRUE)

data_long |> 
  head(100)
