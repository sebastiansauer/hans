prep_data <- function(d){
  d |> 
    transform_to_true_NAs() |> 
    remove_empty(which = c("rows", "cols")) |> 
    repair_dttm_cols() |> 
    remove_constant(na.rm = TRUE) |> 
    select(-contains("svg")) |> 
    select(-contains("icon")) |> 
    filter(!str_detect(actiondetails_0_url, 
                       "=admin|=developer|=lecturer")) |> 
    filter(!str_detect(actiondetails_1_subtitle, 
                       "=admin|=developer|=lecturer")) |> 
    select(-c(contains("idpageview"), 
              contains("pretty"),
              contains("pageviewposition"),
              contains("pageid"),
              #select(-matches("\\w+_(?!0)\\d+_timestamp")),
              contains("timespent"))) |> 
    mutate(idvisit_old = idvisit,
           idvisit = 1:n()) |> 
    select(idvisit, everything())
}