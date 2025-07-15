extract_course_of_visit <- function(d = data_wide_slim){
d |> # data such as "data_wide_slim"
  select(actiondetails_0_url, actiondetails_0_timestamp) |> 
  mutate(course = str_extract(actiondetails_0_url, "(?<=\\.student\\.)[a-zA-Z0-9]+"))
}
