library(collapse)
library(lubridate)

tar_load(data_user1_long_slim)
tar_load(data_slim)

data_slim_tail <- 
  data_slim[1e5:101000, ] 

d <-
  data_slim |> 
  filter(type == "subtitle") |> 
  mutate(value = gsub('["\']', '', value))


d2 <- 
d |> 
  mutate(category = case_when(
    str_detect(value, "https") ~ "visit_page",
    str_detect(value, "login") ~ "login",
    str_detect(value, "Kanäle") ~ "Kanäle",
    str_detect(value, "Medien") ~ "Medien",
    str_detect(value, "GESOA") ~ "GESOA",  
    str_detect(value, "video") ~ "video",
    str_detect(value, "Search Results Count") ~ "Search Results Count",
    str_detect(value, "in_media_search") ~ "in_media_search",  
    str_detect(value, "click_topic") ~ "click_topic",
    str_detect(value, "click_slideChange") ~ "click_slideChange",
    str_detect(value, "click_channelcard") ~ "click_channelcard",
    str_detect(value, "video") ~ "video",   
    TRUE ~ NA
  ))


out <- 
  count_user_action_type(data_slim)
