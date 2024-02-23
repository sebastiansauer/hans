count_user_action_type <- function(data){
  
    data |> 
    filter(type == "subtitle") |> 
    mutate(value = gsub('["\']', '', value)) |> 
    
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
  
}