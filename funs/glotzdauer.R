
glotzdauer_playpause <- function(d, idvisit, action_type, nr){

d_filtered_wide <- 
d |> 
  # for each id_visit, we are interested in the actions where some videoplayer clicks happened:
  mutate(
    is_target = value %in% c("play", "pause")) %>%
  filter(is_target | type == "timestamp") |> 
  select(-is_target) |> 
  group_by(idvisit) |> 
  arrange(idvisit) |> 
  ungroup() |> 
  pivot_wider(names_from = "type", values_from = "value") |> 
  drop_na() 

d_glotzdauer <- 
  d_filtered_wide |> 
  group_by(idvisit) %>%
  summarise(
    first_play = min(timestamp[eventaction == "play"], na.rm = TRUE),
    last_pause = min(timestamp[eventaction == "pause"], na.rm = TRUE),
    date = date(min(timestamp))
  ) %>%
  #filter(!is.na(first_play) & !is.na(last_pause)) %>%
  mutate(time_diff = difftime(last_pause, first_play)) %>%
  ungroup()

return(d_glotzdauer)  

}



