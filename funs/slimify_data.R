slimify_nona_data <- function(data_long) {
  
  out <- 
    data_long |> 
    #drop_na() |> # better placed  more upstream in pipe, so save computation time downstream
    # prepare to count the number of things a user does:
    select(variable, value, idvisit) |> 
    separate(variable, sep = "_", into = c("constant", "nr", "type")) |> 
    select(-c(constant)) |> 
    mutate(nr = as.integer(nr),
           idvisit = as.integer(idvisit),
           type = factor(type),
           value = as.character(value)) |> 
    ungroup() |> 
    roworder(idvisit, nr)
    
  # out <-
  #   data_long2 |> 
  #   # Count the number of things a user does:
  #   group_by(idvisit) |> 
  #   summarise(max_nr = max(nr)) 
  # 
  return(out)

}