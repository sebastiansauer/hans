slimify_data <- function(data_long) {
  
  data_long2 <- 
    data_long |> 
    # prepare to count the number of things a user does:
    select(name, value, idvisit) |> 
    separate(name, sep = "_", into = c("constant", "nr", "type")) |> 
    select(-c(constant)) |> 
    mutate(nr = as.integer(nr),
           idvisit = as.integer(idvisit),
           type = factor(type),
           value = as.character(value)) |> 
    ungroup()
    
  # out <-
  #   data_long2 |> 
  #   # Count the number of things a user does:
  #   group_by(idvisit) |> 
  #   summarise(max_nr = max(nr)) 
  # 
  return(out)

}