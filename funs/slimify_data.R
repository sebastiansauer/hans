slimify_nona_data <- function(data_long) {
  
  out <- 
    data_long |> 
    # prepare to count the number of things a user does:
    fselect(variable, value, idvisit) |> 
    separate(variable, sep = "_", into = c("constant", "nr", "type")) |> 
    fselect(-c(constant)) |> 
    ftransform(nr = as.integer(nr),
               idvisit = as.integer(idvisit),
               type = factor(type),
               value = as.character(value)) |> 
    #ungroup() |> 
    roworder(idvisit, nr)
    
  return(out)

}