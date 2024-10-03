longify_data <- function(data, no.na = TRUE){
  out <- 
    data |> 
    get_vars(vars = c("idvisit", 
                      grep("actiondetails_", names(data),
                           value = TRUE)
                      )
             ) |> 
    pivot(ids = "idvisit",
          how = "longer",
          check.dups = TRUE, 
          factor = FALSE)
  
  if (no.na) {
    out <-
      out %>% 
      filter(complete.cases(.)) |> 
      filter(value != "")
  }
  
  return(out)
}