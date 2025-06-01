longify_data <- function(data, no.na = TRUE){
  out <- 
    data |> 
    # fast "select":
    get_vars(vars = c("idvisit", 
                      grep("actiondetails_", names(data),
                           value = TRUE)
                      )
             ) |> 
    # fast "pivot_longer":
    pivot(ids = "idvisit",
          how = "longer",
          check.dups = TRUE, 
          factor = FALSE)
  
  # optional - rm missing values ("no NA"):
  if (no.na) {
    out <-
      out %>% 
      filter(complete.cases(.)) |> 
      filter(value != "")
  }
  
  return(out)
}