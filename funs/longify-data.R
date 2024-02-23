longify_data <- function(data){
  data |> 
    get_vars(vars = c("idvisit", 
                      grep("actiondetails_", names(data), value = TRUE)
                      )
             ) |> 
    pivot(ids = "idvisit",
          how = "longer",
          check.dups = TRUE, 
          factor = FALSE)
}