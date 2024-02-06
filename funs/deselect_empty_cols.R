deselect_empty_cols <- function(df){
  df |> 
    select(where(~ !all(is.na(.))))
}
