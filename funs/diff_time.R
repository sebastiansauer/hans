diff_time <- function(data) {
  data |> 
    fsubset(type ==  "timestamp") |> 
    group_by(idvisit) |> 
    fmutate(type = as.character(type)) |> 
    fmutate(time = parse_date_time(value, "ymd HMS"),
           time_diff = max(time) - min(time)) 
}

# Note: The data set is still grouped by idvisit!
