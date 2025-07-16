diff_time <- function(data) {
  # compute time variables per visit (WIDE format data):
  data |> 
    filter(type ==  "timestamp") |> 
    select(idvisit, value) |> 
    group_by(idvisit) |> 
    mutate(time = parse_date_time(value, "ymd HMS")) |> 
    summarise(time_diff = max(time) - min(time),
              time_min = min(time),
              time_max = max(time)) 
}

# Note: The data set is still grouped by idvisit!
