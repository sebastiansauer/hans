diff_time <- function(data) {
  data |> 
    filter(type ==  "timestamp") |> 
    group_by(idvisit) |> 
    mutate(type = as.character(type)) |> 
    mutate(time = parse_date_time(value, "ymd HMS")) |> 
    summarise(time_diff = max(time) - min(time))
}