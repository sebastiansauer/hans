when_visited <- function(data){
  

  data |> 
    fsubset(type ==  "timestamp") |> 
    group_by(idvisit) |> 
    filter(row_number() == 1) |> 
    ungroup() |> 
    ftransform(type = as.character(type)) |> 
    ftransform(date_time = parse_date_time(value, "ymd HMS")) |> 
    ftransform(dow = wday(date_time, week_start = 1),
               hour = hour(date_time)) |> 
    fselect(idvisit, dow, hour, date_time) |> 
    fgroup_by(idvisit, dow, hour) |> 
    ungroup()
  

  
}