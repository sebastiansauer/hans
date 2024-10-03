add_dates <- function(data_slim) {

library(lubridate)
  
  data_slim |> 
    ftransform(type = as.character(type)) |>   # collapse
    ftransform(date_time = parse_date_time(value, "ymd HMS")) |>   # lubridate
    ftransform(
      dow = wday(date_time),  # week start set globally to Monday (1)
      hour = hour(date_time),
      year = year(date_time),
      month = month(date_time)) |> 
    ftransform(
      year_month = paste0(as.character(year), "-", as.character(month))
    )
}

