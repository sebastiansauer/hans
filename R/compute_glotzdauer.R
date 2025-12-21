compute_glotzdauer <- function(data = data_separated) {
  #' Video watching duration per visitid
  #' 
  #' Computes the duration of video watching per visitid.
  #' 
  #' @param data The main data frame in long format.
  #' 
  #' @retuns Data frame with 4 columns: id, start, end, duration 
  #' 
  #' @examples 
  #' video <- compute_glotzdaer(data_separated) 

  
  d <- as.data.table(data)

  # Filter relevant rows first
  d_events <- d[
    type %in%
      c("timestamp", "eventaction") &
      (value %in% c("play", "pause") | type == "timestamp")
  ]

  d_filtered <- d_events[idvisit %in% idvisit[type == "eventaction"]]

  d_filtered_tibble <- as_tibble(d_filtered)

  d_filtered_tibble |>
    filter(type != "eventation") |>
    mutate(time_stamp = as_datetime(as.character(value))) |>
    group_by(idvisit) |>
    summarise(
      first_play = min(time_stamp, na.rm = TRUE),
      last_pause = max(time_stamp, na.rm = TRUE)
    ) |>
    mutate(time_diff = difftime(last_pause, first_play, units = "secs"))

  # Compute by group
  # d_glotzdauer_dt <- d_filtered[,
  #   .(

  #     first_play = min(.SD$timestamp, na.rm = TRUE),
  #     last_pause = max(.SD$timestamp, na.rm = TRUE)
  #   ),
  #   by = idvisit
  # ]
  # [, time_diff := difftime(last_pause, first_play)]

  #  d_glotzdauer <- as_tibble(d_glotzdauer_dt)
}
