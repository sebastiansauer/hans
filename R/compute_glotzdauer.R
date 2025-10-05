compute_glotzdauer <- function(d) {
  
  setDT(d)

  # Filter relevant rows first
  d_events <- d[
    type %in%
      c("timestamp", "eventaction") &
      (value %in% c("play", "pause") | type == "timestamp")
  ]

  # Convert timestamps once
  d_events[type == "timestamp", timestamp := as.POSIXct(value)]

  # Compute by group
  d_glotzdauer_dt <- d_events[,
    .(
      
      first_play = min(timestamp[type == "eventaction" & value == "play"], na.rm = TRUE),
      last_pause = max(timestamp[type == "eventaction" & value == "pause"], na.rm = TRUE),
      date = as.Date(min(timestamp, na.rm = TRUE))
    ),
    by = idvisit
  ]

  d_glotzdauer_dt[, time_diff := difftime(last_pause, first_play)]

  return(d_glotzdauer_dt)
}
