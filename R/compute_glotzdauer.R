compute_glotzdauer <- function(d) {
  
  d <- as.data.table(d)

  # Filter relevant rows first
  d_events <- d[
    type %in%
      c("timestamp", "eventaction") &
      (value %in% c("play", "pause") | type == "timestamp")
  ]

  d_filtered <- d_events[idvisit %in% idvisit[type == "eventaction"]]


  # Compute by group
  d_glotzdauer_dt <- d_filtered[,
    .(
      
      first_play = min(timestamp, na.rm = TRUE),
      last_pause = max(timestamp, na.rm = TRUE)
    ),
    by = idvisit
  ][, time_diff := difftime(last_pause, first_play)]

  d_glotzdauer <- as_tibble(d_glotzdauer_dt)
}
