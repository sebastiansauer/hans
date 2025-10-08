compute_glotzdauer <- function(data_separated) {
  d <- as.data.table(data_separated)

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
    mutate(time_stamp = as_date(value)) |>
    group_by(idvisit) |>
    summarise(
      first_play = min(time_stamp, na.rm = TRUE),
      last_pause = max(time_stamp, na.rm = TRUE)
    )

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
