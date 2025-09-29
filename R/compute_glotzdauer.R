compute_glotzdauer <- function(d) {
  setDT(d)

  # Much faster deduplication using data.table
  d_no_dublicates <- unique(d, by = c("nr", "type", "idvisit"))

  # d_no_dublicates <-
  # d |>
  #   # discard dublicates:
  #   distinct(.keep_all = TRUE) |>
  #   group_by(nr, type, idvisit) |>
  #   slice(1) |>
  #   ungroup()

  setDT(d_no_dublicates)
  # Even faster approach - avoid pivoting entirely
  d_events <- d_no_dublicates[
    value %in% c("play", "pause") | type == "timestamp"
  ][
    order(idvisit, nr)
  ]

  # Split into separate data.tables for each type
  timestamps <- d_events[type == "timestamp", .(nr, idvisit, timestamp = value)]
  events <- d_events[type == "eventaction", .(nr, idvisit, eventaction = value)]

  # Join them directly
  d_filtered_wide_dt <- events[timestamps, on = c("nr", "idvisit"), nomatch = 0]

  # d_play_pause_timestamps <-
  #   d_no_dublicates|>
  #   # for each id_visit, we are interested in the actions where some videoplayer clicks happened:
  #   mutate(
  #     is_target = value %in% c("play", "pause")) %>%
  #   filter(is_target | type == "timestamp") |>
  #   select(-is_target) |>
  #   group_by(idvisit) |>
  #   arrange(idvisit) |>
  #   ungroup()

  # d_play_pause_timestamps |>
  #   pivot_wider(names_from = "type", values_from = "value") |>
  #   drop_na()

  d_glotzdauer_dt <- d_filtered_wide_dt[,
    {
      # Convert timestamp to proper datetime if it's not already
      ts <- as.POSIXct(timestamp)

      # Get play and pause timestamps
      play_times <- ts[eventaction == "play"]
      pause_times <- ts[eventaction == "pause"]

      # Calculate results
      list(
        first_play = if (length(play_times) > 0) {
          min(play_times)
        } else {
          as.POSIXct(NA)
        },
        last_pause = if (length(pause_times) > 0) {
          max(pause_times)
        } else {
          as.POSIXct(NA)
        }, # Note: using max for last pause
        date = as.Date(min(ts, na.rm = TRUE))
      )
    },
    by = idvisit
  ][,
    time_diff := difftime(last_pause, first_play)
  ]

  # d_glotzdauer <-
  #   d_filtered_wide |>
  #   group_by(idvisit) %>%
  #   summarise(
  #     first_play = min(timestamp[eventaction == "play"], na.rm = TRUE),
  #     last_pause = min(timestamp[eventaction == "pause"], na.rm = TRUE),
  #     date = date(min(timestamp))
  #   ) %>%
  #   #filter(!is.na(first_play) & !is.na(last_pause)) %>%
  #   mutate(time_diff = difftime(last_pause, first_play)) %>%
  #   ungroup()

  return(d_glotzdauer_dt)
}
