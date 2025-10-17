prep_glotzdauer <- function(data_separated_distinct_slice) {
  data_separated_distinct_slice |>
    # we will assume that negative glotzdauer is the as positive glotzdauer:
    mutate(time_diff_abs_sec = abs(as.numeric(time_diff, units = "secs"))) |>
    # without glotzdauer smaller than 10 minutes:
    filter(time_diff_abs_sec < 60 * 10) |>
    mutate(time_diff_abs_min = time_diff_abs_sec / 60)
}
