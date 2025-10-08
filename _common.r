# sources funs:
list.files("R", full.names = TRUE) |>
  purrr::walk(source)


## Optionen setzen:

options(lubridate.week.start = 1) # Monday as first day
#options(collapse_mask = "all") # use collapse for all dplyr operations
options(chromote.headless = "new") # Chrome headleass needed for gtsave

# ggplot2 theme setzen:
library(ggplot2)
theme_set(theme_minimal())

scale_colour_discrete <- function(...) scale_colour_brewer(palette = "Set2")
scale_fill_discrete <- function(...) scale_fill_brewer(palette = "Set2")
