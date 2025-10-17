comp_semester_rects <- function(plot_data, col_date, ymin = 0, ymax = Inf) {
  library(lubridate)
  library(dplyr)
  library(tibble)

  #' Extract and parse dates for semester rectangle plotting
  #' #' This function extracts dates from the specified column of the input data,
  #' parses them into date
  #' params plot_data A data frame containing the date column.
  #' params col_date The name of the column containing date information - date format, use eg lubridate::ymd(paste0(year, "-01-01"))) # MAKE Date class

  x <- plot_data[[col_date]]
  if (is.factor(x)) {
    x <- as.character(x)
  }

  parse_dates <- function(x) {
    # handle NA-only
    if (all(is.na(x))) {
      return(as.POSIXct(NA))
    }
    # year-month like "2023-1" or "2023-01"
    if (all(grepl("^\\d{4}-\\d{1,2}$", x[!is.na(x)]))) {
      return(ymd(paste0(x, "-01")))
    }
    # try common lubridate parsers (datetime -> date)
    out <- suppressWarnings(ymd_hms(x))
    if (!all(is.na(out))) {
      return(out)
    }
    out <- suppressWarnings(ymd(x))
    if (!all(is.na(out))) {
      return(out)
    }
    # fallback to as.POSIXct for other formats
    out <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
    if (!all(is.na(out))) {
      return(out)
    }
    stop("comp_semester_rects: could not parse dates in column ", col_date)
  }

  dates <- parse_dates(x)
  min_date <- min(dates, na.rm = TRUE)
  max_date <- max(dates, na.rm = TRUE)
  min_year <- year(min_date)
  max_year <- year(max_date)

  rect_years <- seq(min_year, max_year + 1)

  summer_rects <- tibble(year = rect_years) |>
    mutate(
      xmin = ymd(paste0(year, "-03-01")),
      xmax = ymd(paste0(year, "-07-01"))
    )

  winter_rects <- tibble(year = rect_years) |>
    mutate(
      xmin = ymd(paste0(year, "-10-01")),
      xmax = ymd(paste0(year + 1, "-02-01"))
    )

  rect_data <- bind_rows(summer_rects, winter_rects) |>
    mutate(ymin = ymin, ymax = ymax) |>
    filter(xmin <= max_date, xmax >= min_date)

  rect_data
}
