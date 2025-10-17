comp_semester_rects <- function(plot_data, col_date, ymin = 0, ymax = Inf) {



  # --- 2. Determine Plot Range for Rectangles ---
  # Find the min/max year and n-count from your *processed* plot_data
  min_date <- min(as.POSIXct.Date(plot_data[[col_date]]), na.rm = TRUE)
  max_date <- max(as.POSIXct.Date(plot_data[[col_date]]), na.rm = TRUE)
  min_year <- year(min_date)
  max_year <- year(max_date)

  # --- 3. Calculate the Rectangle Coordinates (rect_data) ---

  # Generate years for the rectangles, ensuring we cover the full range
  # including potentially starting a "winter" semester in the min_year-1
  # and ending in max_year+1
  rect_years <- seq(min_year - 1, max_year + 1)

  # Summer semester: March 1 (Y) to July 1 (Y)
  summer_rects <- tibble(year = rect_years) |>
    mutate(
      xmin = ymd(paste0(year, "-03-01")),
      xmax = ymd(paste0(year, "-07-01"))
    )

  # Winter semester: October 1 (Y) to February 1 (Y+1)
  winter_rects <- tibble(year = rect_years) |>
    mutate(
      xmin = ymd(paste0(year, "-10-01")),
      xmax = ymd(paste0(year + 1, "-02-01"))
    )

  # Combine, set Y bounds, and filter to the actual plot area
  rect_data <- bind_rows(summer_rects, winter_rects) |>
    mutate(ymin = y_min, ymax = y_max) |>
    # Only keep rectangles that are fully or partially within the plot's X range
    filter(
      xmin <= max_date,
      xmax >= min_date
    )
}

# Use the output ("rect_data") of the function in your ggplot:
# geom_rect(
#   data = rect_data,
#   aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#   fill = "grey",
#   alpha = 0.2,
#   inherit.aes = FALSE # Essential to use the rect_data columns
# )
