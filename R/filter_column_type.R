filter_column_type <- function(data) {
  data |>
    filter(
      !type %in%
        c("pageloadtime", "pageloadtimemilliseconds", "title", "type", "url")
    )
}
