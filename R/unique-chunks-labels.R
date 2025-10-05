#' Ensure unique chunk labels in an R Markdown file

ensure_unique_chunk_labels <- function(f) {
  #' params file to process
  #' returns nothing, modifies file in place

  # i <- "index.qmd" # target file
  lines <- readLines(f)
  label_count <- list()

  lines_fixed <- sapply(
    lines,
    function(line) {
      if (grepl("^```\\{r ", line)) {
        lab <- sub("^```\\{r ([^}]+)\\}.*$", "\\1", line)
        if (is.null(label_count[[lab]])) {
          label_count[[lab]] <<- 1
        } else {
          label_count[[lab]] <<- label_count[[lab]] + 1
        }
        if (label_count[[lab]] > 1) {
          line <- sub(lab, paste0(lab, "_", label_count[[lab]]), line)
        }
      }
      line
    },
    USE.NAMES = FALSE
  )

  writeLines(lines_fixed, f)
}
