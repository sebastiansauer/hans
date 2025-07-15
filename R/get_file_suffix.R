
file_suffix <- function(files) {
  str_extract(files, pattern = "[^\\.]\\w{2,4}$")
}
