exclude_filetype <- function(filelist, filetype = "json"){
  
  out <- filelist[!grepl(filetype, filelist)]
  
}
