exclude_dupes <- function(files){
  
  if (length(files) == 0) stop("no files found!")

  base_names <- tools::file_path_sans_ext(files)
  duplicates <- duplicated(base_names)
  data_files_wo_dups <- files[!duplicates]
  return(data_files_wo_dups)
}