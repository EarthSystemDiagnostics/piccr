library(readr)
library(purrr)
library(stringr)

readFiles <- function(config) {
  
  folder <- config$input_directory
  file_pattern <- str_c("*", config$file_extension)
  
  filenames <- list.files(path = folder, pattern = file_pattern)
  pathsToFiles <- file.path(folder, filenames)
  
  datasets <- map(pathsToFiles, read_csv)
  names(datasets) <- filenames
  
  return(datasets)
}