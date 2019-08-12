library(readr)
library(purrr)
library(stringr)

readFiles <- function(config, folder = ".") {
  
  files <- list.files(path = folder, pattern = config$FILE_ID)
  pathToFiles <- str_c(folder, "/", files)
  
  datasets <- map(pathToFiles, read_csv)
  names(datasets) <- files
  
  return(datasets)
}