library(readr)
library(purrr)
library(stringr)

#' readFiles
#' 
#' Read all files from a given input directory that have a 
#' given file extension. Only works for csv files.
#' 
#' The input directory and the file extension are given in
#' the config args 'input_directory' and 'file_extension'
#' respectively.
#'
#' @param config A named list. Needs to contain at least the#
#'               components 'input_directory' and 'file_extension'.
#'
#' @return A named list of dataframes. The names of the 
#'         list elements correspond to the file names in the
#'         input directory.
#'
readFiles <- function(config) {
  
  folder <- config$input_directory
  file_pattern <- str_c("*", config$file_extension)
  
  filenames <- list.files(path = folder, pattern = file_pattern)
  pathsToFiles <- file.path(folder, filenames)
  
  datasets <- map(pathsToFiles, read_csv)
  names(datasets) <- filenames
  
  return(datasets)
}