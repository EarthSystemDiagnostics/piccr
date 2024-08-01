#' Read the YAML configuration file
#' 
#' Read in the specified YAML configuration file for the \code{piccr}
#' processing.
#'
#' @param configFile a character string with the file path of the configuration
#' file.
#' 
#' @return A list of the parameters read from the configuration file.
#' 
parseConfig <- function(configFile){
  
  config <- tryCatch(
    expr = suppressWarnings(yaml::read_yaml(configFile)),
    error = function(e) {

      m <- paste0("Error reading config file `",
                  configFile,
                  "`.\nMake sure that you specified the correct path",
                  " and that read permissions are given.")

      stop(m, call. = FALSE)
  })

  config$config_file_name <- configFile

  return(config)
}

#' Read in measurement files
#' 
#' Read all files from a given input directory that match a given file
#' extension. Note that only csv files are supported.
#'
#' @param config A named list which needs to contain at least the
#'   components \code{input_directory} (the directory which contains the files
#'   to be read in) and \code{file_extension} (the file name extension to look
#'   for).
#'
#' @return A named list of data frames where the names of the list elements
#'   correspond to the file names in the input directory and where each data
#'   frame contains the data read in from the file.
#' 
readFiles <- function(config) {
  
  folder <- config$input_directory
  file_pattern <- stringr::str_c("*", config$file_extension)
  
  filenames <- list.files(path = folder, pattern = file_pattern)
  pathsToFiles <- file.path(folder, filenames)
  
  datasets <- purrr::map(pathsToFiles, readr::read_csv,
                         col_types = readr::cols())
  names(datasets) <- filenames
  
  return(datasets)
}
