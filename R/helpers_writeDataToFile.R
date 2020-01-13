#' Save processed data
#'
#' Save a given number of processed datasets on disc as csv files in a specified
#' directory.
#'   
#' @param datasets a list of processed measurement data as output by
#'   \code{\link{processData}} or minimum a list of lists with each sublist
#'   containing a processed data set in the component \code{processed}.
#' @param config a named list which needs to contain the following components:
#'   \describe{
#'   \item{\code{output_directory}:}{character string with the directory path
#'     where the data shall be saved.}
#'   \item{\code{include_standards_in_output}:}{logical; if \code{TRUE} the
#'   processed data of measured standards is included in the output.}
#' }
#' @import dplyr
#' 
writeDataToFile <- function(datasets, config){
  config$output_directory %>%
    createOutputDirectory() %>%
    writeDatasets(datasets, config)
}

#' Create output directory
#'
#' Create the directory in which the programme output data is saved.
#'
#' @param folder character string with the path to the folder which is
#'   to be created. The folder may already exist in which case nothing happens.
#' @return the input \code{folder}.
#' 
createOutputDirectory <- function(folder){
  dir.create(folder, showWarnings = FALSE)
  return(folder) # make function usable in a pipe
}

#' Write data sets to disc
#'
#' Write the given processed data sets to disc as csv files. This is a wrapper
#' for \code{\link{writeSingleDataset}} which performs the actual saving for a
#' single data set.
#'
#' @param folder character string with the directory path in which to save the
#'   data.
#' @param config a named list containing the component
#'   \code{include_standards_in_output}: a logical to signal whether to save
#'   the processed data including the measured standards or not.
#' @inheritParams writeDataToFile
#' @seealso \code{\link{writeSingleDataset}}
#' 
writeDatasets <- function(folder, datasets, config){
  purrr::walk(datasets, writeSingleDataset, folder = folder, config = config)
}

#' Write a data set to disc
#'
#' Write the given processed data set to disc as a csv file in the specified
#' directory.
#' @param dataset a data frame of processed measurement data.
#' @inheritParams writeDatasets
#' @import dplyr
#' 
writeSingleDataset <- function(dataset, folder, config){
  dataset$processed %>%
    removeStandardsFromDataIfRequested(config) %>%
    readr::write_csv(path = file.path(folder, dataset$name), na = "")
}

#' Remove standards from data frame
#'
#' Remove those rows from a given data set which contain the data of the
#' measured standards.
#' 
#' @param dataset a data frame with measurement data of a specific data set.
#' @param config a named list containing the component
#'   \code{include_standards_in_output} to signal whether the standard data
#'   shall be removed from \code{dataset}.
#'
#' @return The input \code{dataset} with the standard data removed if signalled
#'   in \code{config}.
#' 
removeStandardsFromDataIfRequested <- function(dataset, config){
  if(!config$include_standards_in_output){
    return(dplyr::filter(dataset, !isStandard(`Identifier 1`, config)))
  }
  return(dataset)
}
