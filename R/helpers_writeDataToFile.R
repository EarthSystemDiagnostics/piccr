library(tidyverse)

#' writeDataToFile
#'
#' Save processed datasets to disc. Note that the component 
#' 'datasets$processed' contains the dataframes that are
#' saved.
#' 
#' Uses the following config arguments:
#'   - 'output_directory'
#'   - 'include_standards_in_output'
#'   
#' @param datasets A nested list, as output by processData(..).
#'                 Contains the component $processed.
#' @param config A named list. Needs to contain at least the components
#'               'output_directory' and 'include_standards_in_output'.
#'
#' @return No relevant return value
#'
writeDataToFile <- function(datasets, config){
  config$output_directory %>%
    createOutputDirectory() %>%
    writeDatasets(datasets, config)
}

createOutputDirectory <- function(folder){
  dir.create(folder, showWarnings = FALSE)
  return(folder) # make function usable in a pipe
}

writeDatasets <- function(folder, datasets, config){
  processedDatasets <- datasets$processed
  walk(names(processedDatasets), writeSingleDataset, folder = folder, 
       datasets = processedDatasets, config = config)
}

writeSingleDataset <- function(nameOfDataset, folder, datasets, config){
  datasets[[nameOfDataset]] %>%
    removeStandardsFromDataIfRequested(config) %>%
    write_csv(path = file.path(folder, nameOfDataset), na = "")
}

removeStandardsFromDataIfRequested <- function(dataset, config){
  if(!config$include_standards_in_output){
    return(filter(dataset, !isStandard(`Identifier 1`, config)))
  }
  return(dataset)
}