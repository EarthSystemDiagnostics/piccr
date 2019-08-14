library(tidyverse)

writeDataToFile <- function(datasets, config){
  config$output_directory %>%
    createOutputDirectory() %>%
    writeDatasets(datasets, config)
}

createOutputDirectory <- function(folder){
  dir.create(folder, showWarnings = FALSE)
  return(folder)
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