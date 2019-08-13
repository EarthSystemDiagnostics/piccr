library(tidyverse)

writeDataToFile <- function(datasets, config){
  
  config$output_directory %>%
    createOutputDirectory() %>%
    writeDatasets(datasets)
}

createOutputDirectory <- function(folder){
  dir.create(folder, showWarnings = FALSE)
  return(folder)
}

writeDatasets <- function(folder, datasets){
  processedDatasets <- datasets$processed
  walk(names(processedDatasets), writeSingleDataset, folder = folder, datasets = processedDatasets)
}

writeSingleDataset <- function(nameOfDataset, folder, datasets){
  write_csv(datasets[[nameOfDataset]], 
            path = file.path(folder, nameOfDataset), 
            na = "")
}