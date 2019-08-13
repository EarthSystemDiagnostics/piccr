library(tidyverse)

associateStandardsWithConfigInfo <- function(datasets, config){
  map(datasets, associateStandardsWithConfigInfoForDataset, config = config)
}

associateStandardsWithConfigInfoForDataset <- function(dataset, config){
  
  configAsTable <- do.call(rbind, config$STANDARDS) %>%
    data.frame() %>%
    as_tibble() %>%
    transmute(`Identifier 1` = as.character(name), 
              o18_True = as.double(o18_True), 
              H2_True = as.double(H2_True),
              useForMemCorr = as.logical(use_for_memory_correction),
              useForDriftCorr = as.logical(use_for_drift_correction),
              useForCalibration = as.logical(use_for_calibration))
  
  left_join(x = dataset, y = configAsTable)
}
