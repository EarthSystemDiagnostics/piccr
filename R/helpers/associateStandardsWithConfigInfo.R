library(tidyverse)

associateStandardsWithConfigInfo <- function(datasets, config){
  map(datasets, associateStandardsWithConfigInfoForDataset, config = config)
}

associateStandardsWithConfigInfoForDataset <- function(dataset, config){
  
  configAsTable <- do.call(rbind, config$STANDARDS) %>%
    data.frame() %>%
    as_tibble() %>%
    transmute(`Identifier 1` = as.character(NAME), 
              O18_True = as.double(O18_VAL), 
              H2_True = as.double(H2_VAL),
              useForMemCorr = as.logical(MEMORYCORR),
              useForDriftCorr = as.logical(DRIFTCORR),
              useForCalibration = as.logical(CALIBRATION))
  
  left_join(x = dataset, y = configAsTable)
}
