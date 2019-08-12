library(tidyverse)

associateStandardsWithTrueValues <- function(datasets, config){
  map(datasets, associateStandardsWithTrueValuesForDataset, config = config)
}

associateStandardsWithTrueValuesForDataset <- function(dataset, config){
  datasetWithTrueValues <- mutate(dataset, 
                                  O18_True = getTrueValFromName(`Identifier 1`, "O18", config),
                                  H2_True = getTrueValFromName(`Identifier 1`, "H2", config))
}

getTrueValFromName <- function(ids, type, config){
  map_dbl(ids, function(id){
    for(std in config$STANDARDS){
      if(std$NAME == id) return(std[[str_c(type, "_VAL")]])
    }
    return(NA)
  })
}
