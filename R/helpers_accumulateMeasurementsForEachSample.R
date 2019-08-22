library(tidyverse)

accumulateMeasurementsForEachSample <- function(datasets){
  
  map(datasets, accumulateMeasurementsForSingleDataset)
}

accumulateMeasurementsForSingleDataset <- function(dataset){
  
  dataset %>%
    group_by(`Identifier 1`, block) %>%
    summarise(`Identifier 2` = `Identifier 2`[[1]],
              delta.O18 = mean(`d(18_16)Mean`),
              delta.H2 = mean(`d(D_H)Mean`),
              sd.O18 = sd(`d(18_16)Mean`),
              sd.H2 = sd(`d(D_H)Mean`),
              Line = min(Line)) %>%
    arrange(Line) %>%  # preserve original order of samples
    select(-Line) %>%
    rowid_to_column("Line")
}