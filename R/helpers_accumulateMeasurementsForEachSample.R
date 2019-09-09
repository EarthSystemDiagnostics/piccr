library(tidyverse)

accumulateMeasurementsForEachSample <- function(datasets, config){
  
  map(datasets, accumulateMeasurementsForSingleDataset, config = config)
}

accumulateMeasurementsForSingleDataset <- function(dataset, config){
    
  accumulatedData <- dataset %>%
    getLastNInjectionsForEachSample(config) %>%
    doAccumulate() %>% 
    rearrange()
  
  return(accumulatedData)
}

getLastNInjectionsForEachSample <- function(dataset, config){
  
  n <- config$average_over_last_n_inj
  
  # exit early if all injections should be kept
  if (n %in% c(-1, "all")) return(dataset)
  
  dataset %>%
    group_by(`Identifier 1`, block) %>% 
    slice((n() - n + 1):n()) %>%
    ungroup() %>%
    arrange(Line)
}

doAccumulate <- function(dataset){
  
  dataset %>%
    group_by(`Identifier 1`, block) %>%
    summarise(`Identifier 2` = `Identifier 2`[[1]],
              delta.O18 = mean(`d(18_16)Mean`),
              delta.H2 = mean(`d(D_H)Mean`),
              sd.O18 = sd(`d(18_16)Mean`),
              sd.H2 = sd(`d(D_H)Mean`),
              d.Excess = mean(dExcess),
              sd.d.Excess = sd(dExcess),
              Line = min(Line))
}

rearrange <- function(dataset){
  
  dataset %>%
    arrange(Line) %>%  # preserve original order of samples
    select(-Line) %>%
    rowid_to_column("Line")  # use continous row numbers
}