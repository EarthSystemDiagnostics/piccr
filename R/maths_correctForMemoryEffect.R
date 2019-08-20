library(tidyverse)

correctForMemoryEffect <- function(datasets){
  
  d18OCorrected <- map(datasets, correctSingleDatasetForMemoryEffect, column = "d(18_16)Mean")
  dDCorrected <- map(d18OCorrected, correctSingleDatasetForMemoryEffect, column = "d(D_H)Mean")
  
  return(dDCorrected)
}

correctSingleDatasetForMemoryEffect <- function(dataset, column){
  
  memoryCoefficients <- calculateMemoryCoefficients(dataset, column)
  datasetMemoryCorrected <- applyMemoryCorrection(dataset, memoryCoefficients, column)
  
  return(datasetMemoryCorrected)
}

calculateMemoryCoefficients <- function(dataset, column) {
  
  block1 <- filter(dataset, block == 1)
  deltaTrueAndDeltaTruePrev <- getDeltaTrueAndDeltaTruePrevForEachSample(block1, column, `Identifier 1`)
  
  memoryCoefficients <- deltaTrueAndDeltaTruePrev %>%
    mutate(memoryCoefficient = (.[[column]] - deltaTrueApproxPrev) / (deltaTrueApprox - deltaTrueApproxPrev)) %>%
    drop_na() %>%
    group_by(`Inj Nr`) %>%
    summarise(memoryCoefficient = mean(memoryCoefficient))
  
  return(memoryCoefficients)
}

applyMemoryCorrection <- function(dataset, memoryCoefficients, column){
  
  deltaTrueAndDeltaTruePrev <- getDeltaTrueAndDeltaTruePrevForEachSample(dataset, column = column, `Identifier 1`, block)
  
  memoryCorrectedRow <- inner_join(deltaTrueAndDeltaTruePrev, memoryCoefficients, by = c("Inj Nr")) %>%
    transmute(memoryCorrected = .[[column]] + (1-memoryCoefficient) / memoryCoefficient * (.[[column]] - deltaTrueApproxPrev)) %>%
    .$memoryCorrected
  
  dataset[[column]] <- memoryCorrectedRow
  
  return(dataset)
}

getDeltaTrueAndDeltaTruePrevForEachSample <- function(dataset, column, ...){
  
  deltaTrueForLastInjections <- dataset %>%
    rowid_to_column("rowNumber") %>%
    group_by(...) %>%
    slice(n()) %>%
    arrange(`rowNumber`) %>%
    select(deltaTrueApprox = column)
  
  deltaTrueAndDeltaTruePrevForLastInjections <- deltaTrueForLastInjections %>% 
    .$deltaTrueApprox %>% 
    lag() %>%
    add_column(deltaTrueForLastInjections, deltaTrueApproxPrev = .)
  
  deltasForAllRows <- inner_join(dataset, deltaTrueAndDeltaTruePrevForLastInjections)
  
  return(deltasForAllRows)
}