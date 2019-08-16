library(tidyverse)

correctForMemoryEffect <- function(datasets){
  
  map(datasets, correctSingleDatasetForMemoryEffect)
}

correctSingleDatasetForMemoryEffect <- function(dataset){
  
  memoryCoefficients <- calculateMemoryCoefficients(dataset)
  datasetMemoryCorrected <- applyMemoryCorrection(dataset, memoryCoefficients)
  
  return(datasetMemoryCorrected)
}

calculateMemoryCoefficients <- function(dataset) {
  
  block1 <- filter(dataset, block == 1)
  deltaTrueAndDeltaTruePrev <- getDeltaTrueAndDeltaTruePrevForEachSample(block1, `Identifier 1`)
  
  memoryCoefficients <- deltaTrueAndDeltaTruePrev %>%
    mutate(memoryCoefficient = (`d(18_16)Mean` - deltaTrueApproxPrev) / (deltaTrueApprox - deltaTrueApproxPrev)) %>%
    drop_na() %>%
    group_by(`Inj Nr`) %>%
    summarise(memoryCoefficient = mean(memoryCoefficient))
  
  return(memoryCoefficients)
}

applyMemoryCorrection <- function(dataset, memoryCoefficients){
  
  deltaTrueAndDeltaTruePrev <- getDeltaTrueAndDeltaTruePrevForEachSample(dataset, `Identifier 1`, block)
  
  memoryCorrectedRow <- inner_join(deltaTrueAndDeltaTruePrev, memoryCoefficients, by = c("Inj Nr")) %>%
    transmute(memoryCorrected = `d(18_16)Mean` + (1-memoryCoefficient) / memoryCoefficient * (`d(18_16)Mean` - deltaTrueApproxPrev)) %>%
    .$memoryCorrected
  
  datasetMemoryCorrected <- dataset %>%
    mutate(`d(18_16)Mean` = memoryCorrectedRow)
  
  return(datasetMemoryCorrected)
}

getDeltaTrueAndDeltaTruePrevForEachSample <- function(dataset, ...){
  
  deltaTrueForLastInjections <- dataset %>%
    rowid_to_column("rowNumber") %>%
    group_by(...) %>%
    slice(n()) %>%
    arrange(`rowNumber`) %>%
    select(deltaTrueApprox = `d(18_16)Mean`)
  
  deltaTrueAndDeltaTruePrevForLastInjections <- deltaTrueForLastInjections %>% 
    .$deltaTrueApprox %>% 
    lag() %>%
    add_column(deltaTrueForLastInjections, deltaTrueApproxPrev = .)
  
  deltasForAllRows <- inner_join(dataset, deltaTrueAndDeltaTruePrevForLastInjections)
  
  return(deltasForAllRows)
}