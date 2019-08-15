correctForMemoryEffect <- function(datasets, config){

  return(map(datasets, correctSingleDatasetForMemoryEffect, config = config))

}

correctSingleDatasetForMemoryEffect <- function(dataset, config){
  
  memoryCoefficients <- calculateMemoryCoefficients(dataset)
  datasetMemoryCorrected <- applyMemoryCorrection(dataset, memoryCoefficients)
  
  return(datasetMemoryCorrected)
  
}


calculateMemoryCoefficients <- function(dataset) {
  
  block1 <- filter(dataset, block == 1)
  
  lastInjectionForEachStandard <- block1 %>% 
    group_by(`Identifier 1`) %>%
    slice(n()) %>%
    select(`Identifier 1` = `Identifier 1`, deltaTrueApprox = `d(18_16)Mean`)
  
  previousStandards <- lastInjectionForEachStandard %>% 
    .$deltaTrueApprox %>% 
    lag() %>%
    add_column(lastInjectionForEachStandard, deltaTrueApproxPrev = .)
  
  block1DataWithDeltaTrueApprox <- inner_join(block1, previousStandards)
  
  memoryCoefficients <- block1DataWithDeltaTrueApprox %>%
    mutate(memoryCoefficient = (`d(18_16)Mean` - deltaTrueApproxPrev) / (deltaTrueApprox - deltaTrueApproxPrev)) %>%
    drop_na() %>%
    group_by(`Inj Nr`) %>%
    summarise(memoryCoefficient = mean(memoryCoefficient))
  
  return(memoryCoefficients)
}

applyMemoryCorrection <- function(dataset, memoryCoefficients){
  
  lastInjectionForEachSample <- dataset %>% 
    group_by(`Identifier 1`, block) %>%
    slice(n()) %>%
    ungroup()
  lastInjectionForEachSample <-inner_join(dataset, lastInjectionForEachSample) %>%
    select(`Identifier 1` = `Identifier 1`, block = block, deltaTrueApprox = `d(18_16)Mean`)
  
  previousDeltaTrueApprox <- lastInjectionForEachSample %>% 
    .$deltaTrueApprox %>% 
    lag() %>%
    add_column(lastInjectionForEachSample, deltaTrueApproxPrev = .) %>%
    inner_join(dataset)
  
  memoryCorrectedRow <- inner_join(previousDeltaTrueApprox, memoryCoefficients, by = c("Inj Nr")) %>%
    transmute(memoryCorrected = `d(18_16)Mean` + (1-memoryCoefficient) / memoryCoefficient * (`d(18_16)Mean` - deltaTrueApproxPrev)) %>%
    .$memoryCorrected 
  
  datasetMemoryCorrected <- dataset %>%
    mutate(`d(18_16)Mean` = memoryCorrectedRow) %>%
    mutate(`d(18_16)Mean` = replace_na(`d(18_16)Mean`, lastInjectionForEachSample[[1,"deltaTrueApprox"]]))
  
  return(datasetMemoryCorrected)
}
