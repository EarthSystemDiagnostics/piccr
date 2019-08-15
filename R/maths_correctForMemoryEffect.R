correctForMemoryEffect <- function(datasets, config){

  return(map(datasets, correctSingleDatasetForMemoryEffect, config = config))

}

correctSingleDatasetForMemoryEffect <- function(dataset, config){
  
  
}


calculateMemoryCoefficients <- function(dataset) {
  
  block1 <- filter(dataset, block == 1)
  
  lastInjectionForEachStandard <- block1 %>% 
    group_by(`Identifier 1`) %>%
    slice(n()) %>%
    select(`Identifier 1` = `Identifier 1`, deltaTrue = `d(18_16)Mean`)
  
  previousStandards <- lastInjectionForEachStandard %>% 
    .$deltaTrue %>% 
    lag() %>%
    add_column(lastInjectionForEachStandard, deltaTruePrev = .)
  
  memoryCoefficients <- block1 %>% 
    inner_join(previousStandards) %>%
    mutate(memoryCoefficient = (`d(18_16)Mean` - deltaTruePrev) / (deltaTrue - deltaTruePrev)) %>%
    drop_na() %>%
    group_by(`Inj Nr`) %>%
    summarise(mean = mean(memoryCoefficient))
  
  return(memoryCoefficients)
}
