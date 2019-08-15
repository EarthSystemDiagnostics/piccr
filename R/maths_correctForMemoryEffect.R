correctForMemoryEffect <- function(datasets, config){

  return(map(datasets, correctSingleDatasetForMemoryEffect, config = config))

}

correctSingleDatasetForMemoryEffect <- function(dataset, config){
  
  block1 <- filter(dataset, block == 1)
  
  lastInjectionForEachStandard <- block1 %>% 
    group_by(`Identifier 1`) %>%
    slice(n()) %>%
    select(`Identifier 1` = `Identifier 1`, deltaTrue = `d(18_16)Mean`)
  
  previousStandards <- lastInjectionForEachStandard %>% 
    .$deltaFromLastInjection %>% 
    lag() %>%
    add_column(lastInjectionForEachStandard, deltaTruePrev = .)
  
  memoryCorrectedDataset <- block1 %>% 
    inner_join(previousStandards) %>%
    mutate(`d(18_16)Mean` = (`d(18_16)Mean` - deltaTruePrev) / (deltaTruePrev - deltaTrue))
  
}
