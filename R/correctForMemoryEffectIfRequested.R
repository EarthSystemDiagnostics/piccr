correctForMemoryEffectIfRequested <- function(datasets, config){
  
  if(config$use_memory_correction){
    return(map(datasets, correctSingleDatasetForMemoryEffect, config = config))
  } else {
    return(datasets)
  }
}

correctSingleDatasetForMemoryEffect <- function(dataset, config){
  # TODO
}