library(purrr)

groupStandardsInBlocks <- function(datasets, config){
  map(datasets, groupStandardsInBlocksForDataset, config = config)
}

groupStandardsInBlocksForDataset <- function(dataset, config){
  
  dataset <- add_column(dataset, block = NA)
  
  currBlock <- 0
  inBlock <- FALSE
  
  for(row in 1:nrow(dataset)){
    id1 = dataset[row, "Identifier 1"]
    if(isStandard(id1, config)){
      if(inBlock){
        dataset[row, "block"] <- currBlock
      } else {
        currBlock <- currBlock + 1
        inBlock <- TRUE
        dataset[row, "block"] <- currBlock
      }
    } else {
      dataset[row, "block"] <- NA
      inBlock <- FALSE
    }
  }
  return(dataset)
}

isStandard <- function(id1, config){
  id1 %in% map(config$STANDARDS, ~ .$NAME)
}