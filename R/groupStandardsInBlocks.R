groupStandardsInBlocks <- function(datasets, config){
  
  map(datasets, groupStandardsInBlocksForDataset, config = config)
  
}

groupStandardsInBlocksForDataset <- function(dataset, config){
  
  data <- mutate(dataset, is_std = is_standard(`Identifier 1`, config)) %>%
    add_column(block_nr = "probe")
  
  # TODO
}

is_standard <- function(id1, config){
  id1 %in% map(config$STANDARDS, ~ .$NAME)
}