library(purrr)

#' groupStandardsInBlocks
#'
#' For each standard injection, determine which standard block
#' it belongs to. The results are stored in the column 'block'.
#' For probes the value is NA.
#'
#' @param datasets A named list of dataframes.
#' @param config A named list. Needs to contain the component
#'               $standards; a list of lists where each innermost
#'               list needs to contain the component $name.
#'
#' @return A named list of dataframes. The names are the same as
#'         the names of the input 'datasets'. 
#'
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
