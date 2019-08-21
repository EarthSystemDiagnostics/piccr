library(tidyverse)

#' correctForMemoryEffect
#' 
#' Take a list of dataframes with isotope measurement data and apply memory correction
#' to each dataset in the list.
#'
#' @param datasets A named list of data frames. Each dataframe is one Picarro
#'                 isotope file. Each dataframe should contain the column
#'                 block (not included in the raw Picarro output).
#'
#' @return A list. The list elements are named like the input list "datasets". 
#'         Each element of the list is a list with the two named elements
#'         "datasetMemoryCorrected" and "memoryCoefficients".
#' @export
correctForMemoryEffect <- function(datasets){
  
  map(datasets, correctSingleDatasetForMemoryEffect)
}

correctSingleDatasetForMemoryEffect <- function(dataset){
  
  memoryCoefficients <- calculateMemoryCoefficients(dataset)
  datasetMemoryCorrected <- applyMemoryCorrection(dataset, memoryCoefficients)
  
  return(list(datasetMemoryCorrected = datasetMemoryCorrected,
              memoryCoefficients = memoryCoefficients))
}

calculateMemoryCoefficients <- function(dataset) {
  
  block1 <- filter(dataset, block == 1)
  deltaTrueAndDeltaTruePrev <- getDeltaTrueAndDeltaTruePrevForEachSample(block1, `Identifier 1`)
  
  memoryCoefficients <- deltaTrueAndDeltaTruePrev %>%
    mutate(memoryCoeffD18O = (.$`d(18_16)Mean` - deltaTruePrevD18O) / (deltaTrueD18O - deltaTruePrevD18O),
           memoryCoeffDD = (.$`d(D_H)Mean` - deltaTruePrevDD) / (deltaTrueDD - deltaTruePrevDD)) %>%
    group_by(`Inj Nr`) %>%
    summarise(memoryCoeffD18O = mean(memoryCoeffD18O, na.rm = T), memoryCoeffDD = mean(memoryCoeffDD, na.rm = T))
  
  return(memoryCoefficients)
}

applyMemoryCorrection <- function(dataset, memoryCoefficients){
  
  deltaTrueAndDeltaTruePrev <- getDeltaTrueAndDeltaTruePrevForEachSample(dataset, `Identifier 1`, block)
  
  memoryCorrectedCols <- inner_join(deltaTrueAndDeltaTruePrev, memoryCoefficients, by = c("Inj Nr")) %>%
    transmute(memoryCorrectedD18O = .$`d(18_16)Mean` + (1-memoryCoeffD18O) / memoryCoeffD18O * (.$`d(18_16)Mean` - deltaTruePrevD18O),
              memoryCorrectedDD = .$`d(D_H)Mean` + (1-memoryCoeffDD) / memoryCoeffDD * (.$`d(D_H)Mean` - deltaTruePrevDD))
  
  dataset[["d(18_16)Mean"]] <- memoryCorrectedCols$memoryCorrectedD18O
  dataset[["d(D_H)Mean"]] <- memoryCorrectedCols$memoryCorrectedDD
  
  return(dataset)
}

getDeltaTrueAndDeltaTruePrevForEachSample <- function(dataset, ...){
  
  deltaTrue <- dataset %>%
    group_by(...) %>%
    slice((n()-2):n()) %>%
    summarise(deltaTrueD18O = mean(`d(18_16)Mean`),
              deltaTrueDD = mean(`d(D_H)Mean`), 
              Line = min(Line)) %>%
    arrange(Line) %>%
    select(-Line)
  
  deltaTruePrevD18O <- lag(deltaTrue$deltaTrueD18O)
  deltaTruePrevDD <- lag(deltaTrue$deltaTrueDD)
  
  deltaTrueAndDeltaTruePrev <- deltaTrue %>% 
    add_column(deltaTruePrevD18O = deltaTruePrevD18O, deltaTruePrevDD = deltaTruePrevDD)
  
  deltasForAllRows <- inner_join(dataset, deltaTrueAndDeltaTruePrev)
  
  return(deltasForAllRows)
}