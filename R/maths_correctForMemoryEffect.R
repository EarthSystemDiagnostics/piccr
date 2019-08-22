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
    mutate(memoryCoeffD18O = formulaMemCoeff(.$`d(18_16)Mean`, .$deltaTrueD18O, .$deltaTruePrevD18O),
           memoryCoeffDD = formulaMemCoeff(.$`d(D_H)Mean`, .$deltaTrueDD, .$deltaTruePrevDD))
  
  meanMemoryCoefficients <- memoryCoefficients %>%
    group_by(`Inj Nr`) %>%
    summarise(memoryCoeffD18O = mean(memoryCoeffD18O, na.rm = T), 
              memoryCoeffDD = mean(memoryCoeffDD, na.rm = T))
  
  return(meanMemoryCoefficients)
}

applyMemoryCorrection <- function(dataset, memoryCoefficients){
  
  deltaTrueAndDeltaTruePrev <- getDeltaTrueAndDeltaTruePrevForEachSample(dataset, `Identifier 1`, block)
  
  memoryCorrectedCols <- inner_join(deltaTrueAndDeltaTruePrev, memoryCoefficients, by = c("Inj Nr")) %>%
    transmute(memoryCorrectedD18O = formulaCorrectMem(.$`d(18_16)Mean`, .$memoryCoeffD18O, .$deltaTruePrevD18O),
              memoryCorrectedDD = formulaCorrectMem(.$`d(D_H)Mean`, .$memoryCoeffDD, .$deltaTruePrevDD))
  
  dataset[["d(18_16)Mean"]] <- memoryCorrectedCols$memoryCorrectedD18O
  dataset[["d(D_H)Mean"]] <- memoryCorrectedCols$memoryCorrectedDD
  
  return(dataset)
}

getDeltaTrueAndDeltaTruePrevForEachSample <- function(dataset, ...){
  
  lastThreeInj <- dataset %>%
    group_by(...) %>%
    slice((n()-2):n())
  
  deltaTrue <-lastThreeInj  %>%
    summarise(deltaTrueD18O = mean(`d(18_16)Mean`),
              deltaTrueDD = mean(`d(D_H)Mean`), 
              Line = min(Line))
  
  deltaTrueInCorrectOrder <- deltaTrue %>%
    arrange(Line) %>%
    select(-Line)
  
  deltaTruePrevD18O <- lag(deltaTrueInCorrectOrder$deltaTrueD18O)
  deltaTruePrevDD <- lag(deltaTrueInCorrectOrder$deltaTrueDD)
  
  deltaTrueAndDeltaTruePrev <- deltaTrueInCorrectOrder %>% 
    add_column(deltaTruePrevD18O = deltaTruePrevD18O, 
               deltaTruePrevDD = deltaTruePrevDD)
  
  deltasForAllRows <- inner_join(dataset, deltaTrueAndDeltaTruePrev)
  
  return(deltasForAllRows)
}

formulaMemCoeff <- function(data, deltaTrue, deltaTruePrev){
  (data - deltaTruePrev) / (deltaTrue - deltaTruePrev)
}

formulaCorrectMem <- function(data, memCoeff, deltaTruePrev){
  data + (1 - memCoeff) / memCoeff * (data - deltaTruePrev)
}
