library(tidyverse)

#' correctForMemoryEffect
#' 
#' Take a list of dataframes with isotope measurement data and apply memory correction
#' to each dataset in the list.
#'
#' @param datasets A named list of data frames. Each dataframe is one Picarro
#'                 isotope file. Each dataframe should contain the additional column
#'                 "block" (not included in the raw Picarro output).
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
  
  # get list with one dataframe per sample
  samples <- dataset %>%
    group_split(`Identifier 1`, block)
  sampleOrder <- order(map_dbl(samples, ~ first(.$Line)))
  samples <- samples[sampleOrder]
  
  # store state in these vars
  memoryCorrectedO18 <- list(rep(NA, nrow(samples[[1]])))
  memoryCorrectedH2 <- list(rep(NA, nrow(samples[[1]])))
  deltaTruePrevO18 <- mean(tail(samples[[1]]$`d(18_16)Mean`, 3), na.rm = T)
  deltaTruePrevH2 <- mean(tail(samples[[1]]$`d(D_H)Mean`, 3), na.rm = T)
  
  for (i in 2:length(samples)){
    
    sampleData <- samples[[i]]
    
    joinedData <- inner_join(sampleData, memoryCoefficients, by = c("Inj Nr"))
    o18MemoryCorrected <- formulaCorrectMem(
      joinedData$`d(18_16)Mean`, joinedData$memoryCoeffD18O, deltaTruePrevO18)
    dDMemoryCorrected <- formulaCorrectMem(
      joinedData$`d(D_H)Mean`, joinedData$memoryCoeffDD, deltaTruePrevH2)
    
    memoryCorrectedO18[[i]] <- o18MemoryCorrected
    memoryCorrectedH2[[i]]  <- dDMemoryCorrected
    
    deltaTruePrevO18 <- mean(o18MemoryCorrected, na.rm = T)
    deltaTruePrevH2  <- mean(dDMemoryCorrected, na.rm = T)
  }
  
  dataset[["d(18_16)Mean"]] <- unlist(memoryCorrectedO18)
  dataset[["d(D_H)Mean"]]   <- unlist(memoryCorrectedH2)
  
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
