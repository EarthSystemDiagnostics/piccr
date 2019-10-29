library(tidyverse)

#' Correct for memory effect
#' 
#' Take a data.frame with isotope measurement data and apply memory correction to it.
#'
#' @param dataset A data.frame.
#'
#' @return A list. It contains the two named elements
#'         "datasetMemoryCorrected" and "memoryCoefficients".
#'
correctForMemoryEffect <- function(dataset){
  
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
  
  # get the mean mem coeff as a dataframe
  meanMemoryCoefficients <- memoryCoefficients %>%
    group_by(`Inj Nr`) %>%
    summarise(memoryCoeffD18O = mean(memoryCoeffD18O, na.rm = T), 
              memoryCoeffDD = mean(memoryCoeffDD, na.rm = T))
  
  # extract the mem coeff for each standard as a list of dataframes
  memoryCoeffForEachStandard <- memoryCoefficients %>% 
    select(`Inj Nr`, `Identifier 1`, memoryCoeffD18O, memoryCoeffDD) %>% 
    split(.$`Identifier 1`) 
  memoryCoeffForEachStandard <- map(names(memoryCoeffForEachStandard), ~ {
    data <- select(memoryCoeffForEachStandard[[.]], `Inj Nr`, memoryCoeffD18O, memoryCoeffDD)
    data <- select(memoryCoeffForEachStandard[[.]], `Inj Nr`, memoryCoeffD18O, memoryCoeffDD)
    setNames(data, c("Inj Nr", str_c(., "D18O"), str_c(., "DD")))
  })
  
  # join the mean mem coeff and the mem coeff for each standard into one dataframe
  tablesToJoin <- append(memoryCoeffForEachStandard, list(meanMemoryCoefficients))
  memCoeffOutput <- reduce(tablesToJoin, full_join, by = "Inj Nr")

  # remove all trailing rows from output dataframe which only contain NA values
  memCoeffOutput <- memCoeffOutput %>%
    filter(cumall(!is.na(memoryCoeffD18O)))
  
  return(memCoeffOutput)
}

applyMemoryCorrection <- function(dataset, memoryCoefficients){
  
  # get list with one dataframe per sample
  samples     <- group_split(dataset, `Identifier 1`, block)
  sampleOrder <- order(map_dbl(samples, ~ first(.$Line)))
  samples     <- samples[sampleOrder]
  
  # accumulate result in these vars
  memoryCorrectedO18      <- list()
  memoryCorrectedH2       <- list()
  memoryCorrectedO18[[1]] <- rep(NA, nrow(samples[[1]]))
  memoryCorrectedH2[[1]]  <- rep(NA, nrow(samples[[1]]))
  
  # Store state in these vars. Initialize to mean of last three injections of first sample.
  deltaTruePrevO18 <- mean(tail(samples[[1]]$`d(18_16)Mean`, 3), na.rm = T)
  deltaTruePrevH2  <- mean(tail(samples[[1]]$`d(D_H)Mean`, 3), na.rm = T)
  
  for (i in 2:length(samples)){
    
    sampleData <- samples[[i]]
    joinedData <- inner_join(sampleData, memoryCoefficients, by = c("Inj Nr"))
    
    # calculate memory corrected values for the current sample
    o18MemoryCorrected <- formulaCorrectMem(
      joinedData$`d(18_16)Mean`, joinedData$memoryCoeffD18O, deltaTruePrevO18)
    dDMemoryCorrected <- formulaCorrectMem(
      joinedData$`d(D_H)Mean`, joinedData$memoryCoeffDD, deltaTruePrevH2)
    
    # update delta true prev. If the new value is NA use the last known value.
    deltaTruePrevO18New <- mean(o18MemoryCorrected, na.rm = T)
    deltaTruePrevH2New  <- mean(dDMemoryCorrected, na.rm = T)
    if (!is.na(deltaTruePrevO18New)) deltaTruePrevO18 <- deltaTruePrevO18New
    if (!is.na(deltaTruePrevH2New)) deltaTruePrevH2 <- deltaTruePrevH2New
    
    # add memory corr values to result
    memoryCorrectedO18[[i]] <- o18MemoryCorrected
    memoryCorrectedH2[[i]]  <- dDMemoryCorrected
  }
  
  # create output dataframe and return it
  dataset[["d(18_16)Mean"]] <- unlist(memoryCorrectedO18)
  dataset[["d(D_H)Mean"]]   <- unlist(memoryCorrectedH2)
  return(dataset)
}

getDeltaTrueAndDeltaTruePrevForEachSample <- function(dataset, ...){
  
  lastThreeInj <- dataset %>%
    group_by(...) %>%
    slice((n()-2):n())
  
  deltaTrue <-lastThreeInj  %>%
    summarise(deltaTrueD18O = mean(`d(18_16)Mean`, na.rm = T),
              deltaTrueDD = mean(`d(D_H)Mean`, na.rm = T), 
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
