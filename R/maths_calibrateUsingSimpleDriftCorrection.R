library(tidyverse)

#' calibrateUsingSimpleDriftCorrection
#' 
#' Apply calibration and drift correction to the input datasets. Uses
#' linear drift correction and linear calibration. The calibration uses
#' only the standards in block 1. 
#'
#' @param datasets A named list of dataframes with isotope measurement data.
#'                 Each dataframe should contain the additional columns "block", 
#'                 "useForDriftCorr", and "useForMemCorr" (not included in the 
#'                 raw Picarro output).
#' @param config A named list of config arguments. The required config arguments 
#'               are the same as for the function "linearDriftCorrection".
#'
#' @return A list. The list elements are named like the input list "datasets". 
#'         Each element of the list is a list is a dataframe with data that
#'         has been calibrated and drift corrected.
calibrateUsingSimpleDriftCorrection <- function(datasets, config){
  
  datasets %>%
    linearDriftCorrection() %>%
    linearCalibration(config, block = 1)
}

linearDriftCorrection <- function(datasets){
  map(datasets, linearDriftCorrectionSingleDataset)
}

linearDriftCorrectionSingleDataset <- function(dataset){
  
  dataset <- addColumnSecondsSinceStart(dataset)
  alphaValues <- calculateDriftSlope(dataset)
  driftCorrectedData <- applyDriftCorrection(dataset, alphaValues)
  
  return(driftCorrectedData)
}

calculateDriftSlope <- function(dataset){
  
  trainingData <- dataset %>%
    filter(useForDriftCorr == TRUE) %>%
    drop_na(block)
  
  dataForEachStandard <- split(trainingData, trainingData$`Identifier 1`)
  
  # TODO: clean this code
  slopeD18O <- dataForEachStandard %>%
    map(function(x) lm(`d(18_16)Mean` ~ SecondsSinceStart, data = x)) %>%
    map_dbl(~ coef(.)[[2]]) %>%
    mean()
  
  slopeDD <- dataForEachStandard %>%
    map(function(x) lm(`d(D_H)Mean` ~ SecondsSinceStart, data = x)) %>%
    map_dbl(~ coef(.)[[2]]) %>%
    mean()
  
  list(d18O = slopeD18O, dD = slopeDD)
}

applyDriftCorrection <- function(dataset, alpha){
  
  dataset %>%
    mutate(`d(18_16)Mean` = `d(18_16)Mean` - alpha$d18O * SecondsSinceStart,
           `d(D_H)Mean` = `d(D_H)Mean` - alpha$dD * SecondsSinceStart) %>%
    select(-SecondsSinceStart)
}