library(tidyverse)

#' calibrateWithoutDriftCorrection
#'
#' Take a list of dataframes with isotope measurement data and 
#' calibrate each dataframe. Note: 
#'  - If config$use_memory_correction is TRUE all injections are used for 
#'    calibration, else only the last three injections are used.
#'  - If config$use_triple_calibration is TRUE all standards in the specified
#'    block are used for calibration. If it is FALSE, only the first and last
#'    standards are used.
#'    
#' @param datasets A named list of dataframes with isotope measurement data.
#'                 Each dataframe should contain the columns "block" and 
#'                 "useForMemCorr" (not included in the raw Picarro output).
#' @param config A named list. Need to contain the boolean elements "use_memory_correction"
#'               and "use_triple_calibration".
#' @param block A number. Use the standards in this block for calibration. Default: 1.
#'
#' @return A list. The list elements are named like the input list "datasets". 
#'         Each element of the list is a list is a dataframe with calibrated data.
calibrateWithoutDriftCorrection <- function(datasets, config, block = 1){
  
  map(datasets, calibrateNoDriftSingleDataset, config = config, block = block)
}

calibrateNoDriftSingleDataset <- function(dataset, config, block){
  
  calibrationParams <- getCalibInterceptAndSlope(dataset, config, block)
  calibratedDataset <- applyCalibration(dataset, calibrationParams)
  
  return(calibratedDataset)
}

getCalibInterceptAndSlope <- function(dataset, config, useBlock){
  
  trainingData <- getTrainingData(dataset, config, useBlock)
  
  d18OModel <- lm(o18_True ~ `d(18_16)Mean`, data = trainingData)
  d18OIntercept <- coef(d18OModel)[[1]]
  d18OSlope <- coef(d18OModel)[[2]]
  
  dDModel <- lm(H2_True ~ `d(D_H)Mean`, data = trainingData)
  dDIntercept <- coef(dDModel)[[1]]
  dDSlope <- coef(dDModel)[[2]]
  
  list(
    d18O = list(intercept = d18OIntercept, slope = d18OSlope),
    dD = list(intercept = dDIntercept, slope = dDSlope)
  )
}

getTrainingData <- function(dataset, config, useBlock) {
  
  trainingData <- filter(dataset, block == useBlock, useForCalibration == TRUE)
  
  # if no memory correction is applied, use only the last three injections for calibration
  if (config$use_memory_correction == FALSE) {
    trainingData <- trainingData %>% 
      group_by(`Identifier 1`) %>% 
      slice((n()-2):n()) %>% 
      ungroup()
  }
  
  # if two-point calibration is to be used, discard middle standard
  if (config$use_three_point_calibration == FALSE) {
    trainingData <- trainingData %>% 
      split(.$`Identifier 1`) %>% 
      selectGroupsForTwoPointCalib() %>%
      bind_rows()
  }
  
  return(trainingData)
}

selectGroupsForTwoPointCalib <- function(groups){
  
  orderedByLine <- order(map_dbl(groups, ~ .$Line[[1]]))
  firstAndLastElement <- groups[c(orderedByLine[1], tail(orderedByLine, 1))]
  return(firstAndLastElement)
}
applyCalibration <- function(dataset, calibrationParams){
  
  d18OIntercept <- calibrationParams$d18O$intercept
  d18OSlope <- calibrationParams$d18O$slope
  dDIntercept <- calibrationParams$dD$intercept
  dDSlope <- calibrationParams$dD$slope
  
  calibratedDataset <- dataset %>%
    mutate(`d(18_16)Mean` = `d(18_16)Mean` * d18OSlope + d18OIntercept) %>%
    mutate(`d(D_H)Mean` = `d(D_H)Mean` * dDSlope + dDIntercept)
  
  return(calibratedDataset)
}