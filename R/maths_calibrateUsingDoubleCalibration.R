library(tidyverse)

#' calibrateUsingDoubleCalibration
#' 
#' Calibrate the given datasets using double calibration
#'
#' @param datasets A named list of dataframes with isotope measurement data.
#'                 Each dataframe should contain the additional columns "block" and 
#'                 "useForMemCorr" (not included in the raw Picarro output).
#' @param config A named list. Need to contain the boolean elements "use_memory_correction"
#'               and "use_three_point_calibration".
#'
#' @return A list. The list elements are named like the input list "datasets". 
#'         Each element of the list is a list is a dataframe with calibrated data.
calibrateUsingDoubleCalibration <- function(datasets, config){
  
  map(datasets, doubleCalibrationForSingleDataset, config = config)
}

doubleCalibrationForSingleDataset <- function(dataset, config){
  
  calibParamsBlock1 <- getCalibInterceptAndSlope(dataset, config, useBlock = 1)
  calibParamsBlock3 <- getCalibInterceptAndSlope(dataset, config, useBlock = 3)
  calibTimes <- getCalibTimes(dataset)
  
  calibSlopes <- getCalibrationSlopes(calibParamsBlock1, calibParamsBlock3, calibTimes)
  
  applyDoubleCalibration(dataset, calibParamsBlock1, calibSlopes)
}

getCalibTimes <- function(dataset){
  
  addColumnSecondsSinceStart(dataset) %>%
    filter(block %in% c(1,3)) %>%
    group_by(block) %>%
    summarise(time = mean(SecondsSinceStart)) %>%
    arrange(block) %>%
    .$time
}

getCalibrationSlopes <- function(paramsBlock1, paramsBlock3, calibTimes){
  
  timeDiffBetweenBlocks <- calibTimes[[2]] - calibTimes[[1]]
  
  # TODO: clean this
  list(
    d18O = list(
      alpha = (paramsBlock3$d18O$intercept - paramsBlock1$d18O$intercept) / timeDiffBetweenBlocks,
      beta = (paramsBlock3$d18O$slope - paramsBlock1$d18O$slope) / timeDiffBetweenBlocks
    ),
    dD = list(
      alpha = (paramsBlock3$dD$intercept - paramsBlock1$dD$intercept) / timeDiffBetweenBlocks,
      beta = (paramsBlock3$dD$slope - paramsBlock1$dD$slope) / timeDiffBetweenBlocks
    )
  )
}

applyDoubleCalibration <- function(dataset, calibParamsBlock1, calibSlopes){
  
  d18OCalibSlope     <- calibParamsBlock1$d18O$slope
  d18OCalibIntercept <- calibParamsBlock1$d18O$intercept
  dDCalibSlope       <- calibParamsBlock1$dD$slope
  dDCalibIntercept   <- calibParamsBlock1$dD$intercept
  d18OAlpha          <- calibSlopes$d18O$alpha
  d18OBeta           <- calibSlopes$d18O$beta
  dDAlpha            <- calibSlopes$dD$alpha
  dDBeta             <- calibSlopes$dD$beta
  
  timeDependentInterceptAndSlope <- dataset %>%
    addColumnSecondsSinceStart() %>%
    mutate(d18OIntercept = d18OCalibIntercept + d18OAlpha * SecondsSinceStart,
           d18OSlope = d18OCalibSlope + d18OBeta * SecondsSinceStart,
           dDIntercept = dDCalibIntercept + dDAlpha * SecondsSinceStart,
           dDSlope = dDCalibSlope + dDBeta * SecondsSinceStart)
  
  calibratedDataset <- timeDependentInterceptAndSlope %>%
    mutate(`d(18_16)Mean` = d18OSlope * `d(18_16)Mean` + d18OIntercept,
           `d(D_H)Mean` = dDSlope * `d(D_H)Mean` + dDIntercept) %>%
    select(-dDSlope, -d18OSlope, -d18OIntercept, -SecondsSinceStart, -dDIntercept)
  
  return(calibratedDataset)
}