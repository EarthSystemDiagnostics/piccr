library(tidyverse)

calibrateWithoutDriftCorrection <- function(datasets, config, block = 1){
  
  map(datasets, calibrateNoDriftSingleDataset, config = config, block = block)
}

calibrateNoDriftSingleDataset <- function(dataset, config, block){
  
  calibrationParams <- getCalibInterceptAndSlope(dataset, config, block)
  calibratedDataset <- applyCalibration(dataset, calibrationParams)
  
  return(calibratedDataset)
}

getCalibInterceptAndSlope <- function(dataset, config, useBlock){
  
  # TODO
  # - if memory correction is not used, use only last three injections for calibration
  # - two-point vs. three-point calibration
  
  trainingData <- filter(dataset, block == useBlock, useForMemCorr == TRUE)
  
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