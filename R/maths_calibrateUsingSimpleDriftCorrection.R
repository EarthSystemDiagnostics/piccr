calibrateUsingSimpleDriftCorrection <- function(datasets, config){
  
  map(datasets, calibrateUsingSimpleDriftCorrectionForDataset, config = config)
}

calibrateUsingSimpleDriftCorrectionForDataset <- function(dataset, config){
  
  driftRateAlpha <- determineDriftRate(dataset, config)
  datasetDriftCorrected <- correctForDriftingPattern(dataset, driftRateAlpha)
  calibrationSlopeAndIntercept <- calculateCalibrationParameters(datasetDriftCorrected, config)
  datasetCalibrated <- applyCalibration(datasetDriftCorrected, calibrationSlopeAndIntercept)
  
  return(datasetCalibrated)
}

determineDriftRate <- function(dataset, config){
  
}