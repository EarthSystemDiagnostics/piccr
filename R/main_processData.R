#' @export
processData <- function(datasets, config){
  
  if (config$use_memory_correction) {
    datasets <- correctForMemoryEffect(datasets)
  }
  
  if (config$calibration_method == 0){
    calibratedDatasets <- calibrateWithoutDriftCorrection(datasets, config)
  }
  else if (config$calibration_method == 1) {
    calibratedDatasets <- calibrateUsingSimpleDriftCorrection(datasets, config)
  } 
  else if (config$calibration_method == 2) {
    calibratedDatasets <- calibrateUsingDoubleCalibration(datasets, config)
  }
  
  processedData <- accumulateMeasurementsForEachSample(calibratedDatasets)
  pooledStdDev <- calculatePoooledStdDev(processedData)
  
  invisible(list(memoryCorrected = memoryCorrectedDatasets,
                 calibrated = calibratedDatasets,
                 processed = processedData,
                 pooledStdDev = pooledStdDev))
}