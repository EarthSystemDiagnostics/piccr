#' @export
processData <- function(datasets, config){
  
  if (config$use_memory_correction) {
    memoryCorrected <- correctForMemoryEffect(datasets)
    memoryCorrectedDatasets <- map(memoryCorrected, ~ .$datasetMemoryCorrected)
  } else {
    memoryCorrected <- NULL
    memoryCorrectedDatasets <- datasets
  }
  
  if (config$calibration_method == 0){
    calibratedDatasets <- linearCalibration(memoryCorrectedDatasets, config)
  }
  else if (config$calibration_method == 1) {
    calibratedDatasets <- calibrateUsingSimpleDriftCorrection(memoryCorrectedDatasets, config)
  } 
  else if (config$calibration_method == 2) {
    calibratedDatasets <- calibrateUsingDoubleCalibration(memoryCorrectedDatasets, config)
  }
  
  processedData <- accumulateMeasurementsForEachSample(calibratedDatasets)
  processedData <- addColumnDExcess(processedData)  # d_excess = dH - 8 * d18O
  
  pooledStdDev <- calculatePoooledStdDev(processedData)
  
  list(memoryCorrected = memoryCorrected,
       calibrated = calibratedDatasets,
       processed = processedData,
       pooledStdDev = pooledStdDev)
}