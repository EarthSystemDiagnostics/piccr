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
    calibratedDatasets <- linearCalibration(memoryCorrectedDatasets, config, block = 1)
  }
  else if (config$calibration_method == 1) {
    calibratedDatasets <- calibrateUsingSimpleDriftCorrection(memoryCorrectedDatasets, config)
  } 
  else if (config$calibration_method == 2) {
    calibratedDatasets <- calibrateUsingDoubleCalibration(memoryCorrectedDatasets, config)
  }
  
  calibratedDatasets <- addColumnDExcess(calibratedDatasets)
  processedData <- accumulateMeasurementsForEachSample(calibratedDatasets)
  
  pooledStdDev <- calculatePoooledStdDev(processedData)
  
  list(memoryCorrected = memoryCorrected,
       calibrated = calibratedDatasets,
       processed = processedData,
       pooledStdDev = pooledStdDev)
}