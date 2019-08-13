library(magrittr)

#' @export
processData <- function(datasets, config){
  
  memoryCorrectedDatasets <- correctForMemoryEffectIfRequested(datasets, config)
  calibrationParameters   <- calculateCalibrationParameters(memoryCorrectedDatasets, config)
  calibratedDatasets      <- correctForDriftingPattern(memoryCorrectedDatasets, calibrationParameters, config)
  
  processedData <- accumulateMeasurementsForEachSample(calibratedDatasets)
  pooledStdDev <- calculatePoooledStdDev(processedData)
  
  invisible(list(memoryCorrected = memoryCorrectedDatasets,
                 calibrated = calibratedDatasets,
                 processed = processedData,
                 pooledStdDev = pooledStdDev))
}