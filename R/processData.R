library(magrittr)

#' @export
processData <- function(rawData, config){
  
  memoryCorrectedData    <- correctForMemoryEffectIfRequested(rawData, config)
  calibrationParameters  <- calculateCalibrationParameters(memoryCorrectedData, config)
  calibratedData         <- correctForDriftingPattern(memoryCorrectedData, calibrationParameters, config)
  
  processedData <- accumulateMeasurementsForEachSample(calibratedData)
  pooledStdDev <- calculatePoooledStdDev(processedData)
  
  invisible(list(memoryCorrected = memoryCorrectedData,
                 calibrated = calibratedData,
                 processed = processedData,
                 pooledStdDev = pooledStdDev))
}