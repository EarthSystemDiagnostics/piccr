#' @export
processData <- function(rawData){
  
  memoryDorrectedData <- correctForMemoryEffect(rawData)
  calibratedData <- calibrate(memoryCorrectedData)
  driftCorrectedData <- correctForDriftingPattern(calibratedData)
  
  invisible(list(memoryCorrected = memoryCorrectedData, 
                 calibrated = calibratedData, 
                 driftCorrected = driftCorrectedData))
}