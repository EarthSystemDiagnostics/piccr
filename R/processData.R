#' @export
processData <- function(rawData){
  
  memoryCorrectedData <- correctForMemoryEffect(rawData)
  calibratedData      <- calibrate(memoryCorrectedData)
  driftCorrectedData  <- correctForDriftingPattern(calibratedData)
  
  invisible(list(memoryCorrected = memoryCorrectedData, 
                 calibrated = calibratedData, 
                 driftCorrected = driftCorrectedData))
}