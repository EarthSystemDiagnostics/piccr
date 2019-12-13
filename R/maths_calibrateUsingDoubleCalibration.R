#' Calibrate the given dataset using double calibration
#'
#' @param dataset A data.frame with isotope measurement data.
#'                Should contain the additional columns "block" and 
#'                "useForMemCorr" (not included in the raw Picarro output).
#' @param config A named list. Need to contain the boolean elements "use_memory_correction"
#'               and "use_three_point_calibration".
#'
#' @return A data.frame.
#' 
calibrateUsingDoubleCalibration <- function(dataset, config){

  finalBlock <- max(dataset$block, na.rm = TRUE)

  calibParamsBlock1 <- getCalibInterceptAndSlope(dataset, config, useBlock = 1)
  calibParamsBlockN <- getCalibInterceptAndSlope(dataset, config, useBlock = finalBlock)
  calibTimes <- getCalibTimes(dataset, useBlocks = c(1, finalBlock))
  
  calibSlopes <- getCalibrationSlopes(calibParamsBlock1, calibParamsBlockN, calibTimes)
  
  applyDoubleCalibration(dataset, calibParamsBlock1, calibSlopes)
}

#' @import dplyr
getCalibTimes <- function(dataset, useBlocks){
  
  addColumnSecondsSinceStart(dataset) %>%
    filter(block %in% useBlocks) %>%
    group_by(block) %>%
    summarise(time = mean(SecondsSinceStart)) %>%
    arrange(block) %>%
    .$time
}

getCalibrationSlopes <- function(paramsBlock1, paramsBlockN, calibTimes){
  
  timeDiffBetweenBlocks <- calibTimes[[2]] - calibTimes[[1]]
  
  # TODO: clean this
  list(
    d18O = list(
      alpha = (paramsBlockN$d18O$intercept - paramsBlock1$d18O$intercept) / timeDiffBetweenBlocks,
      beta = (paramsBlockN$d18O$slope - paramsBlock1$d18O$slope) / timeDiffBetweenBlocks
    ),
    dD = list(
      alpha = (paramsBlockN$dD$intercept - paramsBlock1$dD$intercept) / timeDiffBetweenBlocks,
      beta = (paramsBlockN$dD$slope - paramsBlock1$dD$slope) / timeDiffBetweenBlocks
    )
  )
}

#' @import dplyr
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
