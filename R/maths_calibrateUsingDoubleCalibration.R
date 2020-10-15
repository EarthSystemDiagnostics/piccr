#' Calibrate data using double-block calibration
#'
#' Calibrate a given data set using calibration slope and intercept values which
#' are linearly interpolated between the first and the final block of standard
#' measurements within the measurement sequence.
#'
#' @param dataset a data frame with measurement data of a specific data set. It
#'   needs to contain the additional columns \code{block},
#'   \code{useForCalibration}, \code{o18_True} and \code{H2_True} which are not
#'   included in the raw Picarro output.
#' @param config a named list which needs to contain the following elements:
#' \describe{
#'   \item{\code{use_memory_correction}:}{logical; has a memory correction been
#'   applied to the input data?}
#'   \item{\code{use_three_point_calibration}:}{logical; shall three or more
#'     standards as specified by the data set column \code{useForCalibration} be
#'     used as calibration standards (\code{TRUE}) or only two (\code{FALSE})?}
#' }
#'
#' @return The input \code{dataset} with the d18O and dD values calibrated
#'   according to the double-block calibration.
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{associateStandardsWithConfigInfo}}
#' 
calibrateUsingDoubleCalibration <- function(dataset, config){

  finalBlock <- max(dataset$block, na.rm = TRUE)

  calibParamsBlock1 <- getCalibration(dataset, config, useBlock = 1)
  calibParamsBlockN <- getCalibration(dataset, config, useBlock = finalBlock)
  
  calibSlopes <- getCalibrationSlopes(calibParamsBlock1, calibParamsBlockN)
  
  calibratedDataset <- applyDoubleCalibration(dataset, calibParamsBlock1,
                                              calibSlopes)
  calibrationParams <- list(
    d18O = dplyr::bind_rows(calibParamsBlock1$d18O, calibParamsBlockN$d18O),
    dD   = dplyr::bind_rows(calibParamsBlock1$dD, calibParamsBlockN$dD)
  )

  return(list(dataset = calibratedDataset, parameter = calibrationParams))
}

#' Average measurement time of blocks
#'
#' This function calculates the average measurement time that has elapsed for
#' the specified standard blocks since the start of the measurement sequence.
#' 
#' @param dataset a data frame with measurement data of a specific data set. It
#'   needs to contain the additional column \code{block} which is not included
#'   in the raw Picarro output.
#' @param useBlocks an integer vector specifying the numbers of the standard
#'   blocks for which the average time shall be calculated.
#' @import dplyr
#'
#' @return A numeric vector of the same length as \code{useBlocks} with the
#'   average measurement time elapsed since start of the measurement for the
#'   respective blocks.
#' @seealso \code{\link{groupStandardsInBlocks}}
#' 
getCalibTimes <- function(dataset, useBlocks){
  
  addColumnSecondsSinceStart(dataset) %>%
    filter(block %in% useBlocks) %>%
    group_by(block) %>%
    summarise(time = mean(SecondsSinceStart)) %>%
    arrange(block) %>%
    .$time
}

#' Temporal change of calibration parameters
#'
#' Estimate the change in calibration parameters between the beginning and the
#' end of the measurement sequence based on a simple linear regression of the
#' calibration parameters against the elapsed measurement time.
#' @param paramsBlock1 the calibration parameters estimated from the first
#' standard block.
#' @param paramsBlockN the calibration parameters estimated from the final
#' standard block.
#' 
#' @return A named list with elements \code{d18O} and \code{dD} where each
#' element is again a list with two elements:
#' \describe{
#' \item{\code{alpha}:}{numeric; the slope of the linear change in the
#'   calibration intercept across the measurement sequence.}
#' \item{\code{beta}:}{numeric; the slope of the linear change in the
#'   calibration slope across the measurement sequence.}
#' }
#' 
getCalibrationSlopes <- function(paramsBlock1, paramsBlockN){
  
  timeDiffBetweenBlocks <- paramsBlockN$d18O$timeStamp - paramsBlock1$d18O$timeStamp
  
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

#' Apply double-block calibration
#'
#' Apply a double-block calibration to a specific data set using provided
#' calibration parameters.
#' @param dataset a data frame with measurement data of a specific data set.
#' @param calibParamsBlock1 a set of calibration parameters (slope and
#' intercept) for d18O and dD (see \code{\link{getCalibration}} for
#' details) as estimated from the first standard block in the measurement
#' sequence.
#' @param calibSlopes slope estimates for d18O and dD for a linear change of the
#' calibration parameters in \code{calibParamsBlock1} across the mesasurement
#' sequence (see \code{\link{getCalibrationSlopes}} for details).
#' @import dplyr
#' 
#' @return The input \code{dataset} with the d18O and dD values calibrated
#'   according to a time-varying calibration slope and intercept.
#' 
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
