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
#' @return A named list with two elements:
#' \describe{
#' \item{dataset:}{the input \code{dataset} with the d18O and dD values
#'   calibrated according to the double-block calibration.}
#' \item{parameter:}{a tibble with the applied calibration parameters, and their
#'   associated quality control information; i.e. the output from
#'   \code{\link{getCalibration}} for the first and the final standard block of
#'   the measurement sequence bound together in a single tibble.}
#' }
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{associateStandardsWithConfigInfo}}
#' 
calibrateUsingDoubleCalibration <- function(dataset, config){

  finalBlock <- max(dataset$block, na.rm = TRUE)

  calibrationParams <- dplyr::bind_rows(
    getCalibration(dataset, config, useBlock = 1),
    getCalibration(dataset, config, useBlock = finalBlock)
  )

  calibratedDataset <- applyDoubleCalibration(dataset, calibrationParams)

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
#'
#' @param params a tibble with the calibration parameters for d18O and dD (see
#'   \code{\link{getCalibration}} for details on the tibble structure) as
#'   estimated from the first and the final standard block in the measurement
#'   sequence.
#' @import dplyr
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
getCalibrationSlopes <- function(params){

  getDifference <- function(data, species, var) {
    data %>%
      filter(species == {{species}}) %>%
      summarise(across({{var}}, diff)) %>%
      pull(1)
  }
  getSlopes <- function(species, data, timeDiff) {
    list(
      alpha = getDifference(data, {{species}}, "intercept") / timeDiff,
      beta = getDifference(data, {{species}}, "slope") / timeDiff)
  }

  timeDiffBetweenBlocks <- getDifference(params, "d18O", "timeStamp")

  species <- c("d18O", "dD")
  species %>%
    lapply(getSlopes, params, timeDiffBetweenBlocks) %>%
    stats::setNames(species)
}

#' Apply double-block calibration
#'
#' Apply a double-block calibration to a specific data set using provided
#' calibration parameters from the first and last standard block in the
#' measurement sequence, which are linearly interpolated in between.
#'
#' @param dataset a data frame with measurement data of a specific data set.
#' @param calibParams a tibble with the calibration parameters (slope and
#'   intercept) for d18O and dD (see \code{\link{getCalibration}} for details on
#'   the tibble structure) as estimated from the first standard block in the
#'   measurement sequence as well as from the last standard block.
#' @import dplyr
#' 
#' @return The input \code{dataset} with the d18O and dD values calibrated
#'   according to the time-varying calibration slope and intercept.
#' 
applyDoubleCalibration <- function(dataset, calibParams){

  getVar <- function(data, species, var) {
    (data %>% filter(species == {{species}}, block == 1))[[var]]
  }

  d18OCalibSlope     <- getVar(calibParams, "d18O", "slope")
  d18OCalibIntercept <- getVar(calibParams, "d18O", "intercept")
  dDCalibSlope       <- getVar(calibParams, "dD", "slope")
  dDCalibIntercept   <- getVar(calibParams, "dD", "intercept")

  calibSlopes <- getCalibrationSlopes(calibParams)

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
