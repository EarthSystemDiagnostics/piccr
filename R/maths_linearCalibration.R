#' Calibrate data using single block
#'
#' Calibrate a given data set using calibration slope and intercept values
#' estimated with standards from a particular standard block of the measurement
#' sequence. The calibration parameters are obtained from regressing the
#' expected standard values against their measured values.
#'
#' If memory correction was applied to the data, all injections are used from
#' the standard data, else only the last three injections. If a two-point
#' calibration is switched on, only the two standards with the lowest and
#' highest isotopic values are used, regardless of how many calibration
#' standards are actually set in the dataset.
#'
#' @inheritParams calibrateUsingDoubleCalibration
#' @param block a single integer giving the number of the standard block which
#'   is to be used for estimating the calibration parameters; defaults to
#'   \code{1} (first block in the measurement sequence).
#'
#' @return The input \code{dataset} with the d18O and dD values calibrated
#'   according to the single-block calibration.
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{associateStandardsWithConfigInfo}}
#' 
linearCalibration <- function(dataset, config, block = 1){
  
  calibrationParams <- getCalibInterceptAndSlope(dataset, config, block)
  calibratedDataset <- applyCalibration(dataset, calibrationParams)
  
  return(calibratedDataset)
}

#' Get calibration parameters
#'
#' Obtain calibration slope and intercept values as estimated from standards in
#' a particular standard block of the measurement sequence from regressing the
#' expected standard values against their measured values.
#'
#' If memory correction was applied to the data, all injections are used from
#' the standard data, else only the last three injections. If a two-point
#' calibration is switched on, only the two standards with the lowest and
#' highest isotopic values are used, regardless of how many calibration
#' standards are actually set in the dataset.
#'
#' @param useBlock a single integer giving the number of the standard block which
#'   is to be used for estimating the calibration parameters.
#' @inheritParams linearCalibration
#'
#' @return A named list with elements \code{d18O} and \code{dD} where each
#' element is again a list with two elements:
#' \describe{
#' \item{\code{intercept}:}{numeric; the estimated intercept of the
#'   calibration.}
#' \item{\code{slope}:}{numeric; the estimated slope of the calibration.}
#' }
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{associateStandardsWithConfigInfo}}
#' 
getCalibInterceptAndSlope <- function(dataset, config, useBlock){
  
  trainingData <- getTrainingData(dataset, config, useBlock)

  # params from inverse regression to have least noise on predictor variable

  d18OModel <- lm(`d(18_16)Mean` ~ o18_True, data = trainingData)
  d18OIntercept <- -1 * coef(d18OModel)[[1]] / coef(d18OModel)[[2]]
  d18OSlope <- 1 / coef(d18OModel)[[2]]
  
  dDModel <- lm(`d(D_H)Mean` ~ H2_True, data = trainingData)
  dDIntercept <- -1 * coef(dDModel)[[1]] / coef(dDModel)[[2]]
  dDSlope <- 1 / coef(dDModel)[[2]]
  
  list(
    d18O = list(intercept = d18OIntercept, slope = d18OSlope),
    dD = list(intercept = dDIntercept, slope = dDSlope)
  )
}

#' Get calibration training data
#'
#' Obtain the standard data to be used for a regression of expected against
#' measured values in order to estimate calibration slope and intercept.
#'
#' If memory correction was applied to the data, all injections are used from
#' the standard data, else only the last three injections. If a two-point
#' calibration is switched on, only the two standards with the lowest and
#' highest isotopic values are used, regardless of how many calibration
#' standards are actually set in the dataset.
#'
#' @param useBlock a single integer giving the number of the standard block
#'   which contains the data of the requested standards.
#' @inheritParams linearCalibration
#' @import dplyr
#' 
#' @return The input \code{dataset} filtered by the requested standard block
#'   (\code{useBlock}) and by the standards set as calibration standards in
#'   \code{config}.
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{associateStandardsWithConfigInfo}}
#' 
getTrainingData <- function(dataset, config, useBlock) {
  
  trainingData <- filter(dataset, block == useBlock, useForCalibration == TRUE)
  
  # if no memory correction is applied, use only the last three injections for calibration
  if (config$use_memory_correction == FALSE) {
    trainingData <- trainingData %>% 
      group_by(`Identifier 1`, `vial_group`) %>%
      slice((n()-2):n()) %>% 
      ungroup()
  }
  
  # if two-point calibration is to be used, discard middle standard
  if (config$use_three_point_calibration == FALSE) {
    trainingData <- trainingData %>% 
      split(.$`Identifier 1`) %>% 
      selectGroupsForTwoPointCalib() %>%
      bind_rows()
  }
  
  return(trainingData)
}

#' Select lowest and highest standard
#'
#' From a group of standards, select the two standards that exhibit the lowest
#' and highest isotope values.
#'
#' @param groups a list of vectors from splitting a data frame block of
#'   standards by their \code{Identifier 1} values through calling
#'   \code{\link{split}}.
#' 
#' @return A data frame with all injections from the two selected standards.
#' 
selectGroupsForTwoPointCalib <- function(groups){

  orderedByIsotopeVal <- order(purrr::map_dbl(groups, ~ mean(.$`d(18_16)Mean`, na.rm = TRUE)))
  highestAndLowestIsotopeVal <- groups[c(orderedByIsotopeVal[1], tail(orderedByIsotopeVal, 1))]
  return(highestAndLowestIsotopeVal)
}

#' Apply linear calibration
#'
#' Apply a linear calibration to a specific data set given calibration slope and
#' intercept values.
#' 
#' @param dataset a data frame with measurement data of a specific data set.
#' @param calibrationParams a named list with elements \code{d18O} and \code{dD}
#'   where each element is again a list with two elements:
#'   \describe{
#'   \item{\code{intercept}:}{numeric; calibration intercept.}
#'   \item{\code{slope}:}{numeric; calibration slope.}
#' }
#' @import dplyr
#'
#' @return The input \code{dataset} with the d18O and dD values calibrated
#'   according to the given calibration parameters.
#' 
applyCalibration <- function(dataset, calibrationParams){
  
  d18OIntercept <- calibrationParams$d18O$intercept
  d18OSlope <- calibrationParams$d18O$slope
  dDIntercept <- calibrationParams$dD$intercept
  dDSlope <- calibrationParams$dD$slope
  
  calibratedDataset <- dataset %>%
    mutate(`d(18_16)Mean` = `d(18_16)Mean` * d18OSlope + d18OIntercept) %>%
    mutate(`d(D_H)Mean` = `d(D_H)Mean` * dDSlope + dDIntercept)
  
  return(calibratedDataset)
}
