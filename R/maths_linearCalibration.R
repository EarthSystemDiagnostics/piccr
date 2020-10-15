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
  
  calibrationParams <- getCalibration(dataset, config, block)
  calibratedDataset <- applyCalibration(dataset, calibrationParams)
  
  return(list(dataset = calibratedDataset, parameter = calibrationParams))
}

#' Get calibration parameters
#'
#' Obtain the calibration parameters as estimated from standards in a particular
#' standard block of the measurement sequence from regressing the expected
#' standard values against their measured values; for this, a simple linear
#' regression is currently implemented.
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
#' element is again a list containing the output of
#'   \code{\link{runCalibrationModel}}.
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{associateStandardsWithConfigInfo}},
#'   \code{\link{runCalibrationModel}}
#' 
getCalibration <- function(dataset, config, useBlock){
  
  trainingData <- getTrainingData(dataset, config, useBlock)
  calibTime    <- getCalibTimes(dataset, useBlocks = useBlock)

  list(
    d18O = runCalibrationModel(trainingData, species = "d18O",
                               block = useBlock, timeStamp = calibTime),
    dD   = runCalibrationModel(trainingData, species = "dD",
                               block = useBlock, timeStamp = calibTime)
  )
}

#' Run calibration model
#'
#' Run the calibration model of measured standard values against their true
#' values. Implemented is a simple linear regression.
#'
#' @param trainingData An input measurement dataset filtered by the standard
#'   block and the isotope standards which are to be used for the calibration.
#' @param species character string with the name of the isotope species for
#'   which the calibration model shall be calculated; valid names are "d18O" for
#'   oxygen isotopes and "dD" for hydrogen isotopes.
#' @param ... optional meta information on the calibration, such as block number
#'   and time stamp of the used measurement subset, passed on to the function
#'   output.
#' @return A tibble with one row and at least six variables:
#' \describe{
#' \item{\code{species}:}{character; the name of the isotope \code{species}
#'   used;}
#' \item{\code{intercept}:}{numeric; the estimated calibration intercept;}
#' \item{\code{slope}:}{numeric; the estimated calibration slope;}
#' \item{\code{pValueIntercept}:}{numeric; the p-value of the calibration
#'   intercept;}
#' \item{\code{pValueSlope}:}{numeric; the p-value of the calibration slope;}
#' \item{\code{rSquared}:}{numeric; the r-squared value of the calibration
#'   regression;}
#' }
#' plus the elements passed in \code{...}.
runCalibrationModel <- function(trainingData, species = "d18O", ...) {

  # params from inverse regression to have least noise on predictor variable

  if (species == "d18O") {
    model <- stats::lm(`d(18_16)Mean` ~ o18_True, data = trainingData)
  } else if (species == "dD") {
    model <- stats::lm(`d(D_H)Mean` ~ H2_True, data = trainingData)
  } else {
    stop("Unknown isotope species requested for calibration.", call. = FALSE)
  }

  modelSummary <- suppressWarnings(summary(model))
  coeffs <- stats::coef(modelSummary)

  tibble::as_tibble(
    list(species = species,
         ...,
         intercept = -1 * coeffs[1, 1] / coeffs[2, 1],
         slope = 1 / coeffs[2, 1],
         pValueIntercept = signif(coeffs[1, 4], 2),
         pValueSlope = signif(coeffs[2, 4], 2),
         rSquared = signif(modelSummary$r.squared, 2))
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
      slice((n() - 2) : n()) %>%
      ungroup() %>%
      arrange(Line)
  }
  
  # if two-point calibration is to be used, discard middle standards
  if (config$use_three_point_calibration == FALSE) {
    trainingData <- trainingData %>%
      selectStandardsForTwoPointCalib() %>%
      arrange(Line)
  }
  
  return(trainingData)
}

#' Select lowest and highest standard
#'
#' From a data set of standards, select the two standards that exhibit the
#' lowest and highest isotope values.
#'
#' @param dataset a data frame with the isotopic data for a set of standards
#'   from a specific block.
#' @import dplyr
#' 
#' @return A data frame with all injections from the two selected standards.
#' 
selectStandardsForTwoPointCalib <- function(dataset){

  groups <- dataset %>%
    split(.$`Identifier 1`)

  orderedByIsotopeVal <- order(
    purrr::map_dbl(groups, ~ mean(.$`d(18_16)Mean`, na.rm = TRUE)))

  highestAndLowestStandard <- bind_rows(
    groups[c(orderedByIsotopeVal[1], utils::tail(orderedByIsotopeVal, 1))])

  return(highestAndLowestStandard)
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
