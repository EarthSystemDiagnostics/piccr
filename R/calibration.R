# ------------------------------------------------------------------------------
# I. simple linear calibration
# ------------------------------------------------------------------------------

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
#' @return A tibble with one row and at least seven variables:
#' \describe{
#' \item{\code{species}:}{character; the name of the isotope \code{species}
#'   used;}
#' \item{\code{intercept}:}{numeric; the estimated calibration intercept;}
#' \item{\code{slope}:}{numeric; the estimated calibration slope;}
#' \item{\code{pValueIntercept}:}{numeric; the p-value of the calibration
#'   intercept;}
#' \item{\code{pValueSlope}:}{numeric; the p-value of the calibration slope;}
#' \item{\code{residualRMSD}:}{numeric; the root mean square deviation of the
#'   calibration regression residuals;}
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
         residualRMSD = signif(calculateRMSD(modelSummary$residuals), 2),
         rSquared = signif(modelSummary$r.squared, 2))
  )
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
#' @return A tibble with two rows and at least seven variables where the first
#'   row is the output of \code{\link{runCalibrationModel}} for \code{d18O} and
#'   the second row the respective output for \code{dD}.
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{associateStandardsWithConfigInfo}},
#'   \code{\link{runCalibrationModel}}
#' 
getCalibration <- function(dataset, config, useBlock){
  
  trainingData <- getTrainingData(dataset, config, useBlock)
  calibTime    <- getCalibTimes(dataset, useBlocks = useBlock)

  dplyr::bind_rows(
    runCalibrationModel(trainingData, species = "d18O",
                        block = useBlock, timeStamp = calibTime),
    runCalibrationModel(trainingData, species = "dD",
                        block = useBlock, timeStamp = calibTime)
  )
}

#' Apply linear calibration
#'
#' Apply a linear calibration to a specific data set given calibration slope and
#' intercept values.
#' 
#' @param dataset a data frame with measurement data of a specific data set.
#' @param calibrationParams the calibration parameters in a tibble with two rows
#'   and three mandatory variables:
#'   \describe{
#'   \item{\code{species}:}{character; must be \code{d18O} for one row and
#'     \code{dD} for the other.}
#'   \item{\code{intercept}:}{numeric; the respective calibration intercept.}
#'   \item{\code{slope}:}{numeric; the respective calibration slope.}
#' }
#' @import dplyr
#'
#' @return The input \code{dataset} with the d18O and dD values calibrated
#'   according to the given calibration parameters.
#' 
applyCalibration <- function(dataset, calibrationParams){
  
  d18O <- calibrationParams %>% filter(species == "d18O")
  dD   <- calibrationParams %>% filter(species == "dD")
  
  calibratedDataset <- dataset %>%
    mutate(`d(18_16)Mean` = `d(18_16)Mean` * d18O$slope + d18O$intercept) %>%
    mutate(`d(D_H)Mean` = `d(D_H)Mean` * dD$slope + dD$intercept)
  
  return(calibratedDataset)
}

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
#' @return A named list with two elements:
#' \describe{
#' \item{dataset:}{the input \code{dataset} with the d18O and dD values
#'   calibrated according to the single-block calibration.}
#' \item{parameter:}{a tibble with the applied calibration parameters, and their
#'   associated quality control information, output from
#'   \code{\link{getCalibration}}.}
#' }
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{associateStandardsWithConfigInfo}}
#' 
linearCalibration <- function(dataset, config, block = 1){
  
  calibrationParams <- getCalibration(dataset, config, block)
  calibratedDataset <- applyCalibration(dataset, calibrationParams)
  
  return(list(dataset = calibratedDataset, parameter = calibrationParams))
}

# ------------------------------------------------------------------------------
# II. simple linear calibration with linear drift correction
# ------------------------------------------------------------------------------

#' Calibrate data using single-block calibration and linear drift correction
#' 
#' Calibrate a given data set using calibration slope and intercept values
#' estimated with standards from the beginning of the measurement sequence
#' following a linear drift correction against measurement time.
#'
#' @param dataset a data frame with measurement data of a specific data set. It
#'   needs to contain the additional columns \code{block},
#'   \code{useForCalibration}, \code{useForDriftCorr}, \code{o18_True} and
#'   \code{H2_True} which are not included in the raw Picarro output.
#' @inheritParams calibrateUsingDoubleCalibration
#' @import dplyr
#'
#' @return A named list with three elements:
#' \describe{
#' \item{dataset:}{the input \code{dataset} with the d18O and dD values
#'   calibrated according to linear drift correction and single calibration.}
#' \item{calibrationParameter:}{a tibble with the applied calibration
#'   parameters, and their associated quality control information, output from
#'   \code{\link{getCalibration}}.}
#' \item{driftParameter:}{a tibble with the applied drift correction parameters,
#'   and their associated quality control information, output from
#'   \code{\link{calculateDriftSlope}}.}
#' }
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{associateStandardsWithConfigInfo}},
#'   \code{\link{linearCalibration}},
#'   \code{\link{linearDriftCorrection}},
#'   \code{\link{getCalibration}},
#'   \code{\link{calculateDriftSlope}}
#' 
calibrateUsingSimpleDriftCorrection <- function(dataset, config){
  
  driftCorrection <- linearDriftCorrection(dataset, config)
  calibration     <- linearCalibration(driftCorrection$dataset,
                                       config, block = 1)

  return(list(
    dataset = calibration$dataset,
    calibrationParameter = calibration$parameter,
    driftParameter = driftCorrection$parameter))
}

# ------------------------------------------------------------------------------
# III. double-block calibration
# ------------------------------------------------------------------------------

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
