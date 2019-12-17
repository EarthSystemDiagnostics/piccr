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
#' @return The input \code{dataset} with the d18O and dD values calibrated
#'   according to drift correction and single calibration.
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{associateStandardsWithConfigInfo}}
#' 
calibrateUsingSimpleDriftCorrection <- function(dataset, config){
  
  dataset %>%
    linearDriftCorrection(config) %>%
    linearCalibration(config, block = 1)
}

#' Drift-correct data
#'
#' Correct a given data set for an assumed constant linear drift of the isotope
#' values with measurement time.
#' 
#' @param dataset a data frame with measurement data of a specific data set. It
#'   needs to contain the additional columns \code{useForDriftCorr} and
#'   \code{vial_group}, which are not included in the raw Picarro output.
#' @param config a named list with the logical component
#'   \code{use_memory_correction} to signal whether a memory correction has been
#' applied to the input data.
#' 
#' @return The input \code{dataset} with the d18O and dD values corrected for a
#'   constant linear drift.
#' @seealso \code{\link{associateStandardsWithConfigInfo}},
#'   \code{\link{assignVialsToGroups}}
#' 
linearDriftCorrection <- function(dataset, config){
  
  dataset <- addColumnSecondsSinceStart(dataset)
  alphaValues <- calculateDriftSlope(dataset, config)
  driftCorrectedData <- applyDriftCorrection(dataset, alphaValues)
  
  return(driftCorrectedData)
}

#' Calculate linear drift slope
#'
#' Calculate a linear drift slope for each isotope species from the average
#' across individual estimated drift slopes from the drift monitoring standards
#' specified in the configuration. If memory correction was applied to the data,
#' all injections are used from the drift monitoring standards, else only the
#' last three injections.
#' 
#' @inheritParams linearDriftCorrection
#' @import dplyr
#'
#' @return A named list with elements \code{d18O} and \code{dD} which contain
#'   the average drift slope in permil per seconds for d18O and dD.
#' @seealso \code{\link{associateStandardsWithConfigInfo}},
#'   \code{\link{assignVialsToGroups}}
#' 
calculateDriftSlope <- function(dataset, config){
  
  trainingData <- dataset %>%
    filter(useForDriftCorr == TRUE) %>%
    tidyr::drop_na(block)

  # if no memory correction is applied, use only the last three injections
  if (config$use_memory_correction == FALSE) {
    trainingData <- trainingData %>%
      group_by(`Identifier 1`, `vial_group`) %>%
      slice((n()-2):n()) %>%
      ungroup()
  }

  dataForEachStandard <- split(trainingData, trainingData$`Identifier 1`)
  
  # TODO: clean this code
  slopeD18O <- dataForEachStandard %>%
    purrr::map(function(x) lm(`d(18_16)Mean` ~ SecondsSinceStart, data = x)) %>%
    purrr::map_dbl(~ coef(.)[[2]]) %>%
    mean()
  
  slopeDD <- dataForEachStandard %>%
    purrr::map(function(x) lm(`d(D_H)Mean` ~ SecondsSinceStart, data = x)) %>%
    purrr::map_dbl(~ coef(.)[[2]]) %>%
    mean()
  
  list(d18O = slopeD18O, dD = slopeDD)
}

#' Apply drift correction
#'
#' Apply a linear drift correction to a specific data set given a constant
#' drift slope.
#' 
#' @param dataset a data frame with measurement data of a specific data set.
#' @param alpha numeric; a constant drift slope in permil per seconds.
#' @import dplyr
#'
#' @return The input \code{dataset} with the d18O and dD values corrected
#'   against a linear drift with slope \code{alpha}.
#' 
applyDriftCorrection <- function(dataset, alpha){
  
  dataset %>%
    mutate(`d(18_16)Mean` = `d(18_16)Mean` - alpha$d18O * SecondsSinceStart,
           `d(D_H)Mean` = `d(D_H)Mean` - alpha$dD * SecondsSinceStart) %>%
    select(-SecondsSinceStart)
}
