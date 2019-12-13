#' Calibrate using simple drift correction
#' 
#' Apply calibration and drift correction to the input dataset. Uses
#' linear drift correction and linear calibration. The calibration uses
#' only the standards in block 1. 
#'
#' @param datasets A data.frame with isotope measurement data.
#'                 Should contain the additional columns "block", 
#'                 "useForDriftCorr", and "useForMemCorr" (not included in the 
#'                 raw Picarro output).
#' @param config A named list of config arguments. The required config arguments 
#'               are the same as for the function "linearDriftCorrection".
#' @import dplyr
#'
#' @return A data.frame.
#' 
calibrateUsingSimpleDriftCorrection <- function(dataset, config){
  
  dataset %>%
    linearDriftCorrection(config) %>%
    linearCalibration(config, block = 1)
}

linearDriftCorrection <- function(dataset, config){
  
  dataset <- addColumnSecondsSinceStart(dataset)
  alphaValues <- calculateDriftSlope(dataset, config)
  driftCorrectedData <- applyDriftCorrection(dataset, alphaValues)
  
  return(driftCorrectedData)
}

#' @import dplyr
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

#' @import dplyr
applyDriftCorrection <- function(dataset, alpha){
  
  dataset %>%
    mutate(`d(18_16)Mean` = `d(18_16)Mean` - alpha$d18O * SecondsSinceStart,
           `d(D_H)Mean` = `d(D_H)Mean` - alpha$dD * SecondsSinceStart) %>%
    select(-SecondsSinceStart)
}
