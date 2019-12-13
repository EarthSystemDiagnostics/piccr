#' linearCalibration
#'
#' Take a data.frame with isotope measurement data and 
#' calibrate it. Does not include drift correction.
#' Note: 
#'  - If config$use_memory_correction is TRUE all injections are used for 
#'    calibration, else only the last three injections are used.
#'  - If config$use_triple_calibration is TRUE all standards in the specified
#'    block are used for calibration. If it is FALSE, only the first and last
#'    standards are used.
#'    
#' @param dataset A data.frame with isotope measurement data. It dataframe should 
#'                contain the additional columns "block" and 
#'                "useForMemCorr" (not included in the raw Picarro output).
#' @param config A named list. Need to contain the boolean elements "use_memory_correction"
#'               and "use_three_point_calibration".
#' @param block A number. Use the standards in this block for calibration. Default: 1.
#'
#' @return A data.frame.
#' 
linearCalibration <- function(dataset, config, block = 1){
  
  calibrationParams <- getCalibInterceptAndSlope(dataset, config, block)
  calibratedDataset <- applyCalibration(dataset, calibrationParams)
  
  return(calibratedDataset)
}

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

#' @import dplyr
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

selectGroupsForTwoPointCalib <- function(groups){

  orderedByIsotopeVal <- order(purrr::map_dbl(groups, ~ mean(.$`d(18_16)Mean`, na.rm = TRUE)))
  highestAndLowestIsotopeVal <- groups[c(orderedByIsotopeVal[1], tail(orderedByIsotopeVal, 1))]
  return(highestAndLowestIsotopeVal)
}

#' @import dplyr
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
