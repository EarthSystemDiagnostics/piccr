#' processData
#'
#' The main interface for using piccr as a part of other programs. 
#' 
#' Implements the processing of water isotope data using memory correction,
#' calibration, and drift correction.
#' Uses the processing options set in the parameter 'config'.
#' Calculates quality indicators for each dataset such as delta excess and the pooled 
#' standard deviation.
#' 
#' @param datasets A named list of data.frames. The isotope measurement data
#'                 as output by the Picarro device.
#' @param config A named list. Required fiels: 
#'                   $average_over_last_n_inj (a number or "all")
#'                   $use_three_point_calibration (logical)
#'                   $calibration_method (1, 2, or 3)
#'                   $use_memory_correction (logical)
#'                   $standards (named list of $name (character), $o18_True (numerical), 
#'                               $H2_True (numerical), $use_for_drift_correction (logical), 
#'                               $use_for_calibration (logical)), $use_as_control_standard(logical)
#'
#' @return A list of the same length as \code{datasets}. Each list element is
#'   again a list of 13 elements with the following structure:
#'             $name (character vector with the name of the data set (file name))
#'             $raw (data frame with the raw input isotope data)
#'             $memoryCorrected (data frame of memory-corrected data)
#'             $calibrated (data frame of data calibrated without drift correction)
#'             $calibratedAndDriftCorrected (data frame of calibrated and drift-corrected data)
#'             $processed (data frame of final processed data)
#'             $deviationsFromTrue (data frame with the deviations from the true values for all measured standards)
#'             $rmsdDeviationsFromTrue (named list (elements \code{d18O} and \code{dD}) with the rmsd of deviationsFromTrue)
#'             $deviationOfControlStandard (named list (elements \code{d18O} and \code{dD}) with the deviation from the true value for the quality control standard)
#'             $pooledSD (named list (elements \code{d18O} and \code{dD}) of pooled standard deviations for the data set)
#'             $memoryCoefficients (data frame of memory coefficients)
#'             $calibrationParams (data frame of calibration regression parameters)
#'             $driftParams (data frame of drift regression parameters)
#' @export
#'
processData <- function(datasets, config){
  
  map(names(datasets), processSingleDataset, config = config, datasets = datasets)
}

processSingleDataset <- function(name, datasets, config){
  
  dataset <- datasets[[name]]
  
  # pre-process the input dataset
  preProcessed <- dataset %>%
    groupStandardsInBlocks(config) %>%
    normalizeInjectionNumbers() %>%
    associateStandardsWithConfigInfo(config)
  
  # apply memory correction if requested
  if (config$use_memory_correction) {
    temp <- correctForMemoryEffect(preProcessed)
    memoryCorrected    <- temp$datasetMemoryCorrected
    memoryCoefficients <- temp$memoryCoefficients
  } else {
    memoryCorrected <- preProcessed
  }
  
  # calibrate the memory corrected data. Only used for output.
  calibrated <- linearCalibration(memoryCorrected, config, block = 1)
  
  # apply calibration and drift correction based on the requested calibration method
  if (config$calibration_method == 0){
    calibratedAndDriftCorrected <- calibrated
  }
  else if (config$calibration_method == 1) {
    calibratedAndDriftCorrected <- calibrateUsingSimpleDriftCorrection(memoryCorrected, config)
  } 
  else if (config$calibration_method == 2) {
    calibratedAndDriftCorrected <- calibrateUsingDoubleCalibration(memoryCorrected, config)
  }
  
  # calculate the d-excess values for all samples
  calibratedWithDExcess <- addColumnDExcess(calibratedAndDriftCorrected)
  
  # accumulate data for each sample
  accumulated <- accumulateMeasurements(calibratedWithDExcess, config)
  
  # get quality control info
  qualityControlInfo <- getQualityControlInfo(calibratedWithDExcess, accumulated)

  # synthesize output list for this dataset and return it  
  output <- buildOutputList(name, config, dataset, memoryCorrected, memoryCoefficients, 
                            calibrated, calibratedAndDriftCorrected, accumulated, qualityControlInfo)
  return(output)
  
}