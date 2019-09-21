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

  # initialize output structure
  
  outputTemplate <- list(
    
    name = NULL,
    
    raw = NULL,
    memoryCorrected = NULL,
    calibrated = NULL,
    calibratedAndDriftCorrected = NULL,
    processed = NULL,

    deviationsFromTrue = NULL,
    rmsdDeviationsFromTrue = NULL,
    deviationOfControlStandard = NULL,
    pooledSD = NULL,
    memoryCoefficients = NULL,
    calibrationParams = NULL,
    driftParams = NULL
  )

  output <- list()
  for (i in 1 : length(datasets)) {
    output[[i]] <- outputTemplate
  }
 
  datasets <- datasets %>%
    groupStandardsInBlocks(config) %>%
    normalizeInjectionNumbers() %>%
    associateStandardsWithConfigInfo(config)
  
  if (config$use_memory_correction) {
    memoryCorrected <- correctForMemoryEffect(datasets)
    memoryCorrectedDatasets <- map(memoryCorrected, ~ .$datasetMemoryCorrected)
    memoryCoefficients <- map(memoryCorrected, ~ .$memoryCoefficients)
  } else {
    memoryCorrectedDatasets <- datasets
  }

  calibrated <- linearCalibration(memoryCorrectedDatasets, config, block = 1)
  
  if (config$calibration_method == 0){
    calibratedDatasets <- calibrated
  }
  else if (config$calibration_method == 1) {
    calibratedDatasets <- calibrateUsingSimpleDriftCorrection(memoryCorrectedDatasets, config)
  } 
  else if (config$calibration_method == 2) {
    calibratedDatasets <- calibrateUsingDoubleCalibration(memoryCorrectedDatasets, config)
  }
  
  calibratedDatasetsWithDExcess <- addColumnDExcess(calibratedDatasets)
  pooledStdDev <- calculatePoooledStdDev(calibratedDatasetsWithDExcess)
  
  processedData <- processDataForOutput(
    calibratedDatasetsWithDExcess, config)

  # fill output structure

  namesOfDatasets <- names(datasets)

  for (i in 1 : length(datasets)) {

    output[[i]]$name <- namesOfDatasets[i]
    
    output[[i]]$raw <- datasets[[i]]

    if (config$use_memory_correction) {
      output[[i]]$memoryCorrected <- memoryCorrectedDatasets[[i]]
      output[[i]]$memoryCoefficients <- memoryCoefficients[[i]]
    }

    output[[i]]$calibrated <- calibrated[[i]]
    output[[i]]$calibratedAndDriftCorrected <- calibratedDatasets[[i]]
    output[[i]]$processed <- processedData[[i]]

    output[[i]]$pooledSD <- pooledStdDev[[i]]
    
  }

  return(output)

}
    ## output[[i]]$calibrated <- NA
    ## output[[i]]$calibratedAndDriftCorrected <- NA

    ## output[[i]]$deviationsFromTrue <- NA
    ## output[[i]]$rmsdDeviationsFromTrue <- NA
    ## output[[i]]$deviationOfControlStandard <- NA
    ## output[[i]]$calibrationParams <- NA
    ## output[[i]]$driftParams <- NA
