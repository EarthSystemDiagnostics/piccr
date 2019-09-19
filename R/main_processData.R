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
#'                               $use_for_calibration (logical))
#'
#' @return A named, nested list. List structure:
#'             $memoryCorrected (list of lists, the names are the names 
#'                               of the input parameter 'datasets')
#'                 $datasetMemoryCorrected (data.frame)
#'                 $memoryCoefficients (data.frame)
#'             $calibrated (list of data.frames, the names are the names 
#'                          of the input parameter 'datasets'))
#'             $processed (list of data.frames, the names are the names 
#'                         of the input parameter 'datasets'))
#'             $pooledStdDev (list of lists, the names are the names 
#'                            of the input parameter 'datasets'))
#'                 $d18O (numerical)
#'                 $dD (numerical)
#' 
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
 
  datasets <- groupStandardsInBlocks(datasets, config) %>%
    associateStandardsWithConfigInfo(config)
  
  if (config$use_memory_correction) {
    memoryCorrected <- correctForMemoryEffect(datasets)
    memoryCorrectedDatasets <- map(memoryCorrected, ~ .$datasetMemoryCorrected)
  } else {
    memoryCorrected <- NULL
    memoryCorrectedDatasets <- datasets
  }
  
  if (config$calibration_method == 0){
    calibratedDatasets <- linearCalibration(memoryCorrectedDatasets, config, block = 1)
  }
  else if (config$calibration_method == 1) {
    calibratedDatasets <- calibrateUsingSimpleDriftCorrection(memoryCorrectedDatasets, config)
  } 
  else if (config$calibration_method == 2) {
    calibratedDatasets <- calibrateUsingDoubleCalibration(memoryCorrectedDatasets, config)
  }
  
  calibratedDatasets <- addColumnDExcess(calibratedDatasets)
  pooledStdDev <- calculatePoooledStdDev(calibratedDatasets)
  
  processedData <- accumulateMeasurementsForEachSample(calibratedDatasets, config)

  # fill output structure

  namesOfDatasets <- names(datasets)

  for (i in 1 : length(datasets)) {

    output[[i]]$name <- namesOfDatasets[i]
    
    output[[i]]$raw <- datasets[[i]]
    
    output[[i]]$processed <- processedData[[i]]
    
  }

  return(output)

}
    ## output[[i]]$memoryCorrected <- memoryCorrectedDatasets[[i]]
    ## output[[i]]$calibrated <- NA
    ## output[[i]]$calibratedAndDriftCorrected <- NA

    ## output[[i]]$deviationsFromTrue <- NA
    ## output[[i]]$rmsdDeviationsFromTrue <- NA
    ## output[[i]]$deviationOfControlStandard <- NA
    ## output[[i]]$pooledSD <- NA
    ## output[[i]]$memoryCoefficients <- NA
    ## output[[i]]$calibrationParams <- NA
    ## output[[i]]$driftParams <- NA
