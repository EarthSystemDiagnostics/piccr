#' Process Picarro CRDS stable isotope data
#'
#' This is the main interface for using \code{piccr} as a part of other
#' programmes to correct and calibrate ("process") a number of similar Picarro
#' measurement data sets at once. It is is a wrapper for the function
#' \code{\link{processSingleDataset}} which works on a single data set to do the
#' actual processing.
#' 
#' This function implements processing of Picarro stable isotope data
#' including memory correction, drift correction, calibration, calculation of
#' d-excess values and assessment of information for quality control
#' monitoring. All processing options are set via parameters in the
#' \code{config} variable.
#' 
#' @param datasets a named list of data frames with the raw isotope measurement
#'   data as output by the Picarro device.
#' @param config a named list of the following required components:
#' \describe{
#' \item{\code{use_memory_correction}.}{logical; shall a memory correction be
#'   applied?}
#' \item{\code{calibration_method}:}{integer; which calibration method shall be
#'   used? Set to \code{0} for simple calibration without drift correction; set
#'   to \code{1} for simple calibration including linear drift correction; set
#'   to \code{2} for a double calibration performed at the beginning and the end
#'   of the measurement sequence including an inherent drift correction.}
#' \item{\code{use_three_point_calibration}:}{logical; shall three or more
#'   standards be used in the calibration (\code{TRUE}) or only two
#'   (\code{FALSE})?}
#' \item{\code{average_over_inj}:}{the number or range of injections to average
#'   over in the final processing step. Set to \code{-1} or "all" to use all
#'   available injections; set to a single integer `n` to use the last `n`
#'   injections; set to a range `n1:n2` to use the injection range `n1:n2`.}
#' \item{\code{standards}:}{a list the same length as the number of standards
#'   used in the measurement data sets; each element is itself a list with the
#'   following elements:
#'   \itemize{
#'   \item \code{name}: character; the name of the standard as specified by the
#'     \code{Identifier 1} value in the data set.
#'   \item \code{o18_True}: numeric; the expected d18O value of the standard.
#'   \item \code{H2_True}: numeric; the expected dD value of the standard.
#'   \item \code{use_for_calibration}: logical; shall the standard be used for
#'     calibration? 
#'   \item \code{use_for_drift_correction}: logical; shall the standard be used
#'     to calculate the drift correction? 
#'   \item \code{use_as_control_standard}: logical; shall the standard be used
#'     as a quality control standard?
#' }}
#' }
#'
#' @return A named list the same length as the input \code{datasets}; each list
#'   element contains the output of \code{\link{processSingleDataset}}.
#' @seealso \code{\link{processSingleDataset}}
#' @export
#' 
processData <- function(datasets, config){

  message(sprintf("\nRunning piccr version %s.\n",
                  utils::packageVersion("piccr")))

  names(datasets) %>%
    purrr::map(processSingleDataset, config = config, datasets = datasets) %>%
    stats::setNames(names(datasets))
}

#' Process a single Picarro CRDS stable isotope data set
#'
#' This function implements the processing of a single Picarro stable isotope
#' data set including memory correction, drift correction, calibration,
#' calculation of d-excess values and assessment of information for quality
#' control monitoring. All processing options are set via parameters in the
#' \code{config} variable.
#' @param name the name of the data set which shall be processed; must match one
#' of the actual names of \code{datasets}.
#' @inheritParams processData
#' @import dplyr
#' 
#' @inherit piccr_output return
#' 
processSingleDataset <- function(name, datasets, config){
  
  dataset <- datasets[[name]]
  
  # pre-process the input dataset
  preProcessed <- dataset %>%
    groupStandardsInBlocks(config) %>%
    assignVialsToGroups() %>%
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
  temp <- linearCalibration(memoryCorrected, config, block = 1)
  calibrated <- temp$dataset
  calibrationParameter <- temp$parameter
  
  # apply calibration and drift correction based on the requested calibration method
  if (config$calibration_method == 0){
    calibratedAndDriftCorrected <- calibrated
  }
  else if (config$calibration_method == 1) {
    temp <- calibrateUsingSimpleDriftCorrection(memoryCorrected, config)
    calibratedAndDriftCorrected <- temp$dataset
    calibrationParameter        <- temp$parameter
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
