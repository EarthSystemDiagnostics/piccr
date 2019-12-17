#' Read the YAML configuration file
#' 
#' Read in the specified YAML configuration file for the \code{piccr}
#' processing.
#'
#' @param configFile a character string with the file path of the configuration
#' file.
#' 
#' @return A list of the parameters read from the configuration file.
#' 
parseConfig <- function(configFile){
  
  tryCatch({
    yaml::read_yaml(configFile)
  }, error = function(e) {
    stop("Error reading config file. Make sure that you specified the 
         correct path and that read permissions are given.")
  })
}

#' Read in measurement files
#' 
#' Read all files from a given input directory that match a given file
#' extension. Note that only csv files are supported.
#'
#' @param config A named list which needs to contain at least the
#'   components \code{input_directory} (the directory which contains the files
#'   to be read in) and \code{file_extension} (the file name extension to look
#'   for).
#'
#' @return A named list of data frames where the names of the list elements
#'   correspond to the file names in the input directory and where each data
#'   frame contains the data read in from the file.
#' 
readFiles <- function(config) {
  
  folder <- config$input_directory
  file_pattern <- stringr::str_c("*", config$file_extension)
  
  filenames <- list.files(path = folder, pattern = file_pattern)
  pathsToFiles <- file.path(folder, filenames)
  
  datasets <- purrr::map(pathsToFiles, readr::read_csv)
  names(datasets) <- filenames
  
  return(datasets)
}

#' Re-calculate injection numbers
#'
#' Re-calculate the injection numbers to account for probes being measured
#' from two or more consecutive vials.
#' 
#' @param dataset a data frame with measurement data of a specific data set.
#' @import dplyr
#'                 
#' @return The input \code{dataset} with the column of injection numbers
#'   re-calculated accounting for possible consecutive vials of the same probe.
#' 
normalizeInjectionNumbers <- function(dataset) {
  
  dataset %>% 
    group_by(`Identifier 1`, block, `vial_group`) %>%
    mutate(`Inj Nr` = row_number()) %>%
    ungroup() %>%
    arrange(Line)
}

#' Associate standards with configuration information
#' 
#' Associate each standard in the given dataset with the configuration
#' information for this standard, i.e. expected values for d18O and dH and the
#' flag parameters whether the standard shall be used for drift correction and
#' calibration or as a control standard. For normal samples, these values are
#' set to \code{NA}.
#' 
#' @param dataset a data frame with measurement data of a specific data set.
#' @param config A named list which needs to contain at least the component
#'   \code{standards} which is expected to be a named list of the following
#'   components:
#'   \describe{
#'   \item{\code{name}:}{character; the name of the standard as specified by the
#'     \code{Identifier 1} value in the data set.}
#'   \item{\code{o18_True}:}{numeric; the expected d18O value of the standard.}
#'   \item{\code{H2_True}:}{numeric; the expected dD value of the standard.}
#'   \item{\code{use_for_drift_correction}:}{logical; shall the standard be
#'     used to calculate the drift correction?}
#'   \item{\code{use_for_calibration}:}{logical; shall the standard be
#'     used for calibration?}
#'   \item{\code{use_as_control_standard}:}{logical: shall the standard be
#'     used as a quality control standard?}
#' }
#' @import dplyr
#'
#' @return The input \code{dataset} supplemented by the columns \code{o18_True},
#'   \code{H2_True}, \code{useForDriftCorrection}, \code{useForCalibration}, and
#'   \code{useAsControlStandard}.
#' 
associateStandardsWithConfigInfo <- function(dataset, config){
    
  configAsTable <- do.call(rbind, config$standards) %>%
    data.frame() %>%
    transmute(`Identifier 1` = as.character(name), 
              o18_True = as.double(o18_True), 
              H2_True = as.double(H2_True),
              useForDriftCorr = as.logical(use_for_drift_correction),
              useForCalibration = as.logical(use_for_calibration),
              useAsControlStandard = as.logical(use_as_control_standard))
  
  left_join(x = dataset, y = configAsTable)
}

#' Group standards in blocks
#'
#' Determine for each standard injection which standard block it belongs to.
#'
#' @param dataset a data frame with measurement data of a specific data set.
#' @param config A named list which needs to contain at least the component
#'   \code{standards} which is expected to be a list containing at least the
#'   component \code{name} giving the name for each used standard as specified
#'   by the \code{Identifier 1} column of the data set.
#'
#' @return The input \code{dataset} supplemented by the column \code{block}
#'   which counts the number of standard blocks across the measurement. For
#'   normal samples, this value is set to \code{NA}.
#' 
groupStandardsInBlocks <- function(dataset, config){
    
  dataset <- tibble::add_column(dataset, block = NA)
  currBlock <- 0
  inBlock <- FALSE
  
  for(row in 1:nrow(dataset)){
    id1 = dataset[row, "Identifier 1"]
    if(isStandard(id1, config)){
      if(inBlock){
        dataset[row, "block"] <- currBlock
      } else {
        currBlock <- currBlock + 1
        inBlock <- TRUE
        dataset[row, "block"] <- currBlock
      }
    } else {
      dataset[row, "block"] <- NA
      inBlock <- FALSE
    }
  }
  return(dataset)
}

#' Calculate seconds since start
#'
#' Calculate the seconds elapsed since the start of the measurement.
#'
#' The input data frame is expected to contain the column
#' \code{Time Code} with values as character vectors of the format
#' 'yyyy/mm/ddhh:mm:ss' (e.g. '2019/11/2510:00:00').
#' 
#' @param dataset a data frame with measurement data of a specific data set.
#' @import dplyr
#'
#' @return The input \code{dataset} supplemented by the column
#' \code{SecondsSinceStart} which gives the seconds elapsed since the start of
#' the measurement.
#' 
addColumnSecondsSinceStart <- function(dataset){
  
  dataset %>%
    mutate(SecondsSinceStart = lubridate::ymd_hms(.$`Time Code`)) %>%
    mutate(SecondsSinceStart = c(0, lubridate::int_diff(.$SecondsSinceStart))) %>%
    mutate(SecondsSinceStart = cumsum(.$SecondsSinceStart))
}

#' Determine if a given probe is a standard.
#'
#' Determine if a given probe is a standard based on its ID value.
#'
#' @param id1 character vector; the ID to test for.
#' @param config A named list which needs to contain at least the component
#'   \code{standards} which is expected to be a list containing at least the
#'   component \code{name} giving the name for each used standard as specified
#'   by the \code{Identifier 1} column of the data set.
#'
#' @return logical; \code{TRUE} if the requested ID is found in the
#'   \code{config} data base of standards, else \code{FALSE}.
#' 
isStandard <- function(id1, config){
  id1 %in% purrr::map(config$standards, ~ .$name)
}

#' Build output list
#' 
#' This function performs the final processing step for a specific dataset after
#' all corrections are done by collecting the relevant data and information and
#' returning them in a single output structure.
#' 
#' @note
#' If you change the output of this function, remember to update
#' the roxygen docstring (section return value) in `piccr_output.R`.
#'
#' @param name the file name of the data set.
#' @param config a named list containing the logical component
#'   \code{use_memory_correction} which specifies if memory correction was used
#'   in the processing.
#' @param dataset a data frame with the raw measurement data.
#' @param memoryCorrected a data frame with the memory-corrected measurement
#'   data.
#' @param memoryCoefficients a data frame of estimated memory coefficients.
#' @param calibrated a data frame with the (memory-corrected and) calibrated
#'   measurement data.
#' @param calibratedAndDriftCorrected a data frame with the (memory-corrected),
#'   calibrated and drift-corrected measurement data.
#' @param accumulated a data frame with the corrected and calibrated data
#'   averaged over a specified number of injections.
#' @param qualityControlInfo the output of \code{\link{getQualityControlInfo}}.
#' @inherit piccr_output return
#' @seealso \code{\link{processData}},
#'   \code{\link{calculateMemoryCoefficients}},
#'   \code{\link{correctForMemoryEffect}},
#'   \code{\link{linearCalibration}},
#'   \code{\link{calibrateUsingSimpleDriftCorrection}},
#'   \code{\link{calibrateUsingDoubleCalibration}},
#'   \code{\link{accumulateMeasurements}},
#'   \code{\link{getQualityControlInfo}}.
#' 
buildOutputList <- function(name, config, dataset, memoryCorrected, memoryCoefficients, 
                            calibrated, calibratedAndDriftCorrected, accumulated, qualityControlInfo){
  
  list(
    name = name,
    
    raw = dataset,
    memoryCorrected = if (config$use_memory_correction) memoryCorrected,
    calibrated = calibrated,
    calibratedAndDriftCorrected = if (config$calibration_method != 0) calibratedAndDriftCorrected,
    processed = accumulated,
    
    memoryCoefficients = if (config$use_memory_correction) memoryCoefficients,
    deviationsFromTrue = qualityControlInfo$deviationsFromTrue,
    rmsdDeviationsFromTrue = qualityControlInfo$rmsdDeviationsFromTrue,
    deviationOfControlStandard = qualityControlInfo$deviationOfControlStandard,
    pooledSD = qualityControlInfo$pooledSD,
    
    # TODO
    calibrationParams = NA,
    driftParams = NA
  )
}

#' Assign vial groups
#'
#' This function adds the additional column \code{vial_group} to the input
#' data frame counting the occurrence of groups of consecutive vials of the
#' same standard or sample across the measurement.
#' 
#' @param dataset a data frame with measurement data of a specific data set;
#'   needs to contain at least the columns \code{Line} and \code{Identifier 1}.
#' @import dplyr
#' 
#' @return The input \code{dataset} appended by the column \code{vial_group}.
#' 
assignVialsToGroups <- function(dataset) {

  groupVials <- function(sampleData) {

    counter_vial_group <- 1
    differenceInLineNumbers <- c(1, diff(sampleData$`Line`))

    for (row in 1 : nrow(sampleData)) {

      if (differenceInLineNumbers[row] > 1) {
        counter_vial_group <- counter_vial_group + 1
      }

      sampleData[row, "vial_group"] <- counter_vial_group

    }

    return(sampleData)
  }

  dataset <- dataset %>%
    tibble::add_column(vial_group = 1) %>%
    group_split(`Identifier 1`) %>%
    purrr::map(groupVials) %>%
    bind_rows() %>%
    arrange(Line)

  return(dataset)

}
