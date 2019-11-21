library(tidyverse)
library(lubridate)
library(yaml)

#' Read the specified YAML configuration file for piccr processing.
#' 
#' @param configFile A character string naming the config file.
#' 
#' @return A list.
#' 
parseConfig <- function(configFile){
  
  tryCatch({
    yaml::read_yaml(configFile)
  }, error = function(e) {
    stop("Error reading config file. Make sure that you specified the 
         correct path and that read permissions are given.")
  })
}

#' Read files
#' 
#' Read all files from a given input directory that have a 
#' given file extension. Only works for csv files.
#' 
#' The input directory and the file extension are given in
#' the config args 'input_directory' and 'file_extension'
#' respectively.
#'
#' @param config A named list. Needs to contain at least the#
#'               components 'input_directory' and 'file_extension'.
#'
#' @return A named list of dataframes. The names of the 
#'         list elements correspond to the file names in the
#'         input directory.
#'
readFiles <- function(config) {
  
  folder <- config$input_directory
  file_pattern <- str_c("*", config$file_extension)
  
  filenames <- list.files(path = folder, pattern = file_pattern)
  pathsToFiles <- file.path(folder, filenames)
  
  datasets <- map(pathsToFiles, read_csv)
  names(datasets) <- filenames
  
  return(datasets)
}

#' Re-calculate injection numbers
#'
#' Re-calculate injection numbers to account for probes being measured
#' from two or more consectuive vials.
#' 
#' @param datasets A data.frame.
#'                 
#' @return A data.frame. The column of injection numbers has been 
#'         re-calculated accounting for possible consecutive vials 
#'         of the same probe.
#'         
normalizeInjectionNumbers <- function(dataset) {
  
  dataset %>% 
    group_by(`Identifier 1`, block, `vial_group`) %>%
    mutate(`Inj Nr` = row_number()) %>%
    ungroup() %>%
    arrange(Line)
}

#' Associate standards with config info
#' 
#' Associate each standard in the given dataset with the 
#' config info for that standard (true values for 018 
#' and H2, use for drift correction, use for calibration).
#' 
#' The output dataset will contain the columns o18_True, H2_True,
#' useForDriftCorrection, and useForCalibration. The values will
#' be NA for probes.
#' 
#' @param dataset A data.frame.
#' @param config A named list. Needs to contain the component
#'               'standards'; a named list with the components
#'               'name', 'o18_True', 'H2_True', 'use_for_drift_correction',
#'               'use_for_calibration', and 'use_as_control_standard'.
#'
#' @return A data.frame.
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
#' For each standard injection, determine which standard block
#' it belongs to. The results are stored in the column 'block'.
#' For probes the value is NA.
#'
#' @param dataset A data.frame.
#' @param config A named list. Needs to contain the component
#'               $standards; a list of lists where each innermost
#'               list needs to contain the component $name.
#'
#' @return A data.frame.
#'
groupStandardsInBlocks <- function(dataset, config){
    
  dataset <- add_column(dataset, block = NA)
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

#' Add the column 'SecondsSinceStart' to the given dataset.
#' 
#' @param dataset A data.frame. Needs to contain the column
#'                'Time Code'. The elements of 'Time Code' should
#'                be character vectors of the format 'yyyy/mm/ddhh:mm:ss'
#'                (e.g. '2019/11/2510:00:00').
#'
#' @return A data.frame that includes the column 'SecondsSinceStart'.
#' 
addColumnSecondsSinceStart <- function(dataset){
  
  dataset %>%
    mutate(SecondsSinceStart = lubridate::ymd_hms(.$`Time Code`)) %>%
    mutate(SecondsSinceStart = c(0, lubridate::int_diff(.$SecondsSinceStart))) %>%
    mutate(SecondsSinceStart = cumsum(.$SecondsSinceStart))
}

#' Determine if a given ID belongs to a standard.
#'
#' @param id1 A character vector. The id to test for.
#' @param config A named list. Needs to contain the component 'standards'.
#'               'standards' is a list, each element needs to contain the
#'               component 'name'.
#'
#' @return A logical. 
#' 
isStandard <- function(id1, config){
  id1 %in% map(config$standards, ~ .$name)
}

#' Build output list
#' 
#' This is the final processing step for each dataset. After all the processing is done, input
#' all relevant information and construct the output for a single dataset for the function
#' processData(..).
#' 
#' Note: If you change the output of this function, don't forget to update
#' the roxygen docstring (section return value) of the function processData(..).
#'
#' @param config A list. Needs to contain the component $use_memory_correction, a logical.
#' @param dataset A data.frame.
#' @param memoryCorrected  A data.frame.
#' @param memoryCoefficients A data.frame.
#' @param calibrated A data.frame.
#' @param calibratedAndDriftCorrected A data.frame.
#' @param processedData A named list. Has the components $accumulatedData (a data.frame), 
#'                      $deviationsFromTrue (a data.frame), $rmsdDeviationsFromTrue
#'                      (a list), $pooledSD (a list), and $deviationOfControlStandard (a list).
#'
#' @return A nested list structure
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
#' data.frame counting the occurrence of groups of consecutive vials of the
#' same standard or probe across the measurement.
#' @param dataset a data.frame; needs to contain at least the columns
#' \code{Line} and \code{Identifier 1}.
#' @return the input data.frame appended by the column \code{vial_group}.
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
    add_column(vial_group = 1) %>%
    group_split(`Identifier 1`) %>%
    map(groupVials) %>%
    bind_rows() %>%
    arrange(Line)

  return(dataset)

}
