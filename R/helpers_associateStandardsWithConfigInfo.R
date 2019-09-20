library(tidyverse)

#' associateStandardsWithConfigInfo
#' 
#' Associate each standard in the given datasets with the 
#' config info for that standard (true values for 018 
#' and H2, use for drift correction, use for calibration).
#' 
#' The output datasets will contain the columns o18_True, H2_True,
#' useForDriftCorrection, and useForCalibration. The values will
#' be NA for probes.
#' 
#' @param datasets A named list of dataframes.
#' @param config A named list. Needs to contain the component
#'               'standards'; a named list with the components
#'               'name', 'o18_True', 'H2_True', 'use_for_drift_correction',
#'               'use_for_calibration', and 'use_as_control_standard'.
#'
#' @return A named list of dataframes. The names are equal to the names
#'         of the input 'datasets'.
#'
associateStandardsWithConfigInfo <- function(datasets, config){
  map(datasets, associateStandardsWithConfigInfoForDataset, config = config)
}

associateStandardsWithConfigInfoForDataset <- function(dataset, config){
  
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
