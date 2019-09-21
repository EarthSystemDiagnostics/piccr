library(tidyverse)

#' Process data for output
#'
#' Calculate the injection average of the samples and obtain the quality control
#' information based on the true standard values.
#' 
#' Uses the config parameter 'average_over_last_n_inj'. If it is
#' -1 or 'all', all injections are used to calculate the sample averages.
#'
#' @param datasets A named list of data.frames
#' @param config A named list. Needs to contain the component
#'               'average_over_last_n_inj'.
#'
#' @return A named list of data.frames. The list elements are named 
#'         like the input list "datasets". 
#'
processDataForOutput <- function(datasets, config) {

  map(datasets, processSingleDatasetForOutput, config = config)
}

processSingleDatasetForOutput <- function(dataset, config) {

  accumulatedData <- accumulateMeasurementsForSingleDataset(dataset, config)

  return(accumulatedData)
}

getQualityControlInfo <- function(dataset, accumulatedDataset) {

  infoDataOnStd <- dataset %>%
    group_by(`Identifier 1`, block) %>%
    summarise(Line = min(Line),
              d18OTrue = `o18_True`[[1]],
              dDTrue = `H2_True`[[1]],
              useAsControlStandard = useAsControlStandard[[1]]) %>%
    rearrange()

  deviationDataOfStandards <- accumulatedDataset %>%
    inner_join(infoDataOnStd) %>%
    mutate(d18ODeviation = d18OTrue - delta.O18,
           dDDeviation = dDTrue - delta.H2) %>%
    select(Line, block,
           d18OMeasured = delta.O18, d18OTrue, d18ODeviation,
           dDMeasured = delta.H2, dDTrue, dDDeviation,
           useAsControlStandard) %>%
    drop_na()

  return(list(
    deviationsFromTrue = select(deviationDataOfStandards,
                                -Line, -useAsControlStandard)
  ))

}

#' accumulateMeasurementsForSingleDataset
#'
#' Average the delta.O18, delta.H2 and d.Excess values for each 
#' sample from a dataset and calculate their standard deviation.
#' 
#' Uses the config parameter 'average_over_last_n_inj'. If it is
#' -1 or 'all', all injections are used to calculate the averages
#' and standard deviations.
#'
#' @param datasets A data frame with the data set.
#' @param config A named list. Needs to contain the component
#'               'average_over_last_n_inj'.
#'
#' @return A data frame.
#'
accumulateMeasurementsForSingleDataset <- function(dataset, config){

  accumulatedData <- dataset %>%
    getLastNInjectionsForEachSample(config) %>%
    doAccumulate() %>% 
    rearrange()

  return(accumulatedData)
}

getLastNInjectionsForEachSample <- function(dataset, config){
  
  n <- config$average_over_last_n_inj
  
  # exit early if all injections should be kept
  if (n %in% c(-1, "all")) return(dataset)
  
  # don't fail if n is a string containing a number
  n <- as.numeric(n)
  
  dataset %>%
    group_by(`Identifier 1`, block) %>% 
    slice((n() - n + 1):n()) %>%
    ungroup() %>%
    arrange(Line)
}

doAccumulate <- function(dataset){
  
  dataset %>%
    group_by(`Identifier 1`, block) %>%
    summarise(`Identifier 2` = `Identifier 2`[[1]],
              delta.O18 = mean(`d(18_16)Mean`),
              delta.H2 = mean(`d(D_H)Mean`),
              sd.O18 = sd(`d(18_16)Mean`),
              sd.H2 = sd(`d(D_H)Mean`),
              d.Excess = mean(dExcess),
              sd.d.Excess =
                  sqrt((sd(`d(D_H)Mean`))^2 + 64 * (sd(`d(18_16)Mean`)^2)),
              Line = min(Line))
}

rearrange <- function(dataset){
  
  dataset %>%
    arrange(Line) %>%  # preserve original order of samples
    select(-Line) %>%
    rowid_to_column("Line")  # use continous row numbers
}
