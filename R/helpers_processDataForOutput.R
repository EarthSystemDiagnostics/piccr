library(tidyverse)

#' Obtain quality control information
#'
#' Obtain the quality control information for a data set based on the deviations
#' of the measured standards from their true values.
#' 
#' @param dataset a data frame with corrected and calibrated measurement data
#' of a specific data set.
#' @param accumulatedDataset a data frame with the accumulated
#' (i.e. injection-averaged) measurement data for this data set.
#' 
#' @return A list with three elements:
#'   $deviationsFromTrue (data frame with the deviations from the true value for
#'    each measured standard.)
#'   $rmsdDeviationsFromTrue (list with elements \code{d18O} and \code{dD} with
#'    the rmsd across \code{deviationsFromTrue} (very first standard excluded).)
#'   $deviationOfControlStandard (list with elements \code{Identifier 1},
#'   \code{d18O} and \code{dD} with the deviations from the true value for the
#'   quality control standard(s) in \code{Identifier 1}.) 
#' 
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

  deviationOfControlStandard <- deviationDataOfStandards %>%
    filter(useAsControlStandard == TRUE) %>%
    ungroup() %>%  # to remove grouping attributes
    select(d18O = d18ODeviation, dD = dDDeviation) %>%
    as.list()

  rmsdDeviationDataOfStandards <- deviationDataOfStandards %>%
    filter(Line > 1) %>%  # do not use very first standard for rmsd calculation
    ungroup() %>%
    summarise(d18O = calculateRMSD(d18OMeasured, d18OTrue),
              dD = calculateRMSD(dDMeasured, dDTrue)) %>%
    as.list()

  return(list(
    deviationsFromTrue = select(deviationDataOfStandards, -Line, -useAsControlStandard),
    pooledSD = calculatePoooledSD(dataset),
    rmsdDeviationsFromTrue = rmsdDeviationDataOfStandards,
    deviationOfControlStandard = deviationOfControlStandard
  ))

}

#' accumulate measurements
#'
#' Average the delta.O18, delta.H2 and d.Excess values for each 
#' sample from a dataset and calculate their standard deviation.
#' 
#' Uses the config parameter 'average_over_inj'. If it is
#' -1 or 'all', all injections are used to calculate the averages
#' and standard deviations.
#'
#' @param datasets A data frame with the data set.
#' @param config A named list. Needs to contain the component
#'               'average_over_inj'.
#'
#' @return A data frame.
#'
accumulateMeasurements <- function(dataset, config){

  accumulatedData <- dataset %>%
    filterInjections(config) %>%
    doAccumulate() %>% 
    rearrange()

  return(accumulatedData)
}

filterInjections <- function(dataset, config){
  
  n <- config$average_over_inj
  
  # exit early if all injections should be kept
  if (n %in% c(-1, "all")) return(dataset)
  
  # Convert n to number or vector of numbers
  n <- eval(parse(text = n))
  
  if (length(n) == 1)
    # use last n injections
    dataset %>%
      group_by(`Identifier 1`, block) %>% 
      slice((n() - n + 1):n()) %>%
      ungroup() %>%
      arrange(Line)
  else
    # n gives range of injections to use
    dataset %>%
      group_by(`Identifier 1`, block) %>% 
      slice(n) %>%
      ungroup() %>%
      arrange(Line)
}

doAccumulate <- function(dataset){
  
  dataset %>%
    group_by(`Identifier 1`, block) %>%
    summarise(`Identifier 2` = `Identifier 2`[[1]],
              delta.O18 = mean(`d(18_16)Mean`, na.rm = T),
              delta.H2 = mean(`d(D_H)Mean`, na.rm = T),
              sd.O18 = sd(`d(18_16)Mean`, na.rm = T),
              sd.H2 = sd(`d(D_H)Mean`, na.rm = T),
              d.Excess = mean(dExcess, na.rm = T),
              sd.d.Excess =
                  sqrt((sd(`d(D_H)Mean`, na.rm = T))^2 + 64 * (sd(`d(18_16)Mean`, na.rm = T)^2)),
              Line = min(Line))
}

rearrange <- function(dataset){
  
  dataset %>%
    arrange(Line) %>%  # preserve original order of samples
    select(-Line) %>%
    rowid_to_column("Line")  # use continous row numbers
}
