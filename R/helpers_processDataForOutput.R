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
    group_by(Sample) %>%
    summarise(d18OTrue = `o18_True`[[1]],
              dDTrue = `H2_True`[[1]],
              useAsControlStandard = useAsControlStandard[[1]])

  deviationDataOfStandards <- accumulatedDataset %>%
    inner_join(infoDataOnStd, by = "Sample") %>%
    mutate(d18ODeviation = d18OTrue - delta.O18,
           dDDeviation = dDTrue - delta.H2) %>%
    select(Sample, `Identifier 1`, block,
           d18OMeasured = delta.O18, d18OTrue, d18ODeviation,
           dDMeasured = delta.H2, dDTrue, dDDeviation,
           useAsControlStandard) %>%
    drop_na()

  deviationOfControlStandard <- deviationDataOfStandards %>%
    filter(useAsControlStandard == TRUE) %>%
    select(d18O = d18ODeviation, dD = dDDeviation) %>%
    as.list()

  vialCountOfFirstStd <- getVialCountOfFirstStd(dataset)

  rmsdDeviationDataOfStandards <- deviationDataOfStandards %>%
    filter(Sample > vialCountOfFirstStd) %>% # discard very first standard here
    summarise(d18O = calculateRMSD(d18OMeasured, d18OTrue),
              dD = calculateRMSD(dDMeasured, dDTrue)) %>%
    as.list()

  return(list(
    deviationsFromTrue = select(deviationDataOfStandards, -useAsControlStandard),
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
    doAccumulate()

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
      group_by(Sample) %>%
      slice((n() - n + 1):n()) %>%
      ungroup()
  else
    # n gives range of injections to use
    dataset %>%
      group_by(Sample) %>%
      slice(n) %>%
      ungroup()
}

doAccumulate <- function(dataset){
  
  dataset %>%
    group_by(Sample) %>%
    summarise(`Identifier 1` = `Identifier 1`[[1]],
              `Identifier 2` = `Identifier 2`[[1]],
              block = block[[1]],
              delta.O18 = mean(`d(18_16)Mean`, na.rm = T),
              delta.H2 = mean(`d(D_H)Mean`, na.rm = T),
              sd.O18 = sd(`d(18_16)Mean`, na.rm = T),
              sd.H2 = sd(`d(D_H)Mean`, na.rm = T),
              d.Excess = mean(dExcess, na.rm = T),
              sd.d.Excess =
                sqrt((sd(`d(D_H)Mean`, na.rm = T))^2 + 64 * (sd(`d(18_16)Mean`, na.rm = T)^2)))
}

getVialCountOfFirstStd <- function(dataset) {

  dataset %>%
    filter(`Identifier 1` == `Identifier 1`[[1]], vial_group == 1) %>%
    select(Sample) %>%
    max()
}
