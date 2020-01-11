#' Obtain quality control information
#'
#' Obtain the quality control information for a processed data set based on the
#' deviations of the measured standards from their expected values.
#' 
#' @param dataset a data frame with corrected and calibrated measurement data of
#'   a specific data set.
#' @param accumulatedDataset a data frame with the accumulated
#'   (i.e. injection-averaged) measurement data for this data set.
#' @import dplyr
#' 
#' @return A list with three elements:
#'   \describe{
#'   \item{\code{deviationsFromTrue}:}{data frame with the measured (and
#'     processsed) value (d18O and dH), the expected value, and the deviation of
#'     the measured from the expected value for each analysed standard.}
#'   \item{\code{rmsdDeviationsFromTrue}:}{a list with the two elements
#'     \code{d18O} and \code{dD} with the respective root mean square deviation
#'     across all deviations in \code{deviationsFromTrue} (with the very first
#'     standard of the measurement sequence excluded).}
#'   \item{\code{deviationOfControlStandard}:}{a list with the two elements
#'     \code{d18O} and \code{dD} of the deviations from the expected value for
#'     the quality control standard(s) specified in the config file.}
#'   \item{\code{pooledSD}:}{a list with the two elements \code{d18O}
#'     and \code{dD} of the pooled standard deviations across all measured
#'     vials.}
#' }
#' @seealso \code{\link{calculatePooledSD}}
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
    tidyr::drop_na()

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
    pooledSD = calculatePooledSD(dataset),
    rmsdDeviationsFromTrue = rmsdDeviationDataOfStandards,
    deviationOfControlStandard = deviationOfControlStandard
  ))

}

#' Accumulate measurements
#'
#' Calculate the average across a specified number of injections of the d18O,
#' dH and d-excess values for each measured sample of a dataset and calculate
#' the standard deviation of the means.
#' 
#' The functions uses the config parameter \code{average_over_inj}, which
#' specifies the injections to average. If it is -1 or 'all', all injections are
#' used, if it is set to a single integer `n`, the last `n` injections are used,
#' else if it is set to a range `n1:n2`, injections `n1:n2` are used.
#'
#' @param dataset a data frame with corrected and calibrated measurement data of
#'   a specific data set.
#' @param config A named list of configuration parameters (e.g. as read from the
#' \code{config.yaml} file) containing at least the component
#' \code{average_over_inj}.
#' @import dplyr
#'
#' @return A data frame with nine columns with the injection-averaged values of
#'   d18O, dH and d-excess together with their standard deviations as well as
#'   the respective \code{Identifier 1}, \code{Identifier 2} and \code{block}
#'   specifiers.
#' 
accumulateMeasurements <- function(dataset, config){

  accumulatedData <- dataset %>%
    filterInjections(config) %>%
    doAccumulate()

  return(accumulatedData)
}

#' Filter injection range
#'
#' Filter out a specified injection range from a measurement data frame.
#'
#' Filtering is done according to the configuration parameter
#' \code{average_over_inj} (e.g. see the \code{config.yaml} file). If the
#' parameter is set to \code{all} or -1, all injections are kept, if it is set
#' to a single integer `n`, the last `n` injections are kept, else if it is set
#' to a range `n1:n2`, injections `n1:n2` are kept.
#'
#' @param dataset a data frame with measurement data of a specific data set.
#' @param config A named list of configuration parameters (e.g. as read from the
#' \code{config.yaml} file) containing at least the component
#' \code{average_over_inj}.
#' @import dplyr
#'
#' @return The input data frame \code{dataset} with only the specified
#' injections remaining.
#' 
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

#' Average over injections
#'
#' This function takes an input data frame and returns the average across all
#' included injections for each sample and the respective standard deviations.
#'
#' @param dataset a data frame with measurement data of a specific data set.
#' @import dplyr
#'
#' @return A data frame with nine columns with the injection-averaged values of
#'   d18O, dH and d-excess together with their standard deviations as well as
#'   the respective \code{Identifier 1}, \code{Identifier 2} and \code{block}
#'   specifiers.
#' 
doAccumulate <- function(dataset){
  
  dataset %>%
    group_by(Sample) %>%
    summarise(`Identifier 1` = `Identifier 1`[[1]],
              `Identifier 2` = `Identifier 2`[[1]],
              block = block[[1]],
              delta.O18 = mean(`d(18_16)Mean`, na.rm = TRUE),
              delta.H2 = mean(`d(D_H)Mean`, na.rm = TRUE),
              sd.O18 = stats::sd(`d(18_16)Mean`, na.rm = TRUE),
              sd.H2 = stats::sd(`d(D_H)Mean`, na.rm = TRUE),
              d.Excess = mean(dExcess, na.rm = TRUE),
              sd.d.Excess =
                sqrt((stats::sd(`d(D_H)Mean`, na.rm = TRUE))^2 + 64 * (stats::sd(`d(18_16)Mean`, na.rm = TRUE)^2)))
}

#' Number of warm-up standard vials
#'
#' Get the number of vials the very first standard in the measurement sequence
#' (the "warm-up standard") is injected from, i.e. "vial grouping" is accounted
#' for, so if the first standard is injected from several vials in a row, the
#' number of the last of these vials is returned.
#'
#' @param dataset a data frame with measurement data of a specific data set.
#' @import dplyr
#'
#' @return A single integer with the number of the last vial of the very
#' first standard in the input \code{dataset}.
#' 
getVialCountOfFirstStd <- function(dataset) {

  dataset %>%
    filter(`Identifier 1` == `Identifier 1`[[1]], vial_group == 1) %>%
    select(Sample) %>%
    max()
}
