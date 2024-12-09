#
# utils: HANDLING INJECTIONS/VIALS
# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
#
# utils: WORKING WITH STANDARDS
# ------------------------------------------------------------------------------

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
  
  left_join(x = dataset, y = configAsTable, by = "Identifier 1")
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
    
  dataset <- tibble::add_column(dataset, block = NA_integer_)
  currBlock <- 0
  inBlock <- FALSE
  
  for (irow in 1 : nrow(dataset)) {

    id1 <- dataset[irow, "Identifier 1"]

    if (isStandard(id1, config)) {

      if (inBlock) {

        dataset[irow, "block"] <- currBlock

      } else {

        currBlock <- currBlock + 1
        inBlock <- TRUE
        dataset[irow, "block"] <- currBlock
      }

    } else {

      dataset[irow, "block"] <- NA
      inBlock <- FALSE
    }
  }
  return(dataset)
}

#' Select lowest and highest standard
#'
#' From a data set of standards, select the two standards that exhibit the
#' lowest and highest isotope values.
#'
#' @param dataset a data frame with the isotopic data for a set of standards
#'   from a specific block.
#' @import dplyr
#' 
#' @return A data frame with all injections from the two selected standards.
#' 
selectStandardsForTwoPointCalib <- function(dataset){

  groups <- dataset %>%
    split(.$`Identifier 1`)

  orderedByIsotopeVal <- order(
    purrr::map_dbl(groups, ~ mean(.$`d(18_16)Mean`, na.rm = TRUE)))

  highestAndLowestStandard <- bind_rows(
    groups[c(orderedByIsotopeVal[1], utils::tail(orderedByIsotopeVal, 1))])

  return(highestAndLowestStandard)
}

#' Remove standards from data frame
#'
#' Remove those rows from a given data set which contain the data of the
#' measured standards.
#'
#' @param dataset a data frame with measurement data of a specific data set.
#' @param config a named list containing the component
#'   \code{include_standards_in_output} to signal whether the standard data
#'   shall be removed from \code{dataset}.
#'
#' @return The input \code{dataset} with the standard data removed if signalled
#'   in \code{config}.
#'
removeStandardsFromDataIfRequested <- function(dataset, config){
  if(!config$include_standards_in_output){
    return(dplyr::filter(dataset, !isStandard(`Identifier 1`, config)))
  }
  return(dataset)
}

# ------------------------------------------------------------------------------
#
# utils: GENERAL MATHS
# ------------------------------------------------------------------------------

#' Calculate pooled standard deviation
#'
#' Calculate the pooled standard deviation for d18O and dD for a given Picarro
#' data set, which provides a measure for the overall stability of consecutive
#' injections in the Picarro data.
#'
#' The pooled standard deviation provides a way to estimate the standard
#' deviation of several populations which may have different mean values but for
#' which you can assume that the standard deviation of each population is the
#' same. For Picarro data, the different populations are the individual samples
#' and we assume that the true standard deviation of the injections for a
#' specific sample is the same for all samples. The pooled standard deviation
#' \eqn{\sigma_p} for \eqn{k} samples is then calculated according to
#' \deqn{
#' x = (n_1 - 1) * \sigma_1^2 + ... + (n_k - 1) * \sigma_k^2
#' y = n_1 + ... + n_k - k
#' \sigma_p = sqrt(x / y)
#' }
#' where \eqn{n_i} and \eqn{\sigma_i} are the number of injections and the
#' standard deviation for sample \eqn{i}, respectively.
#'
#' @param dataset a data frame with measurement data of a specific data set.
#' @import dplyr
#'
#' @return A list with two elements \code{d18O} and \code{dD} with the pooled
#'   standard deviation for d18O and dD, respeectively.
#' @source https://en.wikipedia.org/wiki/Pooled_variance
#'
calculatePooledSD <- function(dataset){

   stdDevForEachSample <- dataset %>%
     group_by(`Identifier 1`, block, vial_group) %>%
     summarise(n = n(),
               sd.d18O = stats::sd(`d(18_16)Mean`, na.rm = TRUE),
               sd.dD = stats::sd(`d(D_H)Mean`), na.rm = TRUE) %>%
     ungroup()

   pooledStdDev <- stdDevForEachSample %>%
     mutate(summand.d18O = (n-1) * sd.d18O ^ 2,
            summand.dD = (n-1) * sd.dD ^ 2) %>%
     summarise(numerator.d18O = sum(summand.d18O, na.rm = TRUE),
               numerator.dD = sum(summand.dD, na.rm = TRUE),
               denominator = sum(n) - n()) %>%
     summarise(pooledStdDev.d18O = sqrt(numerator.d18O / denominator),
               pooledStdDev.dD = sqrt(numerator.dD / denominator))

  list(d18O = pooledStdDev$pooledStdDev.d18O,
       dD = pooledStdDev$pooledStdDev.dD)
}

#' Calculate root-mean-square deviation
#'
#' Calculate the root-mean-square deviation (rmsd) of two numeric vectors.
#'
#' @param v1 numeric vector for which to compute the rmsd with \code{v2}; if
#'   that is \code{NULL} (the default) the rmsd of \code{v1} relative to zero is
#'   calculated.
#' @param v2 numeric vector for which to compute the rmsd with \code{v1}
#'   (optional); if given, it must be of the same length as \code{v1}, if
#'   \code{NULL} (the default) the rmsd of \code{v1} relative to zero is
#'   calculated.
#' @param na.rm a logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds. Defaults to \code{FALSE}.
#'
#' @return The root-mean-square deviation of \code{v1} and \code{v2}, or
#'   \code{NA} (for \code{na.rm = FALSE}) if any of their elements is
#'   \code{NA}; or, if \code{v2} is \code{NULL}, the rmsd from zero of
#'   \code{v1}.
#'
calculateRMSD <- function(v1, v2 = NULL, na.rm = FALSE) {

  if (is.null(v2)) v2 <- rep(0, length(v1))

  if (length(v1) != length(v2)) {
    stop("Arguments must have the same length.")
  }
  res <- sqrt(mean((v1 - v2)^2, na.rm = na.rm))

  return(res)

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

#' Calculate d-excess
#'
#' Calculate the second-order parameter d-excess from the d18O and dD values of
#' a given data set according to \code{d-excess = dD - 8 * d18O}.
#'
#' @param dataset a data frame with measurement data of a specific data set.
#'
#' @return The input \code{dataset} supplemented by the column \code{dExcess}.
#'
addColumnDExcess <- function(dataset){

  dplyr::mutate(dataset, dExcess = `d(D_H)Mean` - `d(18_16)Mean` * 8)
}

#' Average measurement time of blocks
#'
#' This function calculates the average measurement time that has elapsed for
#' the specified standard blocks since the start of the measurement sequence.
#'
#' @param dataset a data frame with measurement data of a specific data set. It
#'   needs to contain the additional column \code{block} which is not included
#'   in the raw Picarro output.
#' @param useBlocks an integer vector specifying the numbers of the standard
#'   blocks for which the average time shall be calculated.
#' @import dplyr
#'
#' @return A numeric vector of the same length as \code{useBlocks} with the
#'   average measurement time elapsed since start of the measurement for the
#'   respective blocks.
#' @seealso \code{\link{groupStandardsInBlocks}}
#' 
getCalibTimes <- function(dataset, useBlocks){
  
  addColumnSecondsSinceStart(dataset) %>%
    filter(block %in% useBlocks) %>%
    group_by(block) %>%
    summarise(time = mean(SecondsSinceStart)) %>%
    arrange(block) %>%
    .$time
}

# ------------------------------------------------------------------------------
#
# utils: SAMPLE AVERAGING
# ------------------------------------------------------------------------------

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
                sqrt((stats::sd(`d(D_H)Mean`, na.rm = TRUE))^2 +
                     64 * (stats::sd(`d(18_16)Mean`, na.rm = TRUE)^2)))
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
