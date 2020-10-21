#' Calibrate data using single-block calibration and linear drift correction
#' 
#' Calibrate a given data set using calibration slope and intercept values
#' estimated with standards from the beginning of the measurement sequence
#' following a linear drift correction against measurement time.
#'
#' @param dataset a data frame with measurement data of a specific data set. It
#'   needs to contain the additional columns \code{block},
#'   \code{useForCalibration}, \code{useForDriftCorr}, \code{o18_True} and
#'   \code{H2_True} which are not included in the raw Picarro output.
#' @inheritParams calibrateUsingDoubleCalibration
#' @import dplyr
#'
#' @return A named list with three elements:
#' \describe{
#' \item{dataset:}{the input \code{dataset} with the d18O and dD values
#'   calibrated according to linear drift correction and single calibration.}
#' \item{calibrationParameter:}{a tibble with the applied calibration
#'   parameters, and their associated quality control information, output from
#'   \code{\link{getCalibration}}.}
#' \item{driftParameter:}{a tibble with the applied drift correction parameters,
#'   and their associated quality control information, output from
#'   \code{\link{calculateDriftSlope}}.}
#' }
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{associateStandardsWithConfigInfo}},
#'   \code{\link{linearCalibration}},
#'   \code{\link{linearDriftCorrection}},
#'   \code{\link{getCalibration}},
#'   \code{\link{calculateDriftSlope}}
#' 
calibrateUsingSimpleDriftCorrection <- function(dataset, config){
  
  driftCorrection <- linearDriftCorrection(dataset, config)
  calibration     <- linearCalibration(driftCorrection$dataset,
                                       config, block = 1)

  return(list(
    dataset = calibration$dataset,
    calibrationParameter = calibration$parameter,
    driftParameter = driftCorrection$parameter))
}

#' Drift-correct data
#'
#' Correct a given data set for an assumed constant linear drift of the isotope
#' values with measurement time.
#' 
#' @param dataset a data frame with measurement data of a specific data set. It
#'   needs to contain the additional columns \code{useForDriftCorr} and
#'   \code{vial_group}, which are not included in the raw Picarro output.
#' @param config a named list with the logical component
#'   \code{use_memory_correction} to signal whether a memory correction has been
#' applied to the input data.
#' 
#' @return A named list with two elements:
#' \describe{
#' \item{dataset:}{the input \code{dataset} with the d18O and dD values
#'   corrected for a constant linear drift.}
#' \item{parameter:}{a tibble with the applied drift correction parameters, and
#'   their associated quality control information, output from
#'   \code{\link{calculateDriftSlope}}.}
#' }
#' @seealso \code{\link{associateStandardsWithConfigInfo}},
#'   \code{\link{assignVialsToGroups}},
#'   \code{\link{calculateDriftSlope}}
#'
linearDriftCorrection <- function(dataset, config){
  
  dataset <- addColumnSecondsSinceStart(dataset)
  driftParams <- calculateDriftSlope(dataset, config)
  driftCorrectedData <- applyDriftCorrection(dataset, driftParams)
  
  return(list(dataset = driftCorrectedData, parameter = driftParams))
}

#' Calculate linear drift slope
#'
#' Calculate a linear drift slope for each isotope species from the average
#' across individual estimated drift slopes from the drift monitoring standards
#' specified in the configuration. If memory correction was applied to the data,
#' all injections are used from the drift monitoring standards, else only the
#' last three injections.
#' 
#' @inheritParams linearDriftCorrection
#' @import dplyr
#'
#' @return A tibble with at least four rows and with six variables, where the
#'   first half of the rows is the output of \code{\link{runDriftModel}} for
#'   \code{d18O} and the second half the respective output for \code{dD}.
#' @seealso \code{\link{associateStandardsWithConfigInfo}},
#'   \code{\link{assignVialsToGroups}},
#'   \code{\link{runDriftModel}}
#' 
calculateDriftSlope <- function(dataset, config){
  
  trainingData <- dataset %>%
    filter(useForDriftCorr == TRUE) %>%
    tidyr::drop_na(block)

  # if no memory correction is applied, use only the last three injections
  if (config$use_memory_correction == FALSE) {
    trainingData <- trainingData %>%
      group_by(`Identifier 1`, `vial_group`) %>%
      slice((n()-2):n()) %>%
      ungroup()
  }

  dataForEachStandard <- split(trainingData, trainingData$`Identifier 1`)

  bind_rows(
    runDriftModel(dataForEachStandard, species = "d18O"),
    runDriftModel(dataForEachStandard, species = "dD")
  )
}

#' Run simple drift estimation model
#'
#' Run the simple drift estimation model of linearly regressing measured
#' standard values against the elapsed measurement time.
#'
#' @param trainingData a named list of input measurement datasets, where each
#'   dataset are the measurement data of a specific standard which is to be
#'   used for the drift estimation analysis. The names of the list should
#'   correspond to the names of the used standards.
#' @param species character string with the name of the isotope species for
#'   which the drift estimation model shall be calculated; valid names are
#'   "d18O" for oxygen isotopes and "dD" for hydrogen isotopes.
#' @import dplyr
#' @return A tibble with \code{n + 1} rows, where \code{n} is the number of used
#'   drift monitoring standards and the additional row corresponds to the mean
#'   estimate across all drift standards, and with six variables:
#' \describe{
#' \item{\code{species}:}{character; the name of the isotope \code{species}
#'   used;}
#' \item{\code{sample}:}{the name of the used standard, or "mean" for the
#'   average value across all analysed standards;}
#' \item{\code{slope}:}{numeric; the estimated drift correction slope in permil
#'   per second;}
#' \item{\code{pValueSlope}:}{numeric; the p-value of the drift correction
#'   slope (\code{NA} for the mean estimate);}
#' \item{\code{residualRMSD}:}{numeric; the root mean square deviation of the
#'   drift regression residuals (\code{NA} for the mean estimate);}
#' \item{\code{rSquared}:}{numeric; the r-squared value of the drift
#'   regression (\code{NA} for the mean estimate).}
#' }
runDriftModel <- function(trainingData, species = "d18O") {

  if (species == "d18O") {
    models <- trainingData %>%
      purrr::map(function(x) {
        stats::lm(`d(18_16)Mean` ~ SecondsSinceStart, data = x)})
  } else if (species == "dD") {
    models <- trainingData %>%
      purrr::map(function(x) {
        stats::lm(`d(D_H)Mean` ~ SecondsSinceStart, data = x)})
  } else {
    stop("Unknown isotope species requested for calibration.", call. = FALSE)
  }

  driftParPerStandard <- names(models) %>%
    purrr::map_dfr(function(name) {
      x <- suppressWarnings(summary(models[[name]]))
      tibble::tibble(
        species = species,
        sample = name,
        slope = stats::coef(x)[2, 1],
        pValue = signif(stats::coef(x)[2, 4], 2),
        residualRMSD = signif(calculateRMSD(x$residuals), 2),
        rSquared = signif(x$r.squared, 2))})

  bind_rows(
    driftParPerStandard,
    driftParPerStandard %>%
    summarise(species = species[[1]], sample = "mean",
              slope = mean(slope), pValue = NA,
              residualRMSD = NA, rSquared = NA))

}

#' Apply drift correction
#'
#' Apply a linear drift correction to a specific data set given a constant
#' drift slope.
#' 
#' @param dataset a data frame with measurement data of a specific data set.
#' @param driftParams the drift correction parameters in a tibble with a
#'   mandatory two rows and three variables:
#'   \describe{
#'   \item{\code{species}:}{character; must be \code{d18O} for one row and
#'     \code{dD} for the other.}
#'   \item{\code{sample}:}{character; must be \code{"mean"} once for
#'     \code{species = "d18O"} and \code{species = "dD"}, respectively,
#'     corresponding to the mean drift slopes which are applied for the drift
#'     correction.}
#'   \item{\code{slope}:}{numeric; the respective drift correction slopes in
#'     permil per second.}
#' }
#' @import dplyr
#'
#' @return The input \code{dataset} with the d18O and dD values corrected
#'   against linear drift according to the estimated drift slopes in
#'   \code{driftParams}.
#' 
applyDriftCorrection <- function(dataset, driftParams){

  d18O <- driftParams %>% filter(species == "d18O", sample == "mean")
  dD   <- driftParams %>% filter(species == "dD", sample == "mean")

  dataset %>%
    mutate(`d(18_16)Mean` = `d(18_16)Mean` - d18O$slope * SecondsSinceStart,
           `d(D_H)Mean` = `d(D_H)Mean` - dD$slope * SecondsSinceStart) %>%
    select(-SecondsSinceStart)
}
