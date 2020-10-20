#' Output quality control summary file
#' 
#' Write an output file to disc summarising the quality control information from
#' the processing steps for each processed file as well as for the entire
#' processing run. Note that if the data is processed again, the summary file is
#' silently overwritten.
#'
#' The summary file consists of up to six sections:
#' \itemize{
#'   \item the first section gives the overall values for the entire processing
#'         run of the root-mean-square deviation (rmsd) of the quality control
#'         standards, the rmsd of all used standards, and the mean value of the
#'         pooled standard deviation per sample;
#'   \item the second section contains tables with the above values for each
#'         processed measurement file;
#'   \item the third section contains the estimated memory coefficients and
#'         their standard deviation, as a function of the injection number, for
#'         each processed measurement file as well as for the average across all
#'         processed files.
#'   \item the fourth section contains information on the applied calibration
#'         regressions for each processed measurement file.
#'   \item the fifth section contains information on an applied linear drift
#'         correction for each measurement file. This is only printed if such a
#'         drift correction was actually applied to the measurement data.
#'   \item the sixth section contains a table which lists for each processed
#'         measurement file the individual deviations of each used standard from
#'         its true value.
#' }
#'
#' @param processedData the processed measurement data as output by
#'   \code{\link{processData}}.
#' @param config a named list with the contents read from the YAML configuration
#'   file, which was used for the processing; at least, it needs to contain the
#'   element \code{output_directory} to set the output directory for the summary
#'   file, if no explicit directory is given in the argument \code{outputFile},
#'   and the element \code{config_file_name} with the file path of the used YAML
#'   configuration file.
#' @param outputFile an optional character vector with an alternative file path
#'   (including file name) for the summary file. If \code{NULL} (the default),
#'   the file \code{run.info} in the directory \code{config$output_directory}
#'   is created.
#' @seealso \code{\link{processFiles}}
#' 
outputSummaryFile <- function(processedData, config, outputFile = NULL) {

  # set default value for outputFile if needed
  if (is.null(outputFile))
    outputFile <- file.path(config$output_directory, "run.info")

  runInfo <- utils::capture.output(printRunInfo(config$config_file_name)) %>%
    paste(collapse = "\n")

  qualityControl <- utils::capture.output(
    printQualityControl(processedData, printCalibrationParameter = TRUE,
                        printDriftParameter = TRUE,
                        printDeviations = TRUE, n = NA,
                        whichMemoryCoefficients = "all")) %>%
    paste(collapse = "\n")

  cat(runInfo, qualityControl, sep = "", file = outputFile)
}

#' Quality control information across processing run
#'
#' This is an auxiliary function to gather the various quality control
#' information determined for each processed measurement file across all such
#' files from a given processing run.
#'
#' @param datasets the processed measurement data as output by
#'   \code{\link{processFiles}} (or by \code{\link{processData}} directly).
#' @import dplyr
#'
#' @return A named list with seven elements:
#' \describe{
#'   \item{\code{rmsdQualityControl}:}{a tibble for the root-mean-square
#'   deviation (rmsd) of the quality control standard for each measurement file,
#'   listing the file name, the name(s) of the quality control standard, and the
#'   rmsd values for the oxygen and hydrogen isotope values.}
#'   \item{\code{rmsdAllStandards}:}{a tibble for the root-mean-square
#'   deviation (rmsd) of all measured standards for each measurement file,
#'   listing the file name and the rmsd values for the oxygen and hydrogen
#'   isotope values.}
#'   \item{\code{pooledSD}:}{a tibble for the pooled standard deviation across
#'   the samples measured in each measurement file, listing the file name and
#'   the pooled SD values for the oxygen and hydrogen isotope values.}
#'   \item{\code{deviationsFromTrue}:}{a list of tibbles, where each tibble
#'   lists the deviations of all measured standards from their true values for a
#'   processed measurement file.}
#'   \item{\code{memoryCoefficients}:}{a tibble of the mean and the SD of the
#'   average memory coefficients depending on the injection number per each
#'   processed measurement file and of the mean and the SD of the overall
#'   average memory coefficients.}
#'   \item{\code{calibrationParameter}:}{a tibble with information on the
#'   applied calibration regression, including calibration slope and intercept,
#'   p-values, r-squared value, and the root mean square deviation of the
#'   calibration residuals. The information are included for each estimated
#'   calibration, i.e., for each processed measurement file, and per file for
#'   each used calibration block and each isotope species.}
#'   \item{\code{driftParameter}:}{a tibble with information on an applied
#'   simple drift correction, or \code{NULL} if no such correction was applied
#'   to the measurement data. The drift correction information include the drift
#'   correction slope, its p-value, the r-squared value of the linear
#'   regression, and the root mean square deviation of the regression
#'   residuals. The information are included for each estimated drift
#'   correction, i.e., for each processed measurement file, and per file for
#'   each isotope species and each used drift monitoring standard as well as the
#'   average across those standards.}
#' }
gatherQualityControlInfo <- function(datasets) {

  rmsdQualityControl <- purrr::map_dfr(datasets, function(x) {
    tibble::tibble(dataset = x$name,
                   name = paste(x$deviationOfControlStandard$name, sep = ", "),
                   d18O = calculateRMSD(x$deviationOfControlStandard$d18O),
                   dD = calculateRMSD(x$deviationOfControlStandard$dD))})

  rmsdAllStandards <- purrr::map_dfr(datasets, function(x) {
    tibble::tibble(dataset = x$name,
                   d18O = x$rmsdDeviationsFromTrue$d18O,
                   dD = x$rmsdDeviationsFromTrue$dD)})

  pooledSD <- purrr::map_dfr(datasets, function(x) {
    tibble::tibble(dataset = x$name,
                   d18O = x$pooledSD$d18O,
                   dD = x$pooledSD$dD)})

  deviationsFromTrue <- lapply(datasets, function(x) {
    x$deviationsFromTrue %>%
      select(Sample, `Identifier 1`, block, d18ODeviation, dDDeviation)
  })

  memCoeffDatasets <- purrr::map_dfr(datasets, function(x) {
    tibble::tibble(dataset = x$name,
                   `Inj Nr` = x$memoryCoefficients$`Inj Nr`,
                   meanD18O = x$memoryCoefficients$`memoryCoeffD18O`,
                   meanDD = x$memoryCoefficients$`memoryCoeffDD`,
                   sdD18O = x$memoryCoefficients$`sdMemoryCoeffD18O`,
                   sdDD = x$memoryCoefficients$`sdMemoryCoeffDD`)})

  memCoeffMean <- memCoeffDatasets %>%
    group_by(`Inj Nr`) %>%
    summarise(dataset = "mean",
              meanD18O = mean(meanD18O),
              meanDD = mean(meanDD),
              sdD18O = sqrt(sum(sdD18O^2)) / n(),
              sdDD = sqrt(sum(sdDD^2)) / n()) %>%
    relocate("dataset", .before = `Inj Nr`)

  memoryCoefficients <- bind_rows(memCoeffMean, memCoeffDatasets)

  calibrationParameter <- purrr::map_dfr(datasets, function(x) {
    x$calibrationParams %>%
      dplyr::mutate(dataset = x$name) %>%
      dplyr::relocate("dataset", .before = "species")})

  driftParameter <- NULL
  if (!is.null(datasets[[1]]$driftParams)) {

    driftParameter <- purrr::map_dfr(datasets, function(x) {
      x$driftParams %>%
        dplyr::mutate(dataset = x$name) %>%
        dplyr::relocate("dataset", .before = "species")})
  }

  return(
    list(
      rmsdQualityControl = rmsdQualityControl,
      rmsdAllStandards = rmsdAllStandards,
      pooledSD = pooledSD,
      deviationsFromTrue = deviationsFromTrue,
      memoryCoefficients = memoryCoefficients,
      calibrationParameter = calibrationParameter,
      driftParameter = driftParameter
    )
  )
}

#' Print quality control information
#'
#' Print a summary of the quality control information for a piccr processing run
#' of several measurement files.
#'
#' Per default, the function prints the following three sections:
#' \itemize{
#'   \item the first section gives the overall values for the entire processing
#'         run of the root-mean-square deviation (rmsd) of the quality control
#'         standards, the rmsd of all used standards, and the mean value of the
#'         pooled standard deviation per sample;
#'   \item the second section contains tables with the above values for each
#'         processed measurement file.
#'   \item the third section contains a table of the overall memory
#'         coefficients depending on the injection number averaged across the
#'         processing run together with their estimated standard deviations.
#' }
#' Additional information can be switched on via the respective function
#' parameters, such as the deviations of all measured standards from their true
#' values, information on the calibration regressions, and information on the
#' drift correction, if applicable.
#'
#' @param datasets the processed measurement data as output by
#'   \code{\link{processFiles}} (or by \code{\link{processData}} directly).
#' @param printDeviations logical to control whether the specific deviations
#'   from the true value of each measured standard are printed for the processed
#'   measurement datasets (defaults to \code{FALSE}).
#' @param n integer to control the number of printed datasets if
#'   \code{printDeviations = TRUE}; default is to print the first three
#'   processed datasets, set to \code{NA} to print all of them.
#' @param printMemoryCoefficients logical to control whether the estimated
#'   memory coefficients shall be printed; defaults to \code{TRUE}.
#' @param whichMemoryCoefficients character string to signal which part of the
#'   estimated memory coefficients are printed if \code{printMemoryCoefficients
#'   = TRUE}. Possible options are "mean" (the default) to print only the values
#'   for the overall average memory coefficients or "all" to also print the
#'   values for the average memory coefficients per each processed measurement
#'   data set.
#' @param printCalibrationParameter logical to control whether information on
#'   the applied calibration regression shall be printed; defaults to
#'   \code{FALSE}.
#' @param printDriftParameter logical to control whether information on
#'   an applied drift correction shall be printed; defaults to \code{FALSE},
#'   and only prints when a drift correction was actually applied.
#' @import dplyr
#'
#' @return The input \code{datasets} are returned invisibly.
#' @examples
#' # Most conveniently, this function is combined with the processing function
#' # in a pipe:
#' \dontrun{
#'   library(magrittr)
#'   library(piccr)
#'
#'   processFiles("my_config_file.yaml") %>% printQualityControl()
#' }
#' @export
#'
printQualityControl <- function(datasets, printDeviations = FALSE, n = 3,
                                printMemoryCoefficients = TRUE,
                                whichMemoryCoefficients = "mean",
                                printCalibrationParameter = FALSE,
                                printDriftParameter = FALSE) {

  oldOpt <- options(width = 200)
  on.exit(options(width = oldOpt$width))

  qualityControlInfo <- gatherQualityControlInfo(datasets)

  cat("\n# ----------------------------------------------\n")
  cat("\n# --- Summary of quality control information ---\n")
  cat("\n# ----------------------------------------------\n")

  cat("\n# --- Average data for entire processing run ---\n")

  cat("\n# RMSD of quality control standards:\n")
  cat(sprintf("d18O = %1.2f, dD = %1.1f\n",
              calculateRMSD(qualityControlInfo$rmsdQualityControl$d18O),
              calculateRMSD(qualityControlInfo$rmsdQualityControl$dD)))

  cat("\n# RMSD of all standards:\n")
  cat(sprintf("d18O = %1.2f, dD = %1.1f\n",
              calculateRMSD(qualityControlInfo$rmsdAllStandards$d18O),
              calculateRMSD(qualityControlInfo$rmsdAllStandards$dD)))

  cat("\n# Pooled standard deviation:\n")
  cat(sprintf("d18O = %1.2f, dD = %1.1f\n",
              mean(qualityControlInfo$pooledSD$d18O),
              mean(qualityControlInfo$pooledSD$dD)))

  cat("\n# --- Specific data for each measurement file ---\n")

  cat("\n# RMSD of quality control standards:\n")
  print(qualityControlInfo$rmsdQualityControl,
        n = nrow(qualityControlInfo$rmsdQualityControl))

  cat("\n# RMSD of all standards:\n")
  print(qualityControlInfo$rmsdAllStandards)

  cat("\n# Pooled standard deviation:\n")
  print(qualityControlInfo$pooledSD)

  if (printMemoryCoefficients) {

    if (whichMemoryCoefficients == "mean") {

      x <- qualityControlInfo$memoryCoefficients %>%
        filter(dataset == "mean") %>%
        select(-dataset)

      cat("\n# --- Mean memory coefficients ---\n\n")
      print(x, n = nrow(x))

    } else if (whichMemoryCoefficients == "all") {

      x <- qualityControlInfo$memoryCoefficients

      cat("\n# --- Overall mean and file means of memory coefficients ---\n\n")
      print(x, n = nrow(x))

    } else {

      cat("\n")
      warning("Unknown option for printing memory coefficients; ",
              "select either 'all' or only the 'mean' memory coefficients.",
              call. = FALSE)
    }
  }

  if (printCalibrationParameter) {

    x <- qualityControlInfo$calibrationParameter

    cat("\n# --- Calibration parameter for each measurement file ---\n\n")
    print(x, n = nrow(x))
  }

  if (printDriftParameter & !is.null(x <- qualityControlInfo$driftParameter)) {

    cat("\n# --- Drift correction parameter for each measurement file ---\n\n")
    print(x, n = nrow(x))
  }

  if (printDeviations) {

    nmax <- length(qualityControlInfo$deviationsFromTrue)
    subset <- TRUE
    if (is.na(n) | n >= nmax) {
      n <- nmax
      subset <- FALSE
    }

    cat("\n# --- Specific deviations from true standard values ---\n\n")

    if (subset) {
      cat(sprintf(
        "# ... displaying output for first %i (of %i) measurement files;\n",
        n, nmax))
      cat("# adjust function parameter 'n' to display a different number.\n\n")
    }

    for (i in 1 : n) {
      x <- qualityControlInfo$deviationsFromTrue[[i]]
      cat(sprintf("Dataset: %s\n", datasets[[i]]$name))
      print(x, n = nrow(x))
      if (i != n) cat("\n")
    }
  }

  return(invisible(datasets))
}

#' Print piccr run information
#'
#' Auxiliary function to build the top section of the quality control summary
#' file printing information on the piccr version which was used for the
#' processing run, the name of the used YAML configuration file, and the
#' processing date.
#'
#' @param configFile the file name of the YAML configuration file which was
#'   used for the processing.
#' @seealso \code{\link{outputSummaryFile}}
#'
printRunInfo <- function(configFile) {

  cat(sprintf("piccr; version %s\n", utils::packageVersion("piccr")))
  cat(sprintf("* config file: %s\n", configFile))
  cat(sprintf("* processing date: %s\n\n", Sys.time()))

}
