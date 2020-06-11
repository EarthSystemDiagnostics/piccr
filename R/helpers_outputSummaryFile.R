#' outputSummaryFile
#' 
#' Write a summary file to disc. The summary file contains
#' quality indicators and consists of three sections: 
#'   - AVERAGE OVER ALL FILES: This section contains the
#'     pooled standard deviation (O18 and H2) averaged 
#'     over all files.
#'     (More parameters will be added in the future.)
#'   - VALUES FOR EACH FILE: This section contains the 
#'     pooled standard deviation (O18 and H2) for each file.
#'   - INTER STANDARD BIAS TO LITERATURE VALUES FOR EACH FILE:
#'     This section displays the deviation of the measured
#'     delta.O18 and delta.H2 values to the true values for
#'     each file.
#'
#' @param processedData A list as output by processData(..). 
#'                      It needs to contain the components
#'                      $pooledStdDev (a named list where each
#'                      element is of the form 'list(d18O = .., dD = ..)')
#'                      and $processed (a named list of dataframes).
#' @param config A named list. Needs to contain the component 
#'               $standards (A list of lists. Each innermost list 
#'               contains the components $name, $o18_True, and  $H2_True).
#'               Should contain $output_directory if no explicit
#'               value is given for the argument outputFile.
#' @param outputFile (Optional) A character vector. Path to the output file.
#'                   If it is not given, the file 'run.info' in the
#'                   directory config$output_directory is used.
#'
#' @return No relevant return value
#' 
outputSummaryFile <- function(processedData, config, configFile, outputFile = NULL){
  
  # set default value for outputFile if needed
  if(is.null(outputFile)) 
    outputFile <- file.path(config$output_directory, "run.info")

  runInfo <- utils::capture.output(printRunInfo(configFile)) %>%
    paste(collapse = "\n")

  qualityControl <- utils::capture.output(
    printQualityControl(processedData, printDeviations = TRUE, n = NA)) %>%
    paste(collapse = "\n")

  cat(runInfo, qualityControl, sep = "", file = outputFile)

  ## firstSection  <- buildFirstSection(processedData)
  ## secondSection <- buildSecondSection(processedData)
  ## thirdSection  <- buildThirdSection(processedData, config)

  ## summaryText <- stringr::str_c(
  ##   firstSection,
  ##   "\n\n",
  ##   secondSection,
  ##   "\n\n",
  ##   thirdSection
  ## )

  ## readr::write_file(summaryText, outputFile)
}

#' Build section "AVERAGE OVER ALL FILES"
#'
#' @param processedData a list as output by \code{processData()}.
buildFirstSection <- function(processedData){
  
  meanPooledSdO18 <- mean(purrr::map_dbl(processedData$pooledStdDev, ~ .$d18O))
  meanPooledSdH2  <- mean(purrr::map_dbl(processedData$pooledStdDev, ~ .$dD))
  
  sprintf(
    stringr::str_c("### AVERAGE OVER ALL FILES: ###\n\n",
                   "pooled standard deviation delta O18: %.2f\n",
                   "pooled standard deviation delta H2: %.2f"),
    meanPooledSdO18, meanPooledSdH2
  )
}

#' Build section "VALUES FOR EACH FILE"
#'
#' @param processedData a list as output by \code{processData()}.
#' @import dplyr
buildSecondSection <- function(processedData){
  
  tableAsString <- purrr::map2_chr(processedData$pooledStdDev, names(processedData$pooledStdDev), 
                                   ~ sprintf("%s, %.2f, %.2f", .y, .x$d18O, .x$dD)) %>%
                   paste(collapse = "\n")
  stringr::str_c(
    "### VALUES FOR EACH FILE: ###\n\n",
    "file name, pooled sd delta O18, pooled sd delta H2\n",
    tableAsString
  )
}

#' Build section "INTER STANDARD BIAS TO LITERATURE VALUES FOR EACH FILE"
#'
#' @param processedData a list as output by \code{processData()}.
#' @param config a named list with the contents of the config file.
#' @import dplyr
buildThirdSection <- function(processedData, config){
  
  # make true values easily accessible
  trueValues <- purrr::map(
    config$standards, 
    ~ list(o18_True = .$o18_True, H2_True = .$H2_True)
  )
  names(trueValues) <- purrr::map_chr(config$standards, ~ .$name)
  
  # construct list of biases
  biasesList <- purrr::map2(
    processedData$processed, names(processedData$processed), ~ apply(.x, 1, function(row){
      block <- row[["block"]]
      name <- row[["Identifier 1"]]
      if(!is.na(block)) # if the block is na the sample is a standard
        sprintf("%s, %s, %.f, %.2f, %.2f", .y, name, as.numeric(block), 
                as.numeric(row[["delta.O18"]]) - trueValues[[name]][["o18_True"]],
                as.numeric(row[["delta.H2"]]) - trueValues[[name]][["H2_True"]])
    })
  )
  # convert list of biases to string
  biasesText <- paste(unlist(biasesList), collapse = "\n")
  
  # create output text for the true values
  trueValuesText <- config$standards %>%
    purrr::map_chr(~ sprintf("%s, %.2f, %.2f", .$name, .$o18_True, .$H2_True)) %>%
    paste(collapse = "\n")
  
  # join text sections into one
  stringr::str_c(
    "### INTER STANDARD BIAS TO LITERATURE VALUES FOR EACH FILE: ###\n\n",
    "## LITERATURE VALUES ##\n",
    "standard, O18, H2",
    trueValuesText,
    "\n\n",
    "## BIAS ##\n",
    "file name, standard, block, bias O18, bias H2\n",
    biasesText
  )
}

gatherQualityControlInfo <- function(datasets) {

  rmsdQualityControl <- purrr::map_dfr(datasets, function(x) {
    tibble::tibble(file = x$name,
                   name = paste(x$deviationOfControlStandard$name, sep = ", "),
                   d18O = calculateRMSD(x$deviationOfControlStandard$d18O),
                   dD = calculateRMSD(x$deviationOfControlStandard$dD))})

  rmsdAllStandards <- purrr::map_dfr(datasets, function(x) {
    tibble::tibble(file = x$name,
                   d18O = x$rmsdDeviationsFromTrue$d18O,
                   dD = x$rmsdDeviationsFromTrue$dD)})

  pooledSD <- purrr::map_dfr(datasets, function(x) {
    tibble::tibble(file = x$name,
                   d18O = x$pooledSD$d18O,
                   dD = x$pooledSD$dD)})

  deviationsFromTrue <- lapply(datasets, function(x) {
    x$deviationsFromTrue %>%
      dplyr::select(Sample, `Identifier 1`, block, d18ODeviation, dDDeviation)
  })

  return(
    list(
      rmsdQualityControl = rmsdQualityControl,
      rmsdAllStandards = rmsdAllStandards,
      pooledSD = pooledSD,
      deviationsFromTrue = deviationsFromTrue
    )
  )
}

#' Print quality control
#'
#' dummy text
#'
#' @param datasets dummy
#' @param printDeviations dummy
#' @param n dummy
#'
#' @return dummy
#' @export
#'
printQualityControl <- function(datasets, printDeviations = FALSE, n = 3) {

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

  if (printDeviations) {

    nmax <- length(qualityControlInfo$deviationsFromTrue)
    subset <- TRUE
    if (is.na(n) | n > nmax) {
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
      x <- qualityControlInfo$deviationsFromTrue[i]
      print(x, n = nrow(x))
    }
  }

  return(invisible(datasets))
}

printRunInfo <- function(configFile) {

  cat(sprintf("piccr; version %s\n", utils::packageVersion("piccr")))
  cat(sprintf("* config file: %s\n", configFile))
  cat(sprintf("* processing date: %s\n\n", Sys.time()))

}
