library(tidyverse)

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
outputSummaryFile <- function(processedData, config, outputFile = NULL){
  
  # set default value for outputFile if needed
  if(is.null(outputFile)) 
    outputFile <- file.path(config$output_directory, "run.info")
  
  firstSection  <- buildFirstSection(processedData)
  secondSection <- buildSecondSection(processedData)
  thirdSection  <- buildThirdSection(processedData, config)
  
  summaryText <- str_c(
    firstSection, 
    "\n\n", 
    secondSection, 
    "\n\n", 
    thirdSection
  )
  
  write_file(summaryText, outputFile)
}

#' Build section "AVERAGE OVER ALL FILES"
buildFirstSection <- function(processedData){
  
  meanPooledSdO18 <- mean(map_dbl(processedData$pooledStdDev, ~ .$d18O))
  meanPooledSdH2  <- mean(map_dbl(processedData$pooledStdDev, ~ .$dD))
  
  sprintf(
    str_c("### AVERAGE OVER ALL FILES: ###\n\n",
          "pooled standard deviation delta O18: %.2f\n",
          "pooled standard deviation delta H2: %.2f"),
    meanPooledSdO18, meanPooledSdH2
  )
}

#' Build section "VALUES FOR EACH FILE"
buildSecondSection <- function(processedData){
  
  tableAsString <- map2_chr(processedData$pooledStdDev, names(processedData$pooledStdDev), 
                            ~ sprintf("%s, %.2f, %.2f", .y, .x$d18O, .x$dD)) %>%
                   paste(collapse = "\n")
  str_c(
    "### VALUES FOR EACH FILE: ###\n\n",
    "file name, pooled sd delta O18, pooled sd delta H2\n",
    tableAsString
  )
}

#' Build section "INTER STANDARD BIAS TO LITERATURE VALUES FOR EACH FILE"
buildThirdSection <- function(processedData, config){
  
  # make true values easily accessible
  trueValues <- map(
    config$standards, 
    ~ list(o18_True = .$o18_True, H2_True = .$H2_True)
  )
  names(trueValues) <- map_chr(config$standards, ~ .$name)
  
  # construct list of biases
  biasesList <- map2(
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
    map_chr(~ sprintf("%s, %.2f, %.2f", .$name, .$o18_True, .$H2_True)) %>%
    paste(collapse = "\n")
  
  # join text sections into one
  str_c(
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