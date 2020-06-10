#' Process Picarro CRDS measurement files
#' 
#' This is the main interface for the stand-alone use of \code{piccr} to process
#' a number of Picarro data files in a given directory according to the settings
#' in the configuration file \code{config.yaml}. The function calls
#' \code{\link{processData}} to do the actual processing.
#' 
#' @param configFile A character string naming the YAML configuration file
#'   that includes the parameter settings to use for the processing; see the
#'   example file provided with \code{piccr} (`system.file("extdata",
#'   "config.yaml", package = "piccr")`) for details.
#' @param writeOutput logical; shall processed data and quality control
#'   information be written to disk in the location specified by the
#'   configuration file? Defaults to \code{TRUE}.
#' @import dplyr
#'
#' @return Invisibly, a list the same length as the number of raw Picarro files
#'   in the input directory; each list element contains the output from
#'   \code{\link{processSingleDataset}}.
#' @seealso \code{\link{processData}}, \code{\link{processSingleDataset}}
#' @export
#' 
processFiles <- function(configFile, writeOutput = TRUE){
  
  config <- parseConfig(configFile)
  
  processedData <- readFiles(config) %>% 
    processData(config)
  
  if (writeOutput) {
    writeDataToFile(processedData, config)
    outputSummaryFile(processedData, config, configFile)
  }
  
  return(invisible(processedData))
}
