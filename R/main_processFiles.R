#' processFiles
#' 
#' Main interface for the stand-alone use of piccr. Input the path to
#' a config file. Then process measurement data according to the 
#' configurations in the config file.
#' 
#' Relies on processData(..) to do the actual processing.
#' 
#' @param configFile A character string naming the config file.
#' @import dplyr
#'
#' @return A list. See the documentation for processData(..) for details.
#' 
#' @export
#' 
processFiles <- function(configFile){
  
  config <- parseConfig(configFile)
  
  processedData <- readFiles(config) %>% 
    processData(config)
  
  writeDataToFile(processedData, config)
  outputSummaryFile(processedData, config)
  
  return(processedData)
}
