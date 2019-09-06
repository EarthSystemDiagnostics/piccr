library(magrittr)

#' processFiles
#' 
#' Main interface for the stand-alone use of piccr. Input the path to
#' a config file. Then process measurement data according to the 
#' configurations in the config file.
#' 
#' Relies on processData(..) to do the actual processing.
#' 
#' @param configFile A string. The path to a config file.
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
  
  return(processedData)
}