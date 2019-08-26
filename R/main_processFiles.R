library(magrittr)

#' @export
processFiles <- function(configFile){
  
  config <- parseConfig(configFile)
  
  processedData <- readFiles(config) %>% 
    processData(config)
  
  writeDataToFile(processedData, config)
  
  return(processedData)
}