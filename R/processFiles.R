#' @export
processFiles <- function(configFile){
  
  config <- parseConfig(configFile)
  
  processedData <- readFiles(config) %>% 
    associateStandardsWithTrueValues(config) %>%
    processData()
  
  writeDataToFile(config, processedData)
  
  invisible(processedData)
}