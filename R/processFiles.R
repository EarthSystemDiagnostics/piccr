#' @export
processFiles <- function(configFile){
  
  config <- parseConfig(configFile)
  
  processedData <- readFiles(config) %>% 
    groupStandardsInBlocks(config)
    associateStandardsWithTrueValues(config) %>%
    processData()
  
  writeDataToFile(config, processedData)
  
  invisible(processedData)
}