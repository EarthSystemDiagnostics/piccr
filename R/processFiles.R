#' @export
processFiles <- function(configFile){
  
  config <- parseConfig(configFile)
  
  processedData <- readFiles(config) %>% 
    groupStandardsInBlocks(config) %>%
    associateStandardsWithConfigInfo(config) %>%
    processData(config)
  
  writeDataToFile(processedData, config)
  
  invisible(processedData)
}