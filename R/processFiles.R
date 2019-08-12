#' @export
processFiles <- function(configFile){
  
  config <- parseConfig(configFile)
  
  processedData <- readFiles(config) %>% 
    groupStandardsInBlocks(config) %>%
    associateStandardsWithConfigInfo(config) %>%
    processData()
  
  writeDataToFile(config, processedData)
  
  invisible(processedData)
}