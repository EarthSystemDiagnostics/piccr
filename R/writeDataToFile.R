library(tidyverse)

writeDataToFile <- function(datasets, outputFolder = "."){
  
  dir.create(file.path(outputFolder, "calibrated"), showWarnings = FALSE)
  walk(names(datasets), ~ write_csv(datasets[[.]], path = str_c(outputFolder, "/calibrated/", .)))
}