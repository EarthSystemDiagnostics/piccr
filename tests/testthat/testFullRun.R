library(testthat)
library(tidyverse)

configPath <- system.file("extdata", "config.yaml", package = "piccr")

# test should be able to run from the repository root directory or from the directory
# tests/testthat. If the directory is testthat, set the input directory to 
# test_data in the config and set the output directory to test_data/output.
if (endsWith(getwd(), "testthat")) {
  configContents <- read_file(configPath)
  configContents <- str_replace(configContents, "input_directory:\\s+.+\\n", "input_directory:  test_data\n")
  configContents <- str_replace(configContents, "output_directory:\\s+.+\\n", "output_directory:  test_data/output\n")
  tempFile <- tempfile()
  write_file(configContents, tempFile)
  configPath <- tempFile
}

test_that("full run with memory correction and simple calibration", {
  
  processedData <- processFiles(config = configPath)
  
  expect_length(processedData, 4)
  expect_length(processedData$memoryCorrected, 3)
  expect_length(processedData$calibrated, 3)
})

test_that("full run with memory correction and simple calibration", {
  
  # make sure test runs from repository root directory and from tests/testthat
  if (endsWith(getwd(), "testthat")) {
    outputDir <- "test_data/output"
  } else {
    outputDir <- "tests/testthat/test_data/output"
  }
  unlink(outputDir, recursive = TRUE)
  
  processFiles(config = configPath)
  outputFiles <- list.files(outputDir)
  
  expect_length(outputFiles, 3)
})