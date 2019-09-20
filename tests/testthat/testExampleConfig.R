library(testthat)
library(tidyverse)
library(rlist)

context("Do a full run using the example config")

test_that("test return value and outputs", {
  
  # ---------- INITIALIZE INPUTS -------------
  
  newConfigPath <- file.path(tempdir(), "config.yaml")
  outputDir <- file.path(tempdir(), "fullRunOutput")
  on.exit(file.remove(newConfigPath))
  on.exit(unlink(file.path(outputDir)))
  
  configPath <- system.file("extdata", "config.yaml", package = "piccr")
  configContents <- rlist::list.load(configPath)
  configContents$input_directory <- if(endsWith(getwd(), "testthat")) "test_data" else "tests/testthat/test_data"
  configContents$output_directory <- outputDir
    
  rlist::list.save(configContents, newConfigPath)
  
  # --------- CALL FUNCTION UNDER TEST ------------
  
  processedData <- processFiles(config = newConfigPath)
  
  # --------- MAKE EXPECTATIONS --------------------
  
  # check format of return value
  expect_true(is.list(processedData))
  expect_length(processedData, 3)
  # TODO: uncomment these tests when the new interface is stable
  #expect_length(processedData$memoryCorrected, 3)
  #expect_length(processedData$calibrated, 3)
  #expect_length(processedData$processed, 3)
  #expect_length(processedData$pooledStdDev, 3)

  # check saved files
  # TODO: uncomment these tests when the new interface is stable
  ## outputFiles <- list.files(outputDir)
  ## expect_equal(outputFiles, c("HIDS2041_IsoWater_20151125_111138.csv",  
  ##                             "HIDS2041_IsoWater_20151126_115726.csv", 
  ##                             "HIDS2041_IsoWater_20151127_143940.csv",
  ##                             "run.info"))
})
