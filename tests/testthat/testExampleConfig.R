library(testthat)
library(tidyverse)
library(rlist)

context("Do a full run using the example config")

test_that("test return value and outputs", {
  
  # ---------- INITIALIZE INPUTS -------------
  
  newConfigPath <- file.path(tempdir(), "config.yaml")
  outputDir <- file.path(tempdir(), "fullRunOutput2")
  on.exit(file.remove(newConfigPath))
  on.exit(unlink(outputDir, recursive = TRUE))
  
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
  for (dataset in processedData){
    expect_is(dataset$name, "character")
    expect_is(dataset$raw, "data.frame")
    expect_is(dataset$memoryCorrected, "data.frame")
    expect_is(dataset$calibrated, "data.frame")
    expect_is(dataset$calibratedAndDriftCorrected, "data.frame")
    expect_is(dataset$processed, "data.frame")
    expect_is(dataset$deviationsFromTrue, "data.frame")
    expect_is(dataset$deviationOfControlStandard, "list")
    expect_length(dataset$deviationOfControlStandard[[1]], 1)
    expect_is(dataset$rmsdDeviationsFromTrue, "list")
    expect_is(dataset$pooledSD, "list")
    expect_is(dataset$memoryCoefficients, "data.frame")
  }

  # check saved files
  outputFiles <- list.files(outputDir)
  expect_equal(outputFiles, c("HIDS2041_IsoWater_20151125_111138.csv",  
                              "HIDS2041_IsoWater_20151126_115726.csv", 
                              "HIDS2041_IsoWater_20151127_143940.csv",
                              "run.info"))
})
