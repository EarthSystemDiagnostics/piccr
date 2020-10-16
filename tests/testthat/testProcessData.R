library(readr)
library(stringr)
library(yaml)

context("test processData")

configPath <- system.file("extdata", "config.yaml", package = "piccr")

# test should be able to run from the repository root directory or from the directory
# tests/testthat. If the directory is testthat, set the input directory to 
# test_data in the config and set the output directory to test_data/output.
configContents <- readr::read_file(configPath)
if (endsWith(getwd(), "testthat")) {
  configContents <- stringr::str_replace(configContents, "input_directory:\\s+.+\\n", "input_directory:  test_data\n")
  configContents <- stringr::str_replace(configContents, "output_directory:\\s+.+\\n", "output_directory:  test_data/output\n")
}
config <- yaml::yaml.load(configContents)

datasets <- list(
  HIDS2041_IsoWater_20151126_115726.csv = readr::read_csv("test_data/HIDS2041_IsoWater_20151126_115726.csv"),
  HIDS2041_IsoWater_20151125_111138.csv = readr::read_csv("test_data/HIDS2041_IsoWater_20151125_111138.csv"),  
  HIDS2041_IsoWater_20151127_143940.csv = readr::read_csv("test_data/HIDS2041_IsoWater_20151127_143940.csv")
)

test_that("check general output structure", {

  actual <- processData(datasets[1], config)

  expect_length(actual, 1)
  expect_true(is.list(actual))
  expect_true(is.list(actual[[1]]))
  expect_length(actual[[1]], 13)

  expect_true(is.vector(actual[[1]]$name))
  expect_length(is.vector(actual[[1]]$name), 1)
  expect_true(is.data.frame(actual[[1]]$raw))
  expect_true(all(!c("useForCalibration", "block", "o18_True", "H2_True", 
                 "useForDriftCorr", "useAsControlStandard") %in% colnames(actual[[1]]$raw)))

  expect_true(is.data.frame(actual[[1]]$memoryCorrected))
  expect_true(is.data.frame(actual[[1]]$memoryCoefficients))
  expect_equal(ncol(actual[[1]]$memoryCoefficients), 13)

  expect_true(is.data.frame(actual[[1]]$calibrated))
  expect_true(is.data.frame(actual[[1]]$calibratedAndDriftCorrected))

  expect_is(actual[[1]]$processed, "data.frame")

  expect_true(is.list(actual[[1]]$pooledSD))
  expect_length(actual[[1]]$pooledSD, 2)

  expect_true(is.data.frame(actual[[1]]$deviationsFromTrue))
  expect_true(is.list(actual[[1]]$rmsdDeviationsFromTrue))
  expect_true(is.list(actual[[1]]$deviationOfControlStandard))

  expect_type(actual[[1]]$calibrationParams, "list")
  expect_equal(dim(actual[[1]]$calibrationParams), c(2, 8))
  expect_equal(actual[[1]]$calibrationParams$species, c("d18O", "dD"))
  expect_equal(actual[[1]]$calibrationParams$block, rep(1, 2))

  actual <- processData(datasets, config)

  expect_length(actual, 3)

})
  
test_that("check that no NAs were introduced", {
    
  actual <- processData(datasets, config)
  
  for (dataset in actual) {
    expect_equal(sum(is.na(dataset$processed$`delta.O18`)), 2)
    expect_equal(sum(is.na(dataset$processed$`delta.H2`)), 2)
  }
  
})

test_that("check that calibration method 2 runs", {

  config$calibration_method <- 2
  actual <- processData(datasets[1], config)

  expect_is(actual[[1]]$calibratedAndDriftCorrected, "data.frame")
  expect_equal(dim(actual[[1]]$memoryCorrected),
               dim(actual[[1]]$calibratedAndDriftCorrected))

  expect_equal(dim(actual[[1]]$calibrationParams), c(4, 8))
  expect_equal(actual[[1]]$calibrationParams$species, rep(c("d18O", "dD"), 2))
  expect_equal(actual[[1]]$calibrationParams$block, c(1, 1, 3, 3))

})

test_that("check that data set names are preserved", {

  actual <- processData(datasets, config)

  expect_equal(names(actual), names(datasets))

})
