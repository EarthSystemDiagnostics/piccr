library(testthat)
library(readr)
library(yaml)

context("test processData")

configPath <- system.file("extdata", "config.yaml", package = "piccr")

# test should be able to run from the repository root directory or from the directory
# tests/testthat. If the directory is testthat, set the input directory to 
# test_data in the config and set the output directory to test_data/output.
configContents <- read_file(configPath)
if (endsWith(getwd(), "testthat")) {
  configContents <- str_replace(configContents, "input_directory:\\s+.+\\n", "input_directory:  test_data\n")
  configContents <- str_replace(configContents, "output_directory:\\s+.+\\n", "output_directory:  test_data/output\n")
}
config <- yaml::yaml.load(configContents)

datasets <- list(
  HIDS2041_IsoWater_20151126_115726.csv = read_csv("test_data/HIDS2041_IsoWater_20151126_115726.csv"),
  HIDS2041_IsoWater_20151125_111138.csv = read_csv("test_data/HIDS2041_IsoWater_20151125_111138.csv"),  
  HIDS2041_IsoWater_20151127_143940.csv = read_csv("test_data/HIDS2041_IsoWater_20151127_143940.csv")
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
  expect_equal(ncol(actual[[1]]$memoryCoefficients), 11)

  expect_true(is.data.frame(actual[[1]]$calibrated))
  expect_true(is.data.frame(actual[[1]]$calibratedAndDriftCorrected))

  expect_true(is.data.frame(actual[[1]]$processed))

  expect_true(is.list(actual[[1]]$pooledSD))
  expect_length(actual[[1]]$pooledSD, 2)

  expect_true(is.data.frame(actual[[1]]$deviationsFromTrue))
  expect_true(is.list(actual[[1]]$rmsdDeviationsFromTrue))
  expect_true(is.list(actual[[1]]$deviationOfControlStandard))

  actual <- processData(datasets, config)

  expect_length(actual, 3)

})
  
test_that("check that no NAs were introduced", {
    
  actual <- processData(datasets, config)
  
  for (dataset in actual$processed) {
    expect_equal(sum(is.na(dataset$`delta.O18`)), 1)
    expect_equal(sum(is.na(dataset$`delta.H2`)), 1)
  }
  
})
