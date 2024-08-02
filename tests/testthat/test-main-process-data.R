test_that("processData works on example file with differently grouped vials", {

  # ---------- INITIALIZE INPUTS -------------

  inputDir <- if (endsWith(getwd(), "testthat")) {
                "test_data/special_file"
              } else {
                "tests/testthat/test_data/special_file"
              }

  standards <- list(
    list(
      name = "TD1",
      o18_True = -33.8,
      H2_True = -266.7,
      use_for_drift_correction = TRUE,
      use_for_calibration = FALSE,
      use_as_control_standard = FALSE),
    list(
      name = "VSMOW2",
      o18_True = 0.,
      H2_True = 0.,
      use_for_drift_correction = FALSE,
      use_for_calibration = TRUE,
      use_as_control_standard = FALSE),
    list(
      name = "SLAP2",
      o18_True = -55.5,
      H2_True = -427.5,
      use_for_drift_correction = FALSE,
      use_for_calibration = TRUE,
      use_as_control_standard = FALSE),
    list(
      name = "DML",
      o18_True = -42.39,
      H2_True = -341.24,
      use_for_drift_correction = FALSE,
      use_for_calibration = FALSE,
      use_as_control_standard = TRUE))

  config <- list(
    file_extension = ".csv",
    input_directory = inputDir,
    include_standards_in_output = TRUE,
    average_over_inj = 3,
    use_three_point_calibration = FALSE,
    calibration_method = 1,
    use_memory_correction = TRUE,
    standards = standards
  )

  # --------- TEST PROGRAMME RUN ------------

  # -----------------------------------------
  # 1. no corrections at all

  config$use_memory_correction <- FALSE
  config$calibration_method <- 0

  # check if piccr runs without error
  expect_error(processedData <- readFiles(config) %>%
                 processData(config), NA)

  skip_if_not(exists("processedData"), "previous test")

  # check number of accumulated data points
  expect_equal(nrow(processedData[[1]]$processed), 43)

  # -----------------------------------------
  # 2. apply memory correction

  config$use_memory_correction <- TRUE
  config$calibration_method <- 0

  # check if piccr runs without error
  expect_error(processedData <- readFiles(config) %>%
                 processData(config), NA)

  skip_if_not(exists("processedData"), "previous test")

  # check number of memory coefficients used
  expect_equal(nrow(processedData[[1]]$memoryCoefficients), 10)

  # check validity of memory-corrected data
  expect_equal(nrow(processedData[[1]]$memoryCorrected), 440)

  # -----------------------------------------
  # 3. apply memory and simple drift correction

  config$use_memory_correction <- TRUE
  config$calibration_method <- 1

  # check if piccr runs without error
  expect_error(processedData <- readFiles(config) %>%
                 processData(config), NA)

})

# ------------------------------------------------------------------------------
# ----- READL DATA USAGE -------------------------------------------------------
# ------------------------------------------------------------------------------

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

# ------------------------------------------------
# Run tests on different processing configurations
# ------------------------------------------------

# ------------------------------------------------------------------------------
# memory correction and calibration method 1

actual <- processData(datasets[1], config)
actualMemoryCalib1 <- processData(datasets, config)

test_that("general output structure is correct", {

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
  expect_equal(dim(actual[[1]]$calibrationParams), c(2, 9))
  expect_equal(actual[[1]]$calibrationParams$species, c("d18O", "dD"))
  expect_equal(actual[[1]]$calibrationParams$block, rep(1, 2))

  expect_type(actual[[1]]$driftParams, "list")
  expect_equal(dim(actual[[1]]$driftParams), c(8, 6))
  expect_equal(actual[[1]]$driftParams$species, c(rep("d18O", 4), rep("dD", 4)))
  expect_equal(actual[[1]]$driftParams$sample,
               rep(c("DML", "JASE", "TD1", "mean"), 2))

  expect_length(actualMemoryCalib1, 3)

})
  
test_that("no NAs were introduced", {
  
  for (dataset in actualMemoryCalib1) {
    expect_equal(sum(is.na(dataset$processed$`delta.O18`)), 2)
    expect_equal(sum(is.na(dataset$processed$`delta.H2`)), 2)
  }
  
})

test_that("data set names are preserved", {

  expect_equal(names(actualMemoryCalib1), names(datasets))

})

# ------------------------------------------------------------------------------
# memory correction and calibration method 2

config$calibration_method <- 2
actualMemoryCalib2 <- processData(datasets, config)

test_that("calibration method 2 runs", {

  expect_is(actualMemoryCalib2[[1]]$calibratedAndDriftCorrected, "data.frame")
  expect_equal(dim(actualMemoryCalib2[[1]]$memoryCorrected),
               dim(actualMemoryCalib2[[1]]$calibratedAndDriftCorrected))

  expect_equal(dim(actualMemoryCalib2[[1]]$calibrationParams),
               c(4, 9))
  expect_equal(actualMemoryCalib2[[1]]$calibrationParams$species,
               rep(c("d18O", "dD"), 2))
  expect_equal(actualMemoryCalib2[[1]]$calibrationParams$block,
               c(1, 1, 3, 3))

})

# ------------------------------------------------------------------------------
# No memory correction and calibration method 0

test_that("calibration method 0 runs w/o memory correction", {

  config$calibration_method <- 0
  config$use_memory_correction <- FALSE

  actual <- processData(datasets[1], config)

  expect_length(actual[[1]]$memoryCorrected, 0)
  expect_length(actual[[1]]$memoryCoefficients, 0)
  expect_length(actual[[1]]$calibratedAndDriftCorrected, 0)
  expect_length(actual[[1]]$driftParams, 0)

  expect_equal(dim(actual[[1]]$calibrationParams), c(2, 9))
  expect_equal(actual[[1]]$calibrationParams$species, c("d18O", "dD"))
  expect_equal(actual[[1]]$calibrationParams$block, rep(1, 2))

})
