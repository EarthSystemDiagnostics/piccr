library(rlist)

context("Do a full run using example configs")

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

test_that("test example file with differently grouped vials", {

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
