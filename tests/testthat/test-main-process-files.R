test_that("return value and outputs from processFiles are correct", {
  
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
    expect_type(dataset$name, "character")
    expect_true(is.data.frame(dataset$raw))
    expect_true(is.data.frame(dataset$memoryCorrected))
    expect_true(is.data.frame(dataset$calibrated))
    expect_true(is.data.frame(dataset$calibratedAndDriftCorrected))
    expect_true(is.data.frame(dataset$processed))
    expect_true(is.data.frame(dataset$deviationsFromTrue))
    expect_type(dataset$deviationOfControlStandard, "list")
    expect_length(dataset$deviationOfControlStandard[[1]], 1)
    expect_type(dataset$rmsdDeviationsFromTrue, "list")
    expect_type(dataset$pooledSD, "list")
    expect_true(is.data.frame(dataset$memoryCoefficients))
    expect_true(is.data.frame(dataset$calibrationParams))
    expect_true(is.data.frame(dataset$driftParams))
  }

  # check saved files
  outputFiles <- list.files(outputDir)
  expect_equal(outputFiles, c("HIDS2041_IsoWater_20151125_111138.csv",  
                              "HIDS2041_IsoWater_20151126_115726.csv", 
                              "HIDS2041_IsoWater_20151127_143940.csv",
                              "run.info"))

  # --------- CHECK OTHER PROCESSING OPTIONS -------

  configContents$use_memory_correction <- FALSE
  rlist::list.save(configContents, newConfigPath)

  expect_error(processFiles(config = newConfigPath), NA)

  configContents$calibration_method <- 0
  rlist::list.save(configContents, newConfigPath)

  expect_error(processFiles(config = newConfigPath), NA)

  configContents$use_memory_correction <- TRUE
  configContents$calibration_method <- 2
  rlist::list.save(configContents, newConfigPath)

  expect_error(processFiles(config = newConfigPath), NA)
})
