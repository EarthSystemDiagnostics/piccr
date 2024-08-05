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

test_that("general acceptance is fulfilled", {

  # memory correction and calibration method 1
  actualMemoryCalib1 <- test_processData(datasets, config)

  # memory correction and calibration method 2
  config$calibration_method <- 2
  actualMemoryCalib2 <- test_processData(datasets, config)

  # memory correction and calibration method 0
  config$calibration_method <- 0
  actualMemoryCalib0 <- test_processData(datasets, config)

  # NO memory correction and calibration method 2; average first three injections
  config$calibration_method <- 2
  config$use_memory_correction <- FALSE
  config$average_over_inj <- "1:3"
  actualNoMemoryCalib2 <- test_processData(datasets, config)

  # --------------------------------------------------------------------------
  # ACCEPTANCE TEST 1:
  # new piccr pkg performs as good as old command line version, or even better
  # --------------------------------------------------------------------------

  qcMemoryCalib2   <- gatherQualityControlInfo(actualMemoryCalib2)

  rmsdD18OGood <- calculateRMSD(qcMemoryCalib2$rmsdQualityControl$d18O)
  rmsdDDGood   <- calculateRMSD(qcMemoryCalib2$rmsdQualityControl$dD)

  rmsdD18OCLVersion <- calculateRMSD(c(-0.096, -0.014, -0.053))
  rmsdDDCLVersion   <- calculateRMSD(c(-1.205, -0.486, -0.693))

  expect_lte(rmsdD18OGood, rmsdD18OCLVersion)
  expect_lte(rmsdDDGood, rmsdDDCLVersion)

  # ----------------------------------------------------------
  # ACCEPTANCE TEST 2:
  # no memory correction is worse than using memory correction
  # ----------------------------------------------------------

  qcNoMemoryCalib2 <- gatherQualityControlInfo(actualNoMemoryCalib2)

  rmsdD18OBad <- calculateRMSD(qcNoMemoryCalib2$rmsdQualityControl$d18O)
  rmsdDDBad   <- calculateRMSD(qcNoMemoryCalib2$rmsdQualityControl$dD)

  expect_lte(rmsdD18OGood, rmsdD18OBad)
  expect_lte(rmsdDDGood, rmsdDDBad)

  # ---------------------------------------------------------
  # ACCEPTANCE TEST 3:
  # drift correction is as good as without it, or even better
  # ---------------------------------------------------------

  # set upper tolerance for "equally as good" drift correction
  toleranceD18O <- 0.01
  toleranceDD <-   0.1

  qcMemoryCalib0   <- gatherQualityControlInfo(actualMemoryCalib0)
  qcMemoryCalib1   <- gatherQualityControlInfo(actualMemoryCalib1)

  # calibration method 2 versus method 0

  d <- rmsdD18OGood - calculateRMSD(qcMemoryCalib0$rmsdQualityControl$d18O)

  if (d > 0) {
    expect_lte(d, toleranceD18O)
  } else {
    expect_lte(d, 0)
  }

  d <- rmsdDDGood - calculateRMSD(qcMemoryCalib0$rmsdQualityControl$dD)

  if (d > 0) {
    expect_lte(d, toleranceDD)
  } else {
    expect_lte(d, 0)
  }

  # calibration method 1 versus method 0

  d <- calculateRMSD(qcMemoryCalib1$rmsdQualityControl$d18O) -
    calculateRMSD(qcMemoryCalib0$rmsdQualityControl$d18O)

  if (d > 0) {
    expect_lte(d, toleranceD18O)
  } else {
    expect_lte(d, 0)
  }

  d <- calculateRMSD(qcMemoryCalib1$rmsdQualityControl$dD) -
    calculateRMSD(qcMemoryCalib0$rmsdQualityControl$dD)

  if (d > 0) {
    expect_lte(d, toleranceDD)
  } else {
    expect_lte(d, 0)
  }

  # ----------------------------------
  # BENCHMARK TEST:
  # current performance should be kept
  # ----------------------------------

  # best current performance
  benchmarkD18O <- 0.03
  benchmarkDD   <- 0.4

  expect_lte(rmsdD18OGood, benchmarkD18O)
  expect_lte(rmsdDDGood, benchmarkDD)

})
