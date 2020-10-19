library(tibble)
library(readr)
library(stringr)

context("Test creating quality control information and writing them to file")

# ------------ INITIALIZE INPUTS -------------

deviationsFromTrue1 <- tibble::tribble(
  ~Sample, ~`Identifier 1`, ~block, ~d18ODeviation, ~dDDeviation,
  # ---- / -------------- / ----- / ------------- / ------------
  1,     "C",               1L,      0.01,          0.1,
  2,     "A",               1L,      0.07,          0.7,
  3,     "QC",              2L,      0.01,          0.1,
  4,     "C",               3L,      -0.07,         -0.7
)
deviationsFromTrue2 <- tibble::tribble(
  ~Sample, ~`Identifier 1`, ~block, ~d18ODeviation, ~dDDeviation,
  # ---- / -------------- / ----- / ------------- / ------------
  1,     "C",               1L,      -0.1,          1,
  2,     "A",               1L,      -0.7,          -7,
  3,     "QC",              2L,      -0.1,          -1,
  4,     "C",               3L,      0.7,           -7
)
memoryCoefficients1 <- tibble::tribble(
  ~`Inj Nr`, ~`StdA_vial1_d18O`, ~`StdA_vial1_dD`, ~`StdB_vial1_d18O`, ~`StdB_vial1_dD`, ~memoryCoeffD18O, ~memoryCoeffDD, ~sdMemoryCoeffD18O, ~sdMemoryCoeffDD,
  # ------ / ----------------- / --------------- / ----------------- / --------------- / --------------- / ------------- / ----------------- / ----------------
  1,         0.96,               0.94,             0.96,               0.94,              0.96,            0.94,           0,                  0,
  2,         0.98,               0.97,             0.98,               0.97,              0.98,            0.97,           0,                  0,
  3,         1,                  1,                1,                  1,                 1,               1,              0,                  0
)
memoryCoefficients2 <- tibble::tribble(
  ~`Inj Nr`, ~`StdA_vial1_d18O`, ~`StdA_vial1_dD`, ~`StdB_vial1_d18O`, ~`StdB_vial1_dD`, ~memoryCoeffD18O, ~memoryCoeffDD, ~sdMemoryCoeffD18O, ~sdMemoryCoeffDD,
  # ------ / ----------------- / --------------- / ----------------- / --------------- / --------------- / ------------- / ----------------- / ----------------
  1,         0.96,               0.94,             0.96,               0.94,              0.96,            0.94,           0,                  0,
  2,         0.98,               0.97,             0.98,               0.97,              0.98,            0.97,           0,                  0,
  3,         1,                  1,                1,                  1,                 1,               1,              0,                  0,
  4,         1,                  1,                1,                  1,                 1,               1,              0,                  0
)
calibrationParameter1 <- tibble::tribble(
  ~species, ~block, ~timeStamp, ~intercept, ~slope, ~other_cols,
  # ----- / ----- / --------- / --------- / ----- / -----------
  "d18O",   1,      1000.,      0.,         1.,     "foo",
  "dD",     1,      1000.,      0.,         1.,     "foo"
)
calibrationParameter2 <- tibble::tribble(
  ~species, ~block, ~timeStamp, ~intercept, ~slope, ~other_cols,
  # ----- / ----- / --------- / --------- / ----- / -----------
  "d18O",   1,      500.,       10.,        2.7,    "foo",
  "dD",     1,      500.,       100.,       9.3,    "foo"
)
processedData <- list(
  good = list(
    name = "good",
    deviationOfControlStandard = list(name = "QC", d18O = 0.01, dD = 0.1),
    rmsdDeviationsFromTrue = list(d18O = 0.05, dD = 0.5),
    pooledSD = list(d18O = 0.03, dD = 0.3),
    deviationsFromTrue = deviationsFromTrue1,
    memoryCoefficients = memoryCoefficients1,
    calibrationParams = calibrationParameter1
  ),
  bad = list(
    name = "bad",
    deviationOfControlStandard = list(name = "QC", d18O = 0.1, dD = 1),
    rmsdDeviationsFromTrue = list(d18O = 0.5, dD = 5),
    pooledSD = list(d18O = 0.3, dD = 3),
    deviationsFromTrue = deviationsFromTrue2,
    memoryCoefficients = memoryCoefficients2,
    calibrationParams = calibrationParameter2
  )
)
qc <- tibble::tribble(
  ~dataset, ~name, ~d18O, ~dD,
  # ----- / ---- / ---- / -- /
  "good",    "QC",  0.01,  0.1,
  "bad",     "QC",  0.1,   1
)
rmsdAll <- tibble::tribble(
  ~dataset, ~d18O, ~dD,
  # ----- / ---- / -- /
  "good",   0.05,  0.5,
  "bad",    0.5,   5
)
pooledSD <- tibble::tribble(
  ~dataset, ~d18O, ~dD,
  # ----- / ---- / -- /
  "good",   0.03,  0.3,
  "bad",    0.3,   3
)
memCoeff <- tibble::tribble(
  ~dataset, ~`Inj Nr`, ~meanD18O, ~meanDD, ~sdD18O, ~sdDD,
  # ---- / -------- / -------- / ------ / ------ / ------
  "mean",  1,         0.96,      0.94,    0,       0,
  "mean",  2,         0.98,      0.97,    0,       0,
  "mean",  3,         1,         1,       0,       0,
  "mean",  4,         1,         1,       0,       0,
  "good",  1,         0.96,      0.94,    0,       0,
  "good",  2,         0.98,      0.97,    0,       0,
  "good",  3,         1,         1,       0,       0,
  "bad",   1,         0.96,      0.94,    0,       0,
  "bad",   2,         0.98,      0.97,    0,       0,
  "bad",   3,         1,         1,       0,       0,
  "bad",   4,         1,         1,       0,       0
)
calibrationParameter <- tibble::tribble(
  ~dataset, ~species, ~block, ~timeStamp, ~intercept, ~slope, ~other_cols,
  # ----- / ------- / ----- / --------- / --------- / ----- / -----------
  "good",   "d18O",   1,      1000.,      0.,         1.,     "foo",
  "good",   "dD",     1,      1000.,      0.,         1.,     "foo",
  "bad",    "d18O",   1,      500.,       10.,        2.7,    "foo",
  "bad",    "dD",     1,      500.,       100.,       9.3,    "foo"
)

test_that("gathering of quality control data works", {
  
  expected <- list(
    rmsdQualityControl = qc,
    rmsdAllStandards = rmsdAll,
    pooledSD = pooledSD,
    deviationsFromTrue = list(
      good = deviationsFromTrue1, bad = deviationsFromTrue2),
    memoryCoefficients = memCoeff,
    calibrationParameter = calibrationParameter
  )

  actual <- gatherQualityControlInfo(processedData)

  expect_equal(actual, expected)

})

test_that("writing of quality control data works", {

  # ----------------------------------------------------------------------------
  # expectations

  expected1 <- stringr::str_c(
    sprintf("piccr; version %s\n", utils::packageVersion("piccr")),
    "* config file: myconfig.yaml\n",
    sprintf("* processing date: %s xx:xx:xx\n", Sys.Date())
  )

  expected2 <- stringr::str_c(
    "\n# ----------------------------------------------\n",
    "\n# --- Summary of quality control information ---\n",
    "\n# ----------------------------------------------\n",
    "\n# --- Average data for entire processing run ---\n",
    "\n# RMSD of quality control standards:\n",
    "d18O = 0.07, dD = 0.7\n",
    "\n# RMSD of all standards:\n",
    "d18O = 0.36, dD = 3.6\n",
    "\n# Pooled standard deviation:\n",
    "d18O = 0.16, dD = 1.6\n",
    "\n# --- Specific data for each measurement file ---\n",
    "\n# RMSD of quality control standards:\n",
    capture.output(print(qc)) %>% paste(collapse = "\n"),
    "\n\n# RMSD of all standards:\n",
    capture.output(print(rmsdAll)) %>% paste(collapse = "\n"),
    "\n\n# Pooled standard deviation:\n",
    capture.output(print(pooledSD)) %>% paste(collapse = "\n")
  )

  expected3a <- stringr::str_c(
    "\n\n# --- Mean memory coefficients ---\n\n",
    capture.output(print(memCoeff[1 : 4, -1])) %>% paste(collapse = "\n")
  )

  expected3b <- stringr::str_c(
    "\n\n# --- Overall mean and file means of memory coefficients ---\n\n",
    capture.output(print(memCoeff)) %>% paste(collapse = "\n")
  )

  expected4 <- stringr::str_c(
    "\n\n# --- Calibration parameter for each measurement file ---\n\n",
    capture.output(print(calibrationParameter)) %>% paste(collapse = "\n")
  )

  expected5a <- stringr::str_c(
    "\n\n# --- Specific deviations from true standard values ---\n\n",
    "# ... displaying output for first 1 (of 2) measurement files;\n",
    "# adjust function parameter 'n' to display a different number.\n\n",
    "Dataset: good\n",
    capture.output(print(deviationsFromTrue1)) %>% paste(collapse = "\n")
  )

  expected5b <- stringr::str_c(
    "\n\n# --- Specific deviations from true standard values ---\n\n",
    "Dataset: good\n",
    capture.output(print(deviationsFromTrue1)) %>% paste(collapse = "\n"),
    "\n\nDataset: bad\n",
    capture.output(print(deviationsFromTrue2)) %>% paste(collapse = "\n")
  )

  # ----------------------------------------------------------------------------
  # test printing function

  expected <- stringr::str_c(expected2, expected3a, expected5a)

  actual <- capture.output(
    printQualityControl(processedData, printDeviations = TRUE, n = 1)) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  expected <- stringr::str_c(expected2, expected3b, expected5a)

  actual <- capture.output(
    printQualityControl(processedData, printDeviations = TRUE, n = 1,
                        whichMemoryCoefficients = "all")) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  expected <- stringr::str_c(expected2, expected3a, expected4)

  actual <- capture.output(
    printQualityControl(processedData, printMemoryCoefficients = TRUE,
                        printCalibrationParameter = TRUE)) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  expected <- stringr::str_c(expected2, expected4)

  actual <- capture.output(
    printQualityControl(processedData, printMemoryCoefficients = FALSE,
                        printCalibrationParameter = TRUE)) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  expect_warning(capture.output(printQualityControl(
    processedData, printDeviations = TRUE, n = 1,
    whichMemoryCoefficients = "wrong string")))

  # ----------------------------------------------------------------------------
  # test writing to output file

  config <- list(config_file_name = "myconfig.yaml")
  tmpfile <- tempfile()

  outputSummaryFile(processedData, config, tmpfile)

  expected <- stringr::str_c(expected1, expected2, expected3b,
                             expected4, expected5b)

  actual <- readr::read_file(tmpfile)

  # filter out variable time stamp string
  ncut <- 85
  ntime <- 9
  expected <- paste0(substr(expected, 1, ncut),
                     substr(expected, ncut + ntime, nchar(expected)))
  actual <- paste0(substr(actual, 1, ncut),
                     substr(actual, ncut + ntime, nchar(actual)))

  expect_equal(actual, expected)

})
