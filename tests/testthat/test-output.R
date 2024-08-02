# ------------------------------------------------------------------------------
# ----- testing writing data to file -------------------------------------------
# ------------------------------------------------------------------------------

d1Content <- "Identifier 1,a,b\na,1.2,3.1\n"
d2Content <- "Identifier 1,c,d\na,3.1,\nb,9,-2\n"

config <- list(output_directory = tempdir(), include_standards_in_output = TRUE)
datasets <- list(
  list(name = "d1.csv", processed = readr::read_csv(d1Content)), 
  list(name = "d2.csv", processed = readr::read_csv(d2Content))
)

test_that("correct files are created when output dir exists", {

  writeDataToFile(datasets, config)
  
  expect_true(file.exists(file.path(tempdir(), "d1.csv")))
  expect_true(file.exists(file.path(tempdir(), "d2.csv")))

})

test_that("correct files are created when output dir does not exist", {
  
  # make sure that the directory does not exist
  unlink("some_dir_that_does_not_exist", recursive = TRUE)
  stopifnot(!dir.exists("some_dir_that_does_not_exist"))
  
  config <- list(output_directory = "some_dir_that_does_not_exist",
                 include_standards_in_output = TRUE)
  
  writeDataToFile(datasets, config)
  
  expect_true(file.exists(file.path("some_dir_that_does_not_exist", "d1.csv")))
  expect_true(file.exists(file.path("some_dir_that_does_not_exist", "d2.csv")))
  
  unlink("some_dir_that_does_not_exist", recursive = TRUE)

})

test_that("file contents are correct", {

  writeDataToFile(datasets, config)
  
  expect_identical(readr::read_file(file.path(tempdir(), "d1.csv")),
                   d1Content)
  expect_identical(readr::read_file(file.path(tempdir(), "d2.csv")),
                   d2Content)

})

test_that("omitting standards from output works when standard is in file", {

  config <- list(output_directory = tempdir(), 
                 include_standards_in_output = FALSE,
                 standards = list(list(name = "a"), list(name = "c")))
  
  writeDataToFile(datasets, config)
  
  expect_identical(readr::read_file(file.path(tempdir(), "d1.csv")),
                   "Identifier 1,a,b\n")
  
  expect_identical(readr::read_file(file.path(tempdir(), "d2.csv")),
                   "Identifier 1,c,d\nb,9,-2\n")

})

test_that("omitting standards from output works when only probes are in file", {

  config <- list(output_directory = tempdir(), 
                 include_standards_in_output = FALSE,
                 standards = list(list(name = "c")))
  
  writeDataToFile(datasets, config)
  
  expect_identical(readr::read_file(file.path(tempdir(), "d1.csv")),
                   d1Content)
  
  expect_identical(readr::read_file(file.path(tempdir(), "d2.csv")),
                   d2Content)

})

test_that("omitting standards from output works when standard list is empty)", {

  config <- list(output_directory = tempdir(), 
                 include_standards_in_output = FALSE,
                 standards = list())
  
  writeDataToFile(datasets, config)
  
  expect_identical(readr::read_file(file.path(tempdir(), "d1.csv")),
                   d1Content)
  
  expect_identical(readr::read_file(file.path(tempdir(), "d2.csv")),
                   d2Content)

})

# ------------------------------------------------------------------------------
# ----- testing output of summary file -----------------------------------------
# ------------------------------------------------------------------------------

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
driftParameter1 <- tibble::tribble(
  ~species, ~sample, ~slope, ~other_cols,
  # ----- / ------ / ----- / -----------
  "d18O",   "StdA",  0,      "foo",
  "d18O",   "StdB",  0,      "foo",
  "d18O",   "mean",  0,      "foo",
  "dD",     "StdA",  0,      "foo",
  "dD",     "StdB",  0,      "foo",
  "dD",     "mean",  0,      "foo"
)
driftParameter2 <- tibble::tribble(
  ~species, ~sample, ~slope, ~other_cols,
  # ----- / ------ / ----- / -----------
  "d18O",   "StdA",  1,      "foo",
  "d18O",   "StdB",  5,      "foo",
  "d18O",   "mean",  3,      "foo",
  "dD",     "StdA",  -10,    "foo",
  "dD",     "StdB",  -70,    "foo",
  "dD",     "mean",  -40,    "foo"
)
processedData <- list(
  good = list(
    name = "good",
    deviationOfControlStandard = list(name = "QC", d18O = 0.01, dD = 0.1),
    rmsdDeviationsFromTrue = list(d18O = 0.05, dD = 0.5),
    pooledSD = list(d18O = 0.03, dD = 0.3),
    deviationsFromTrue = deviationsFromTrue1,
    memoryCoefficients = memoryCoefficients1,
    calibrationParams = calibrationParameter1,
    driftParams = driftParameter1
  ),
  bad = list(
    name = "bad",
    deviationOfControlStandard = list(name = "QC", d18O = 0.1, dD = 1),
    rmsdDeviationsFromTrue = list(d18O = 0.5, dD = 5),
    pooledSD = list(d18O = 0.3, dD = 3),
    deviationsFromTrue = deviationsFromTrue2,
    memoryCoefficients = memoryCoefficients2,
    calibrationParams = calibrationParameter2,
    driftParams = driftParameter2
  )
)
processedDataNoDrift <- lapply(processedData, function(x) {
  x$driftParams <- NULL
  c(x, list(driftParams = NULL))
})
processedDataNoMemory <- lapply(processedData, function(x) {
  x$memoryCoefficients <- NULL
  c(x, list(memoryCoefficients = NULL))
})
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
driftParameter <- tibble::tribble(
  ~dataset, ~species, ~sample, ~slope, ~other_cols,
  # ----- / ------- / ------ / ----- / -----------
  "good",   "d18O",   "StdA",  0,      "foo",
  "good",   "d18O",   "StdB",  0,      "foo",
  "good",   "d18O",   "mean",  0,      "foo",
  "good",   "dD",     "StdA",  0,      "foo",
  "good",   "dD",     "StdB",  0,      "foo",
  "good",   "dD",     "mean",  0,      "foo",
  "bad",    "d18O",   "StdA",  1,      "foo",
  "bad",    "d18O",   "StdB",  5,      "foo",
  "bad",    "d18O",   "mean",  3,      "foo",
  "bad",    "dD",     "StdA",  -10,    "foo",
  "bad",    "dD",     "StdB",  -70,    "foo",
  "bad",    "dD",     "mean",  -40,    "foo"
)

test_that("gathering of quality control data works", {
  
  expected <- list(
    rmsdQualityControl = qc,
    rmsdAllStandards = rmsdAll,
    pooledSD = pooledSD,
    deviationsFromTrue = list(
      good = deviationsFromTrue1, bad = deviationsFromTrue2),
    memoryCoefficients = memCoeff,
    calibrationParameter = calibrationParameter,
    driftParameter = driftParameter
  )

  actual <- gatherQualityControlInfo(processedData)

  expect_equal(actual, expected)

  expectedNoDrift <- expected
  expectedNoDrift$driftParameter <- NULL
  expectedNoDrift <- c(expectedNoDrift, list(driftParameter = NULL))

  actual <- gatherQualityControlInfo(processedDataNoDrift)

  expect_equal(actual, expectedNoDrift)

  expectedNoMemory <- expected
  expectedNoMemory$memoryCoefficients <- NULL
  expectedNoMemory <- c(expectedNoMemory,
                        list(memoryCoefficients = NULL))[c(1 : 4, 7, 5, 6)]

  actual <- gatherQualityControlInfo(processedDataNoMemory)

  expect_equal(actual, expectedNoMemory)

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

  expected5 <- stringr::str_c(
    "\n\n# --- Drift correction parameter for each measurement file ---\n\n",
    capture.output(print(driftParameter)) %>% paste(collapse = "\n")
  )

  expected6a <- stringr::str_c(
    "\n\n# --- Specific deviations from true standard values ---\n\n",
    "# ... displaying output for first 1 (of 2) measurement files;\n",
    "# adjust function parameter 'n' to display a different number.\n\n",
    "Dataset: good\n",
    capture.output(print(deviationsFromTrue1)) %>% paste(collapse = "\n")
  )

  expected6b <- stringr::str_c(
    "\n\n# --- Specific deviations from true standard values ---\n\n",
    "Dataset: good\n",
    capture.output(print(deviationsFromTrue1)) %>% paste(collapse = "\n"),
    "\n\nDataset: bad\n",
    capture.output(print(deviationsFromTrue2)) %>% paste(collapse = "\n")
  )

  # ----------------------------------------------------------------------------
  # test printing function

  # mean memory coefficients and specific deviations for one file
  expected <- stringr::str_c(expected2, expected3a, expected6a)

  actual <- capture.output(
    printQualityControl(processedData, printDeviations = TRUE, n = 1)) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  # all memory coefficients and specific deviations for one file
  expected <- stringr::str_c(expected2, expected3b, expected6a)

  actual <- capture.output(
    printQualityControl(processedData, printDeviations = TRUE, n = 1,
                        whichMemoryCoefficients = "all")) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  # mean memory coefficients and calibration parameters
  expected <- stringr::str_c(expected2, expected3a, expected4)

  actual <- capture.output(
    printQualityControl(processedData, printMemoryCoefficients = TRUE,
                        printCalibrationParameter = TRUE)) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  # only calibration parameters
  expected <- stringr::str_c(expected2, expected4)

  actual <- capture.output(
    printQualityControl(processedData, printMemoryCoefficients = FALSE,
                        printCalibrationParameter = TRUE)) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  # warning due to invalid string for memory coefficients
  expect_warning(capture.output(printQualityControl(
    processedData, printDeviations = TRUE, n = 1,
    whichMemoryCoefficients = "wrong string")))

  # only drift parameters
  expected <- stringr::str_c(expected2, expected5)

  actual <- capture.output(
    printQualityControl(processedData, printMemoryCoefficients = FALSE,
                        printDriftParameter = TRUE)) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  # mean memory coefficients, calibration parameters, drift parameters, and
  # specific deviations for one file
  expected <- stringr::str_c(expected2, expected3a, expected4,
                             expected5, expected6a)

  actual <- capture.output(
    printQualityControl(processedData,
                        printCalibrationParameter = TRUE,
                        printDriftParameter = TRUE,
                        printDeviations = TRUE, n = 1)) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  # drift parameters shouldn't print
  expected <- stringr::str_c(expected2, expected3a)

  actual <- capture.output(
    printQualityControl(processedDataNoDrift, printDriftParameter = TRUE)) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  # memory coefficients shouldn't print
  expected <- stringr::str_c(expected2)

  actual <- capture.output(
    printQualityControl(processedDataNoMemory)) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  # ----------------------------------------------------------------------------
  # test writing to output file

  config <- list(config_file_name = "myconfig.yaml")
  tmpfile <- tempfile()

  outputSummaryFile(processedData, config, tmpfile)

  expected <- stringr::str_c(expected1, expected2, expected3b,
                             expected4, expected5, expected6b)

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

# ------------------------------------------------------------------------------
# ----- processing data for output ---------------------------------------------
# ------------------------------------------------------------------------------

test_that("quality control output structure is correct", {

  dataset1 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~block, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~dExcess, ~o18_True, ~H2_True, ~useAsControlStandard, ~Sample, ~vial_group,
    # -- / -------------- / -------------- / ----- / -------- / -------------- / ------------ / --------/ ---------/ --------/ ---------------------/ -------/ -----------
    1,     "WU",            "w",             1,      1,         0.9,             8.5,           15,       1,         10,       FALSE,                 1,       1,
    2,     "WU",            "w",             1,      2,         1,               9,             20,       1,         10,       FALSE,                 1,       1,
    3,     "WU",            "w",             1,      3,         1.1,             10.7,          25,       1,         10,       FALSE,                 1,       1,
    4,     "C",             "x",             1,      1,         1.9,             19,            15,       2,         20,       FALSE,                 2,       1,
    5,     "C",             "x",             1,      2,         2.1,             20.7,          20,       2,         20,       FALSE,                 2,       1,
    6,     "C",             "x",             1,      3,         2,               22.1,          25,       2,         20,       FALSE,                 2,       1,
    7,     "probe1",        "p",             NA,     1,         4,               49,            4,        NA,        NA,       FALSE,                 3,       1,
    8,     "probe1",        "p",             NA,     2,         5,               49,            5,        NA,        NA,       FALSE,                 3,       1,
    9,     "QC",            "qq",            2,      1,         10.4,            95,            11,       10,        100,      TRUE,                  4,       1,
    10,    "QC",            "qq",            2,      2,         9.8,             102.5,         9,        10,        100,      TRUE,                  4,       1,
    11,    "probe2",        "pp",            NA,     1,         6,               60,            4,        NA,        NA,       FALSE,                 5,       1,
    12,    "probe2",        "pp",            NA,     2,         7,               71,            5,        NA,        NA,       FALSE,                 5,       1,
    13,    "B",             "z",             3,      1,         2.8,             28.5,          11,       3,         30,       FALSE,                 6,       1,
    14,    "B",             "z",             3,      2,         3.2,             31,            9,        3,         30,       FALSE,                 6,       1,
    15,    "C",             "x",             3,      1,         2.3,             19.1,           -2,       2,         20,      FALSE,                 7,       2,
    16,    "C",             "x",             3,      2,         2.45,            22.7,           2,        2,         20,      FALSE,                 7,       2
  )
  expected1 <- tibble::tribble(
    ~Sample, ~`Identifier 1`, ~block, ~d18OMeasured, ~d18OTrue, ~d18ODeviation, ~dDMeasured, ~dDTrue, ~dDDeviation,
    # -----/ ---------------/ ------/ -------------/ ---------/ --------------/ -----------/ -------/ ------------
    1,       "WU",            1,      1,             1,         0,              9.4,         10,      0.6,
    2,       "C",             1,      2,             2,         0,              20.6,        20,      -0.6,
    4,       "QC",            2,      10.1,          10,        -0.1,           98.75,       100,     1.25,
    6,       "B",             3,      3,             3,         0,              29.75,       30,      0.25,
    7,       "C",             3,      2.375,         2,         -0.375,         20.9,        20,      -0.9
    )
  expected2 <- list(name = "QC", d18O = -0.1, dD = 1.25)
  expected3 <- list(d18O = 0.194, dD = 0.836)
  expected4 <- list(d18O = 0.382, dD = 3.427)
  
  actual1 <- accumulateMeasurements(dataset1, list(average_over_inj = "all"))
  actual2 <- getQualityControlInfo(dataset1, actual1)
  
  expect_is(actual1, "data.frame")
  expect_length(actual2, 4)

  expect_equal(
    dplyr::mutate_if(actual2$deviationsFromTrue, is.numeric, round, digits = 5),
    expected1)

  expect_equal(actual2$deviationOfControlStandard, expected2)
  expect_equal(lapply(actual2$rmsdDeviationsFromTrue, round, 3), expected3)
  expect_equal(lapply(actual2$pooledSD, round, 3), expected4)

})
