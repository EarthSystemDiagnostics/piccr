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
processedData <- list(
  good = list(
    name = "good",
    deviationOfControlStandard = list(name = "QC", d18O = 0.01, dD = 0.1),
    rmsdDeviationsFromTrue = list(d18O = 0.05, dD = 0.5),
    pooledSD = list(d18O = 0.03, dD = 0.3),
    deviationsFromTrue = deviationsFromTrue1
  ),
  bad = list(
    name = "bad",
    deviationOfControlStandard = list(name = "QC", d18O = 0.1, dD = 1),
    rmsdDeviationsFromTrue = list(d18O = 0.5, dD = 5),
    pooledSD = list(d18O = 0.3, dD = 3),
    deviationsFromTrue = deviationsFromTrue2
  )
)
qc <- tibble::tribble(
  ~file,  ~name, ~d18O, ~dD,
  # -- /  ---- / ---- / -- /
  "good", "QC",  0.01,  0.1,
  "bad",  "QC",  0.1,   1
)
rmsdAll <- tibble::tribble(
  ~file,  ~d18O, ~dD,
  # -- /  ---- / -- /
  "good", 0.05,  0.5,
  "bad",  0.5,   5
)
pooledSD <- tibble::tribble(
  ~file,  ~d18O, ~dD,
  # -- /  ---- / -- /
  "good", 0.03,  0.3,
  "bad",  0.3,   3
)

test_that("gathering of quality control data works", {
  
  expected <- list(
    rmsdQualityControl = qc,
    rmsdAllStandards = rmsdAll,
    pooledSD = pooledSD,
    deviationsFromTrue = list(
      good = deviationsFromTrue1, bad = deviationsFromTrue2)
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

  expected3 <- stringr::str_c(
    "\n\n# --- Specific deviations from true standard values ---\n\n",
    "# ... displaying output for first 1 (of 2) measurement files;\n",
    "# adjust function parameter 'n' to display a different number.\n\n",
    "$good\n",
    capture.output(print(deviationsFromTrue1)) %>% paste(collapse = "\n"),
    "\n"
  )

  expected4 <- stringr::str_c(
    "\n\n# --- Specific deviations from true standard values ---\n\n",
    "$good\n",
    capture.output(print(deviationsFromTrue1)) %>% paste(collapse = "\n"),
    "\n\n$bad\n",
    capture.output(print(deviationsFromTrue2)) %>% paste(collapse = "\n"),
    "\n"
  )

  # ----------------------------------------------------------------------------
  # test printing function

  expected <- stringr::str_c(expected2, expected3)

  actual <- capture.output(
    printQualityControl(processedData, printDeviations = TRUE, n = 1)) %>%
    paste(collapse = "\n")

  expect_equal(actual, expected)

  # ----------------------------------------------------------------------------
  # test writing to output file

  config <- list()
  tmpfile <- tempfile()
  configFile <- "myconfig.yaml"

  outputSummaryFile(processedData, config, configFile, tmpfile)

  expected <- stringr::str_c(expected1, expected2, expected4)

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
