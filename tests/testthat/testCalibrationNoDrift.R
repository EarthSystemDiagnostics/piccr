library(testthat)
library(tidyverse)

context("Test simple calibration without drift correction")

dataset <- tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~o18_True, ~H2_True,
  # -- / -------------- / -------- / -------------- / ----- / ------------ / -------- / ---------
   1,    "A",             1,         1.05,            1,      -5,            1,         -5,
   2,    "A",             2,         1,               1,      -5,            1,         -5,
   3,    "A",             3,         1.1,             1,      -5,            1,         -5,
   4,    "C",             1,         2,               1,      4,             2,         4,
   5,    "C",             2,         2.3,             1,      4,             2,         4,
   6,    "C",             3,         2.05,            1,      4,             2,         4,
   7,    "B",             1,         3,               1,      7,             3,         7,
   8,    "B",             2,         3.1,             1,      7,             3,         7,
   9,    "B",             3,         3,               1,      7,             3,         7
)
expected <- tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~o18_True, ~H2_True,
  # -- / -------------- / -------- / -------------- / ----- / ------------ / -------- / ---------
  1,    "A",             1,         0.99,             1,      -5,            1,         -5,
  2,    "A",             2,         0.94,             1,      -5,            1,         -5,
  3,    "A",             3,         1.04,             1,      -5,            1,         -5,
  4,    "C",             1,         1.93,             1,      4,             2,         4,
  5,    "C",             2,         2.23,             1,      4,             2,         4,
  6,    "C",             3,         1.98,             1,      4,             2,         4,
  7,    "B",             1,         2.93,             1,      7,             3,         7,
  8,    "B",             2,         3.03,             1,      7,             3,         7,
  9,    "B",             3,         2.93,             1,      7,             3,         7
)

test_that("test getCalibInterceptAndSlope", {
  
  o18InterceptExpected <- -0.05801953
  o18SlopeExpected <- 0.9958159
  H2InterceptExpected <- 0
  H2SlopeExpected <- 1
  
  actual <- getCalibInterceptAndSlope(dataset, config = NULL, block = 1)
  
  expect_equal(actual$d18O$intercept, o18InterceptExpected)
  expect_equal(actual$d18O$slope, o18SlopeExpected)
  
  expect_equal(actual$dD$intercept, H2InterceptExpected)
  expect_equal(actual$dD$slope, H2SlopeExpected)
})

test_that("test applyCalibration", {
  
  calibrationParams <- list(
    d18O = list(intercept = 5, slope = 0.9),
    dD = list(intercept = -2, slope = 1.3)
  )
  data <- tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol,
    # ------------ / ------------ / ----------
    15,              7,             "w",
    8,               12,            "x",
    -2,              0,             "y",
    0,               -30,           "z"
  )
  expected <- tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol,
    # ------------ / ------------ / ----------
    18.5,            7.1,            "w",
    12.2,            13.6,           "x",
    3.2,             -2,             "y",
    5,               -41,            "z"
  )
  
  actual <- applyCalibration(data, calibrationParams)
  actual <- mutate(actual, `d(D_H)Mean` = round(`d(D_H)Mean`, 1))
  
  expect_true(all_equal(actual, expected))
})

test_that("test calibrateNoDriftSingleDataset", {
  
  actual <- calibrateNoDriftSingleDataset(dataset, config = NULL, block = 1)
  actual <- mutate(actual, `d(18_16)Mean` = round(`d(18_16)Mean`, 2), `d(D_H)Mean` = round(`d(D_H)Mean`, 1))
  
  expect_true(all_equal(actual, expected))
})

test_that("test calibrateWithoutDriftCorrection", {
  
  actual <- calibrateWithoutDriftCorrection(list(df1 = dataset), config = NULL)
  actual[[1]] <- mutate(actual[[1]], `d(18_16)Mean` = round(`d(18_16)Mean`, 2), `d(D_H)Mean` = round(`d(D_H)Mean`, 1))
  
  expect_length(actual, 1)
  expect_true(all_equal(actual$df1, expected))
})
