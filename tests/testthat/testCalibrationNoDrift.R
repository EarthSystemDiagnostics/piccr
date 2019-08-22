library(testthat)
library(tidyverse)

context("Test simple calibration without drift correction")

dataset1 <- tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~o18_True, ~H2_True, ~useForMemCorr,
  # -- / -------------- / -------- / -------------- / ----- / ------------ / -------- / ------- / ---------------
   1,    "A",             1,         1.05,            1,      -5,            1,         -5,       TRUE,
   2,    "A",             2,         1,               1,      -5,            1,         -5,       TRUE,
   3,    "A",             3,         1.1,             1,      -5,            1,         -5,       TRUE,
   4,    "C",             1,         2,               1,      4,             2,         4,        TRUE,
   5,    "C",             2,         2.3,             1,      4,             2,         4,        TRUE,
   6,    "C",             3,         2.05,            1,      4,             2,         4,        TRUE,
   7,    "B",             1,         3,               1,      7,             3,         7,        TRUE,
   8,    "B",             2,         3.1,             1,      7,             3,         7,        TRUE,
   9,    "B",             3,         3,               1,      7,             3,         7,        TRUE
)
expected1 <- tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~o18_True, ~H2_True, ~useForMemCorr,
  # -- / -------------- / -------- / -------------- / ----- / ------------ / -------- / ------- / ---------------
  1,    "A",             1,         0.99,             1,      -5,            1,         -5,       TRUE,
  2,    "A",             2,         0.94,             1,      -5,            1,         -5,       TRUE,
  3,    "A",             3,         1.04,             1,      -5,            1,         -5,       TRUE,
  4,    "C",             1,         1.93,             1,      4,             2,         4,        TRUE,
  5,    "C",             2,         2.23,             1,      4,             2,         4,        TRUE,
  6,    "C",             3,         1.98,             1,      4,             2,         4,        TRUE,
  7,    "B",             1,         2.93,             1,      7,             3,         7,        TRUE,
  8,    "B",             2,         3.03,             1,      7,             3,         7,        TRUE,
  9,    "B",             3,         2.93,             1,      7,             3,         7,        TRUE
)

config <- list(use_memory_correction = TRUE, use_three_point_calibration = TRUE)

test_that("test getCalibInterceptAndSlope", {
  
  o18InterceptExpected <- -0.05801953
  o18SlopeExpected <- 0.9958159
  H2InterceptExpected <- 0
  H2SlopeExpected <- 1
  
  actual <- getCalibInterceptAndSlope(dataset1, config = config, useBlock = 1)
  
  expect_equal(actual$d18O$intercept, o18InterceptExpected)
  expect_equal(actual$d18O$slope, o18SlopeExpected)
  
  expect_equal(actual$dD$intercept, H2InterceptExpected)
  expect_equal(actual$dD$slope, H2SlopeExpected)
})

test_that("test getCalibInterceptAndSlope for dataset with rows that should be excluded", {
  
  # In this dataset only the first two rows should be used to determine calibration intercept and slope.
  dataset2 <- tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~block, ~o18_True, ~H2_True, ~useForMemCorr,
    # ------------ / ----------- / ----- / -------- / ------- / ---------------
    1,             2,            1,      1,         2,        TRUE,
    5,             3,            1,      5,         3,        TRUE,
    50,            100,          2,      1,         2,        TRUE,
    50,            100,          1,      1,         2,        FALSE,
    50,            100,          2,      1,         2,        FALSE,
    50,            100,          NA,     1,         2,        TRUE
  )
  
  actual <- getCalibInterceptAndSlope(dataset2, config = config, useBlock = 1)
  
  expect_equal(actual$d18O$intercept, 0)
  expect_equal(actual$d18O$slope, 1)
  
  expect_equal(actual$dD$intercept, 0)
  expect_equal(actual$dD$slope, 1)
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
  
  actual <- calibrateNoDriftSingleDataset(dataset1, config = config, block = 1)
  actual <- mutate(actual, `d(18_16)Mean` = round(`d(18_16)Mean`, 2), `d(D_H)Mean` = round(`d(D_H)Mean`, 1))
  
  expect_true(all_equal(actual, expected1))
})

test_that("test calibrateWithoutDriftCorrection", {
  
  actual <- calibrateWithoutDriftCorrection(list(df1 = dataset1), config = config)
  actual[[1]] <- mutate(actual[[1]], `d(18_16)Mean` = round(`d(18_16)Mean`, 2), `d(D_H)Mean` = round(`d(D_H)Mean`, 1))
  
  expect_length(actual, 1)
  expect_true(all_equal(actual$df1, expected1))
})

test_that("test use only last three injections if memory correction is not used", {
  
  dataset3 <- tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~o18_True, ~H2_True, ~useForMemCorr,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / -------- / ------- / ---------------
    1,    "A",             1,         100,              1,      100,           2,        -5,        TRUE,
    2,    "A",             2,         2,                1,      -5,            2,        -5,        TRUE,
    3,    "A",             3,         2,                1,      -5,            2,        -5,        TRUE,
    3,    "A",             4,         2,                1,      -5,            2,        -5,        TRUE,
    4,    "C",             1,         100,              1,      100,           5,         4,        TRUE,
    5,    "C",             2,         100,              1,      100,           5,         4,        TRUE,
    6,    "C",             3,         5,                1,      4,             5,         4,        TRUE,
    6,    "C",             4,         5,                1,      4,             5,         4,        TRUE,
    6,    "C",             5,         5,                1,      4,             5,         4,        TRUE,
    7,    "B",             1,         -2,               1,      7,             -2,        7,        TRUE,
    8,    "B",             2,         -2,               1,      7,             -2,        7,        TRUE,
    9,    "B",             3,         -2,               1,      7,             -2,        7,        TRUE
  )
  
  config <- list(use_memory_correction = FALSE, use_three_point_calibration = TRUE)
  
  actual <- getCalibInterceptAndSlope(dataset3, config = config, useBlock = 1)
  
  expect_equal(actual$d18O$intercept, 0)
  expect_equal(actual$d18O$slope, 1)
  
  expect_equal(actual$dD$intercept, 0)
  expect_equal(actual$dD$slope, 1)
})

test_that("test two point calibration", {
  
  dataset4 <- tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~o18_True, ~H2_True, ~useForMemCorr,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / -------- / ------- / ---------------
    1,    "A",             1,         100,              1,      100,           2,        -5,       TRUE,
    2,    "A",             2,         2,                1,      -5,            2,        -5,       TRUE,
    3,    "A",             3,         2,                1,      -5,            2,        -5,       TRUE,
    4,    "A",             4,         2,                1,      -5,            2,        -5,       TRUE,
    5,    "C",             1,         100,              1,      100,           5,         4,        TRUE,
    6,    "C",             2,         100,              1,      100,           5,         4,        TRUE,
    7,    "C",             3,         100,              1,      4,             5,         4,        TRUE,
    8,    "C",             4,         100,              1,      4,             5,         4,        TRUE,
    9,    "C",             5,         100,              1,      4,             5,         4,        TRUE,
    10,   "B",             1,         -2,               1,      7,             -2,        7,        TRUE,
    11,   "B",             2,         -2,               1,      7,             -2,        7,        TRUE,
    12,   "B",             3,         -2,               1,      7,             -2,        7,        TRUE
  )
  
  config <- list(use_memory_correction = FALSE, use_three_point_calibration = FALSE)
  
  actual <- getCalibInterceptAndSlope(dataset4, config = config, useBlock = 1)
  
  expect_equal(actual$d18O$intercept, 0)
  expect_equal(actual$d18O$slope, 1)
  
  expect_equal(actual$dD$intercept, 0)
  expect_equal(actual$dD$slope, 1)
})