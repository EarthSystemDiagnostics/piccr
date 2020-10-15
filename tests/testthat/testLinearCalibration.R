library(tibble)
library(dplyr)

context("Test simple calibration without drift correction")

# In this data set, o18_True is calculated from d(18_16)Mean applying
# a slope = 0.9 and an intercept = -2.
dataset1 <- tibble::tribble(
  ~Line, ~`Time Code`, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~o18_True, ~H2_True, ~useForCalibration,
  # -- / ----------- / -------------- / -------- / -------------- / ----- / ------------ / -------- / ------- / ---------------
   1,    "2020/10/1400:00:01", "A",     1,         1,               1,      -5,            -1.1,      -5,       TRUE,
   2,    "2020/10/1400:00:02", "A",     2,         1,               1,      -5,            -1.1,      -5,       TRUE,
   3,    "2020/10/1400:00:03", "A",     3,         1,               1,      -5,            -1.1,      -5,       TRUE,
   4,    "2020/10/1400:00:04", "C",     1,         2,               1,      4,             -0.2,      4,        TRUE,
   5,    "2020/10/1400:00:05", "C",     2,         2,               1,      4,             -0.2,      4,        TRUE,
   6,    "2020/10/1400:00:06", "C",     3,         2,               1,      4,             -0.2,      4,        TRUE,
   7,    "2020/10/1400:00:07", "B",     1,         3,               1,      7,             0.7,       7,        TRUE,
   8,    "2020/10/1400:00:08", "B",     2,         3,               1,      7,             0.7,       7,        TRUE,
   9,    "2020/10/1400:00:09", "B",     3,         3,               1,      7,             0.7,       7,        TRUE
)
expected1 <- tibble::tribble(
  ~Line, ~`Time Code`, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~o18_True, ~H2_True, ~useForCalibration,
  # -- / ----------- / -------------- / -------- / -------------- / ----- / ------------ / -------- / ------- / ---------------
  1,    "2020/10/1400:00:01", "A",      1,         -1.1,             1,      -5,            -1.1,      -5,       TRUE,
  2,    "2020/10/1400:00:02", "A",      2,         -1.1,             1,      -5,            -1.1,      -5,       TRUE,
  3,    "2020/10/1400:00:03", "A",      3,         -1.1,             1,      -5,            -1.1,      -5,       TRUE,
  4,    "2020/10/1400:00:04", "C",      1,         -0.2,             1,      4,             -0.2,      4,        TRUE,
  5,    "2020/10/1400:00:05", "C",      2,         -0.2,             1,      4,             -0.2,      4,        TRUE,
  6,    "2020/10/1400:00:06", "C",      3,         -0.2,             1,      4,             -0.2,      4,        TRUE,
  7,    "2020/10/1400:00:07", "B",      1,         0.7,              1,      7,             0.7,       7,        TRUE,
  8,    "2020/10/1400:00:08", "B",      2,         0.7,              1,      7,             0.7,       7,        TRUE,
  9,    "2020/10/1400:00:09", "B",      3,         0.7,              1,      7,             0.7,       7,        TRUE
)

expected1ParamsD18O <- tibble::tibble(
  species = "d18O",
  block = 1,
  timeStamp = 4,
  intercept = -2.,
  slope = 0.9,
  pValueIntercept = 0,
  pValueSlope = 0,
  rSquared = 1
)

expected1ParamsDD <- tibble::tibble(
  species = "dD",
  block = 1,
  timeStamp = 4,
  intercept = 0.,
  slope = 1.,
  pValueIntercept = 0.26,
  pValueSlope = 0,
  rSquared = 1
)

config <- list(use_memory_correction = TRUE, use_three_point_calibration = TRUE)

test_that("running the calibration model", {

  # should throw an error
  expect_error(runCalibrationModel(dataset1, species = "unknown"))

  expectedD18O <- tibble::tibble(
    species = "d18O",
    block = 1,
    timeStamp = 1,
    intercept = -2.,
    slope = 0.9,
    pValueIntercept = 0,
    pValueSlope = 0,
    rSquared = 1
  )

  expectedDD <- tibble::tibble(
    species = "dD",
    block = 1,
    timeStamp = 1,
    foo = "bla",
    intercept = 0.,
    slope = 1.,
    pValueIntercept = 0.26,
    pValueSlope = 0,
    rSquared = 1
  )

  actualD18O <- runCalibrationModel(dataset1, species = "d18O",
                                    block = 1, timeStamp = 1)

  actualDD   <- runCalibrationModel(dataset1, species = "dD",
                                    block = 1, timeStamp = 1, foo = "bla")

  expect_equal(actualD18O, expectedD18O)
  expect_equal(actualDD, expectedDD)

})

test_that("test getCalibration", {
  
  o18InterceptExpected <- -2.
  o18SlopeExpected <- 0.9
  H2InterceptExpected <- 0
  H2SlopeExpected <- 1

  actual <- getCalibration(dataset1, config = config, useBlock = 1)
  
  expect_equal(actual$d18O$intercept, o18InterceptExpected)
  expect_equal(actual$d18O$slope, o18SlopeExpected)
  
  expect_equal(actual$dD$intercept, H2InterceptExpected)
  expect_equal(actual$dD$slope, H2SlopeExpected)
})

test_that("test getCalibration for dataset with rows that should be excluded", {
  
  # In this dataset only the first two rows should be used to determine calibration intercept and slope.
  dataset2 <- tibble::tribble(
    ~`Time Code`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~block, ~o18_True, ~H2_True, ~useForCalibration,
    # --------- / ------------ / ----------- / ----- / -------- / ------- / ---------------
    "2020/10/1400:00:01", 1,     2,            1,      1,         2,        TRUE,
    "2020/10/1400:00:02", 5,     3,            1,      5,         3,        TRUE,
    "2020/10/1400:00:03", 50,    100,          2,      1,         2,        TRUE,
    "2020/10/1400:00:04", 50,    100,          1,      1,         2,        FALSE,
    "2020/10/1400:00:05", 50,    100,          2,      1,         2,        FALSE,
    "2020/10/1400:00:06", 50,    100,          NA,     1,         2,        TRUE
  )
  
  actual <- getCalibration(dataset2, config = config, useBlock = 1)
  
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
  data <- tibble::tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol,
    # ------------ / ------------ / ----------
    15,              7,             "w",
    8,               12,            "x",
    -2,              0,             "y",
    0,               -30,           "z"
  )
  expected <- tibble::tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol,
    # ------------ / ------------ / ----------
    18.5,            7.1,            "w",
    12.2,            13.6,           "x",
    3.2,             -2,             "y",
    5,               -41,            "z"
  )
  
  actual <- applyCalibration(data, calibrationParams)
  actual <- dplyr::mutate(actual, `d(D_H)Mean` = round(`d(D_H)Mean`, 1))
  
  expect_equal(actual, expected)
})

test_that("test simple linear calibration", {

  expected <- list(
    dataset = expected1,
    parameter = list(d18O = expected1ParamsD18O, dD = expected1ParamsDD)
  )

  actual <- linearCalibration(dataset1, config = config, block = 1)

  expect_type(actual, "list")
  expect_length(actual, 2)

  actual$dataset <- dplyr::mutate(actual$dataset, `d(18_16)Mean` = round(`d(18_16)Mean`, 2), `d(D_H)Mean` = round(`d(D_H)Mean`, 1))
  
  expect_equal(actual, expected)

  actual <- linearCalibration(dataset1, config = config)
  actual$dataset <- dplyr::mutate(actual$dataset, `d(18_16)Mean` = round(`d(18_16)Mean`, 2), `d(D_H)Mean` = round(`d(D_H)Mean`, 1))
  
  expect_equal(actual, expected)
})

test_that("test use only last three injections if memory correction is not used", {
  
  dataset3 <- tibble::tribble(
    ~Line, ~`Time Code`, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~o18_True, ~H2_True, ~useForCalibration, ~vial_group,
    # -- / ----------- / -------------- / -------- / -------------- / ----- / ------------ / -------- / ------- / ----------------- / -----------
    1,    "2020/10/1400:00:01", "A",      1,         100,              1,      100,           2,        -5,        TRUE,               1,
    2,    "2020/10/1400:00:02", "A",      2,         2,                1,      -5,            2,        -5,        TRUE,               1,
    3,    "2020/10/1400:00:03", "A",      3,         2,                1,      -5,            2,        -5,        TRUE,               1,
    3,    "2020/10/1400:00:04", "A",      4,         2,                1,      -5,            2,        -5,        TRUE,               1,
    4,    "2020/10/1400:00:05", "C",      1,         100,              1,      100,           5,         4,        TRUE,               1,
    5,    "2020/10/1400:00:06", "C",      2,         100,              1,      100,           5,         4,        TRUE,               1,
    6,    "2020/10/1400:00:07", "C",      3,         5,                1,      4,             5,         4,        TRUE,               1,
    6,    "2020/10/1400:00:08", "C",      4,         5,                1,      4,             5,         4,        TRUE,               1,
    6,    "2020/10/1400:00:09", "C",      5,         5,                1,      4,             5,         4,        TRUE,               1,
    7,    "2020/10/1400:00:10", "B",      1,         -2,               1,      7,             -2,        7,        TRUE,               1,
    8,    "2020/10/1400:00:11", "B",      2,         -2,               1,      7,             -2,        7,        TRUE,               1,
    9,    "2020/10/1400:00:12", "B",      3,         -2,               1,      7,             -2,        7,        TRUE,               1,
  )
  
  config <- list(use_memory_correction = FALSE, use_three_point_calibration = TRUE)
  
  actual <- getCalibration(dataset3, config = config, useBlock = 1)
  
  expect_equal(actual$d18O$intercept, 0)
  expect_equal(actual$d18O$slope, 1)
  
  expect_equal(actual$dD$intercept, 0)
  expect_equal(actual$dD$slope, 1)
})

test_that("test two point calibration", {
  
  dataset4 <- tibble::tribble(
    ~Line, ~`Time Code`, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~o18_True, ~H2_True, ~useForCalibration, ~vial_group,
    # -- / ----------- / -------------- / -------- / -------------- / ----- / ------------ / -------- / ------- / ----------------- / -----------
    1,    "2020/10/1400:00:01", "C",      1,         0,                1,      100,           5,         4,        TRUE,               1,
    2,    "2020/10/1400:00:02", "C",      2,         0,                1,      100,           5,         4,        TRUE,               1,
    3,    "2020/10/1400:00:03", "C",      3,         0,                1,      4,             5,         4,        TRUE,               1,
    4,    "2020/10/1400:00:04", "C",      4,         0,                1,      4,             5,         4,        TRUE,               1,
    5,    "2020/10/1400:00:05", "C",      5,         0,                1,      4,             5,         4,        TRUE,               1,
    6,    "2020/10/1400:00:06", "A",      1,         100,              1,      100,           2,        -5,        TRUE,               1,
    7,    "2020/10/1400:00:07", "A",      2,         2,                1,      -5,            2,        -5,        TRUE,               1,
    8,    "2020/10/1400:00:08", "A",      3,         2,                1,      -5,            2,        -5,        TRUE,               1,
    9,    "2020/10/1400:00:09", "A",      4,         2,                1,      -5,            2,        -5,        TRUE,               1,
    10,   "2020/10/1400:00:10", "B",      1,         -2,               1,      7,             -2,        7,        TRUE,               1,
    11,   "2020/10/1400:00:11", "B",      2,         -2,               1,      7,             -2,        7,        TRUE,               1,
    12,   "2020/10/1400:00:12", "B",      3,         -2,               1,      7,             -2,        7,        TRUE,               1
  )
  
  config <- list(use_memory_correction = FALSE, use_three_point_calibration = FALSE)
  
  actual <- getCalibration(dataset4, config = config, useBlock = 1)
  
  expect_equal(actual$d18O$intercept, 0)
  expect_equal(actual$d18O$slope, 1)
  
  expect_equal(actual$dD$intercept, 0)
  expect_equal(actual$dD$slope, 1)
})

test_that("test training data for grouped vials", {

  config <- list(use_memory_correction = FALSE,
                 use_three_point_calibration = TRUE)

  dataset5 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~useForCalibration, ~vial_group,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / ----------------- / -----------
    1,     "A",             1,         1,               1,      10,            TRUE,               1,
    2,     "A",             2,         1,               1,      10,            TRUE,               1,
    3,     "A",             3,         1,               1,      10,            TRUE,               1,
    4,     "A",             4,         1,               1,      10,            TRUE,               1,
    5,     "B",             1,         2,               1,      20,            TRUE,               1,
    6,     "B",             2,         2,               1,      20,            TRUE,               1,
    7,     "B",             3,         2,               1,      20,            TRUE,               1,
    8,     "B",             4,         2,               1,      20,            TRUE,               1,
    9,     "C",             4,         3,               1,      30,            TRUE,               1,
    10,    "C",             1,         3,               1,      30,            TRUE,               1,
    11,    "C",             2,         3,               1,      30,            TRUE,               1,
    12,    "C",             3,         3,               1,      30,            TRUE,               1,
    13,    "A",             1,         1,               1,      10,            TRUE,               2,
    14,    "A",             2,         1,               1,      10,            TRUE,               2,
    15,    "A",             3,         1,               1,      10,            TRUE,               2,
    16,    "A",             4,         1,               1,      10,            TRUE,               2
  )
  expected <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~useForCalibration, ~vial_group,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / ----------------- / -----------
    2,     "A",             2,         1,               1,      10,            TRUE,               1,
    3,     "A",             3,         1,               1,      10,            TRUE,               1,
    4,     "A",             4,         1,               1,      10,            TRUE,               1,
    6,     "B",             2,         2,               1,      20,            TRUE,               1,
    7,     "B",             3,         2,               1,      20,            TRUE,               1,
    8,     "B",             4,         2,               1,      20,            TRUE,               1,
    10,    "C",             1,         3,               1,      30,            TRUE,               1,
    11,    "C",             2,         3,               1,      30,            TRUE,               1,
    12,    "C",             3,         3,               1,      30,            TRUE,               1,
    14,    "A",             2,         1,               1,      10,            TRUE,               2,
    15,    "A",             3,         1,               1,      10,            TRUE,               2,
    16,    "A",             4,         1,               1,      10,            TRUE,               2
  )

  actual5 <- getTrainingData(dataset5, config, useBlock = 1)

  expect_equal(actual5, expected)

  config <- list(use_memory_correction = TRUE,
                 use_three_point_calibration = FALSE)

  dataset6 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~useForCalibration, ~vial_group,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / ----------------- / -----------
    1,     "A",             1,         NA,               1,     NA,            TRUE,               1,
    2,     "A",             2,         NA,               1,     NA,            TRUE,               1,
    3,     "A",             3,         NA,               1,     NA,            TRUE,               1,
    4,     "A",             4,         NA,               1,     NA,            TRUE,               1,
    5,     "B",             1,         2,               1,      20,            TRUE,               1,
    6,     "B",             2,         2,               1,      20,            TRUE,               1,
    7,     "B",             3,         2,               1,      20,            TRUE,               1,
    8,     "B",             4,         2,               1,      20,            TRUE,               1,
    9,     "C",             4,         3,               1,      30,            TRUE,               1,
    10,    "C",             1,         3,               1,      30,            TRUE,               1,
    11,    "C",             2,         3,               1,      30,            TRUE,               1,
    12,    "C",             3,         3,               1,      30,            TRUE,               1,
    13,    "A",             1,         1,               1,      10,            TRUE,               2,
    14,    "A",             2,         1,               1,      10,            TRUE,               2,
    15,    "A",             3,         1,               1,      10,            TRUE,               2,
    16,    "A",             4,         1,               1,      10,            TRUE,               2
  )
  expected <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~useForCalibration, ~vial_group,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / ----------------- / -----------
    1,     "A",             1,         NA,               1,     NA,            TRUE,               1,
    2,     "A",             2,         NA,               1,     NA,            TRUE,               1,
    3,     "A",             3,         NA,               1,     NA,            TRUE,               1,
    4,     "A",             4,         NA,               1,     NA,            TRUE,               1,
    9,     "C",             4,         3,               1,      30,            TRUE,               1,
    10,    "C",             1,         3,               1,      30,            TRUE,               1,
    11,    "C",             2,         3,               1,      30,            TRUE,               1,
    12,    "C",             3,         3,               1,      30,            TRUE,               1,
    13,    "A",             1,         1,               1,      10,            TRUE,               2,
    14,    "A",             2,         1,               1,      10,            TRUE,               2,
    15,    "A",             3,         1,               1,      10,            TRUE,               2,
    16,    "A",             4,         1,               1,      10,            TRUE,               2
  )

  actual6 <- getTrainingData(dataset6, config, useBlock = 1)

  expect_equal(actual6, expected)

  config <- list(use_memory_correction = FALSE,
                 use_three_point_calibration = FALSE)

  dataset7 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~useForCalibration, ~vial_group,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / ----------------- / -----------
    1,     "A",             1,         1,               1,      10,            FALSE,              1,
    2,     "A",             2,         1,               1,      10,            FALSE,              1,
    3,     "A",             3,         1,               1,      10,            FALSE,              1,
    4,     "A",             4,         1,               1,      10,            FALSE,              1,
    5,     "B",             1,         2,               1,      20,            TRUE,               1,
    6,     "B",             2,         2,               1,      20,            TRUE,               1,
    7,     "B",             3,         2,               1,      20,            TRUE,               1,
    8,     "B",             4,         2,               1,      20,            TRUE,               1,
    9,     "C",             4,         3,               1,      30,            TRUE,               1,
    10,    "C",             1,         3,               1,      30,            TRUE,               1,
    11,    "C",             2,         3,               1,      30,            TRUE,               1,
    12,    "C",             3,         3,               1,      30,            TRUE,               1,
    13,    "B",             1,         2,               1,      20,            TRUE,               2,
    14,    "B",             2,         2,               1,      20,            TRUE,               2,
    15,    "B",             3,         2,               1,      20,            TRUE,               2,
    16,    "B",             4,         2,               1,      20,            TRUE,               2,
    17,    "D",             1,         4,               1,      40,            TRUE,               1,
    18,    "D",             2,         4,               1,      40,            TRUE,               1,
    19,    "D",             3,         4,               1,      40,            TRUE,               1,
    20,    "D",             4,         4,               1,      40,            TRUE,               1
    )
  expected <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~useForCalibration, ~vial_group,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / ----------------- / -----------
    6,     "B",             2,         2,               1,      20,            TRUE,               1,
    7,     "B",             3,         2,               1,      20,            TRUE,               1,
    8,     "B",             4,         2,               1,      20,            TRUE,               1,
    14,    "B",             2,         2,               1,      20,            TRUE,               2,
    15,    "B",             3,         2,               1,      20,            TRUE,               2,
    16,    "B",             4,         2,               1,      20,            TRUE,               2,
    18,    "D",             2,         4,               1,      40,            TRUE,               1,
    19,    "D",             3,         4,               1,      40,            TRUE,               1,
    20,    "D",             4,         4,               1,      40,            TRUE,               1
  )

  actual7 <- getTrainingData(dataset7, config, useBlock = 1)

  expect_equal(actual7, expected)

})
