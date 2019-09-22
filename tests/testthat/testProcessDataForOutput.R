library(testthat)
library(tidyverse)

context("test processDataForOutput")

test_that("test quality control output structure", {

  dataset1 <- tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~block, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~dExcess, ~o18_True, ~H2_True, ~useAsControlStandard,
    # -- / -------------- / -------------- / ----- / -------- / -------------- / ------------ / --------/ ---------/ --------/ ---------------------
    1,     "WU",            "w",             1,      1,         0.9,             8.5,           15,       1,         10,       FALSE,
    2,     "WU",            "w",             1,      2,         1,               9,             20,       1,         10,       FALSE,
    3,     "WU",            "w",             1,      3,         1.1,             10.7,          25,       1,         10,       FALSE,
    4,     "C",             "x",             1,      1,         1.9,             19,            15,       2,         20,       FALSE,
    5,     "C",             "x",             1,      2,         2.1,             20.7,          20,       2,         20,       FALSE,
    6,     "C",             "x",             1,      3,         2,               22.1,          25,       2,         20,       FALSE,
    7,     "probe1",        "p",             NA,     1,         4,               49,            4,        NA,        NA,       FALSE,
    8,     "probe1",        "p",             NA,     2,         5,               49,            5,        NA,        NA,       FALSE,
    9,     "QC",            "qq",            2,      1,         10.4,            95,            11,       10,        100,      TRUE,
    10,    "QC",            "qq",            2,      2,         9.8,             102.5,         9,        10,        100,      TRUE,
    11,    "probe2",        "pp",            NA,     1,         6,               60,            4,        NA,        NA,       FALSE,
    12,    "probe2",        "pp",            NA,     2,         7,               71,            5,        NA,        NA,       FALSE,
    13,    "B",             "z",             3,      1,         2.8,             28.5,          11,       3,         30,       FALSE,
    14,    "B",             "z",             3,      2,         3.2,             31,            9,        3,         30,       FALSE,
    15,    "C",             "x",             3,      1,         2.3,             19.1,           -2,       2,         20,      FALSE,
    16,    "C",             "x",             3,      2,         2.45,            22.7,           2,        2,         20,      FALSE
  )
  expected1 <- tribble(
    ~`Identifier 1`, ~block, ~d18OMeasured, ~d18OTrue, ~d18ODeviation, ~dDMeasured, ~dDTrue, ~dDDeviation,
    # -------------/ ------/ -------------/ ---------/ --------------/ -----------/ -------/ ------------
    "WU",            1,      1,             1,         0,              9.4,         10,      0.6,
    "C",             1,      2,             2,         0,              20.6,        20,      -0.6,
    "QC",            2,      10.1,          10,        -0.1,           98.75,       100,     1.25,
    "B",             3,      3,             3,         0,              29.75,       30,      0.25,
    "C",             3,      2.375,         2,         -0.375,         20.9,        20,      -0.9
    )
  expected2 <- list(`Identifier 1` = "QC", d18O = -0.1, dD = 1.25)
  expected3 <- list(d18O = 0.194, dD = 0.836)
  
  accumulatedData <- accumulateMeasurementsForSingleDataset(
    dataset1, list(average_over_last_n_inj = "all"))

  actual <- getQualityControlInfo(dataset1, accumulatedData)

  expect_length(actual, 3)

  expect_true(is.data.frame(actual$deviationsFromTrue))
  ## TODO: expect_equal(actual$deviationsFromTrue, expected1)
  expect_equal(nrow(actual$deviationsFromTrue), 5)
  expect_equal(ncol(actual$deviationsFromTrue), 8)

  expect_true(is.list(actual$deviationOfControlStandard))
  expect_equal(actual$deviationOfControlStandard, expected2)

  expect_true(is.list(actual$rmsdDeviationsFromTrue))
  expect_equal(lapply(actual$rmsdDeviationsFromTrue, round, 3), expected3)

  actual <- processDataForOutput(list(df1 = dataset1),
                                 list(average_over_last_n_inj = "all"))
  expect_length(actual, 1)
  expect_length(actual$df1, 4)
  
})
