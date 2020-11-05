library(tibble)
library(dplyr)

context("test processing data for output")

test_that("test quality control output structure", {

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
  expect_equal(dplyr::mutate_if(actual2$deviationsFromTrue, is.numeric, round, digits = 5), expected1)
  expect_equal(actual2$deviationOfControlStandard, expected2)
  expect_equal(lapply(actual2$rmsdDeviationsFromTrue, round, 3), expected3)
  expect_equal(lapply(actual2$pooledSD, round, 3), expected4)
})

test_that("test correct vial count of very first standard in file", {

  dataset1 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~`Inj Nr`, ~Sample, ~vial_group,
    # ---/ ---------------/ ---------------/ ---------/ -------/ -----------/
    1,     "A",             "x",             1,         1,       1,
    2,     "A",             "x",             2,         1,       1,
    3,     "A",             "x",             3,         1,       1,
    4,     "B",             "y",             1,         2,       1,
    5,     "B",             "y",             2,         2,       1,
    6,     "B",             "y",             3,         2,       1,
    7,     "C",             "z",             1,         3,       1,
    8,     "C",             "z",             2,         3,       1,
    9,     "C",             "z",             3,         3,       1
  )
  dataset2 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~`Inj Nr`, ~Sample, ~vial_group,
    # ---/ ---------------/ ---------------/ ---------/ -------/ -----------/
    1,     "A",             "x",             1,         1,       1,
    2,     "A",             "x",             2,         1,       1,
    3,     "A",             "x",             3,         1,       1,
    4,     "A",             "x",             1,         2,       1,
    5,     "A",             "x",             2,         2,       1,
    6,     "A",             "x",             3,         2,       1,
    7,     "B",             "z",             1,         3,       1,
    8,     "B",             "z",             2,         3,       1,
    9,     "B",             "z",             3,         3,       1,
    10,    "A",             "x",             1,         4,       2,
    11,    "A",             "x",             2,         4,       2,
    12,    "A",             "x",             3,         4,       2,
    13,    "C",             "z",             1,         5,       1,
    14,    "C",             "z",             2,         5,       1,
    15,    "C",             "z",             3,         5,       1,
    16,    "C",             "z",             1,         6,       2,
    17,    "C",             "z",             2,         6,       2,
    18,    "C",             "z",             3,         6,       2,
    19,    "D",             "f",             1,         7,       1,
    20,    "D",             "f",             2,         7,       1,
    21,    "D",             "f",             3,         7,       1
  )

  actual1 <- getVialCountOfFirstStd(dataset1)
  actual2 <- getVialCountOfFirstStd(dataset2)

  expect_equal(actual1, 1)
  expect_equal(actual2, 2)

})
