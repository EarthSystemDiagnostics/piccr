library(testthat)
library(tidyverse)

context("test accumulateMeasurementsForEachSample")


test_that("mean values are correct", {

  dataset1 <- tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~block, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~dExcess,
    # -- / -------------- / -------------- / ----- / -------- / -------------- / ------------ / --------
    1,     "C",             "x",             1,      1,         1,               15,            15,
    2,     "C",             "x",             1,      2,         2,               20,            20,
    3,     "C",             "x",             1,      3,         3,               25,            25,
    4,     "A",             "y",             NA,     1,         4,               0,             4,
    5,     "A",             "y",             NA,     2,         5,               0,             5,
    6,     "B",             "z",             2,      1,         11,              -5.3,          11,
    7,     "B",             "z",             2,      2,         9,               -4.7,          9,
    8,     "C",             "x",             3,      1,         -2,              2,             -2,
    9,     "C",             "x",             3,      2,         -4,              -2,            2
  )
  expected1 <- tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~block, ~delta.O18, ~delta.H2, ~sd.O18, ~sd.H2, ~d.Excess, ~sd.d.Excess,
    # -- / -------------- / -------------- / ----- / --------- / -------- / ------ / ----- / -------  / -----------
    1,     "C",             "x",             1,      2,          20,        1,       5,      20,        5,
    2,     "A",             "y",             NA,     4.5,        0,         0.71,    0,      4.5,       0.71,
    3,     "B",             "z",             2,      10,         -5,        1.41,    0.42,   10,        1.41,
    4,     "C",             "x",             3,      -3,         0,         1.41,    2.83,   0,         2.83
  )
    
  actual <- accumulateMeasurementsForEachSample(list(df1 = dataset1), list(average_over_last_n_inj = "all"))
  actualRounded <- mutate_if(actual$df1, is.numeric, ~ round(., 2))
  
  expect_length(actual, 1)

  expect_equal(actualRounded, expected1)
})

test_that("use only last 2 injections to calculate average", {
  
  dataset1 <- tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~block, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~dExcess,
    # -- / -------------- / -------------- / ----- / -------- / -------------- / ------------ / --------
    1,     "C",             "x",             1,      1,         1000,            1000,          1000,
    2,     "C",             "x",             1,      2,         2,               20,            20,
    3,     "C",             "x",             1,      3,         3,               25,            25,
    4,     "A",             "y",             NA,     1,         -100,            500,           100,
    5,     "A",             "y",             NA,     2,         -100,            500,           100,
    6,     "A",             "y",             NA,     3,         4,               0,             4,
    7,     "A",             "y",             NA,     4,         5,               0,             5,
    8,     "B",             "z",             2,      1,         99,              -100,          70,
    9,     "B",             "z",             2,      1,         11,              -5.3,          11,
    10,     "B",             "z",             2,      2,         9,               -4.7,          9,
    11,     "C",             "x",             3,      1,         -2,              2,             -2,
    12,     "C",             "x",             3,      2,         -4,              -2,            2
  )
  expected1 <- tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~block, ~delta.O18, ~delta.H2, ~sd.O18, ~sd.H2, ~d.Excess, ~sd.d.Excess,
    # -- / -------------- / -------------- / ----- / --------- / -------- / ------ / ----- / -------  / -----------
    1,     "C",             "x",             1,      2.5,        22.5,      0.71,    3.54,   22.5,      3.54,
    2,     "A",             "y",             NA,     4.5,        0,         0.71,    0,      4.5,       0.71,
    3,     "B",             "z",             2,      10,         -5,        1.41,    0.42,   10,        1.41,
    4,     "C",             "x",             3,      -3,         0,         1.41,    2.83,   0,         2.83
  )
  
  actual <- accumulateMeasurementsForEachSample(list(df1 = dataset1), list(average_over_last_n_inj = 2))
  actualRounded <- mutate_if(actual$df1, is.numeric, ~ round(., 2))
  
  expect_length(actual, 1)
  
  expect_equal(actualRounded, expected1)
})