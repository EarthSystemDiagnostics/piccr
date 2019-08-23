library(testthat)
library(tidyverse)

context("test addColumnDExcess (d_excess = dH - 8 * d18O)")

test_that("test addColumnDExcess", {
  
  dataset1 <- tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol,
    # ------------ / ------------ / -------- /
    1,               4,             10,
    2,               3,             10,
    3,               2,             10,
    -4,              1,             10,
    5,               0,             10
  )
  expected1 <- tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol, ~dExcess,
    # ------- / --------- / -------- / --------
    1,         4,             10,        -4,
    2,         3,             10,        -13,
    3,         2,             10,        -22,
    -4,        1,             10,        33,
    5,         0,             10,        -40
  ) 
  dataset2 <- tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol,
    # ------- / --------- / -------
    1,          8,             10,
    2,          16,            10,
    3,          32,            10,
    -5,         100,           10,
    -20,        0,             10
  )
  expected2 <- tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol, ~dExcess,
    # ------- / -------- / -------- / --------
    1,         8,             10,        0,
    2,         16,            10,        0,
    3,         32,            10,        8,
    -5,        100,           10,        140,
    -20,       0,             10,        160
  )
  
  actual <- addColumnDExcess(list(df1 = dataset1, df2 = dataset2))
  
  expect_length(actual, 2)
  expect_equal(actual$df1, expected1)
  expect_equal(actual$df2, expected2)
})