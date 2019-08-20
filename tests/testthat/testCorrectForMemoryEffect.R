library(testthat)
library(tidyverse)

context("Test the memory correction logic")

# -------------- define test data and expected output ------------

# in this dataset: 
# d18O: m1 = 0.5, m2 = 0.75, m3 = 1
# dD: m1 = 0.7, m2 = 0.9, m3 = 0.99
dataset1 <- tribble(
  ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
  # ------------ / -------- / -------------- / ----- / -------------
    "A",           1,         0.5,             1,      2.5,
    "A",           2,         0.75,            1,      -2.5,
    "A",           3,         1,               1,      -4.75,
    "C",           1,         1.5,             1,      1.375,
    "C",           2,         1.75,            1,      3.125,
    "C",           3,         2,               1,      3.9125,
    "B",           1,         2.5,             1,      2.97375,
    "B",           2,         2.75,            1,      0.99125,
    "B",           3,         3,               1,      0.099125
)
expected1 <- tribble(
  ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
  # ------------ / -------- / -------------- / ----- / -------------
    "A",           1,         NA,               1,      NA,
    "A",           2,         NA,               1,      NA,
    "A",           3,         NA,               1,      NA,
    "C",           1,         2,                1,      4,
    "C",           2,         2,                1,      4,
    "C",           3,         2,                1,      4,
    "B",           1,         3,                1,      0,
    "B",           2,         3,                1,      0,
    "B",           3,         3,                1,      0
)

# in this dataset: 
# d18O: m1 = 0.5, m2 = 1
# dD: ḿ1 = 0.97, m2 = 0.999
dataset2 <- tribble(
  ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
  # ------------ / -------- / -------------- / ----- / -------------
  "Probe1",        1,         10,              NA,     -14.85,
  "Probe1",        2,         10,              NA,     -14.995,
  "A",             1,         5.5,             1,      6.34015,
  "A",             2,         1,               1,      6.978005,
  "B",             1,         1.5,             1,      -11.43066,
  "B",             2,         2,               1,      -11.98102,
  "C",             1,         2.5,             1,      57.84057,
  "C",             2,         3,               1,      59.92802,
  "Probe2",        1,         6.5,             NA,     -75.80216,
  "Probe2",        2,         10,              NA,     -79.86007,
  "A",             1,         15,              2,      4.394198,
  "A",             2,         20,              2,      6.91314,
  "B",             1,         10,              2,      -11.43261,
  "B",             2,         0,               2,      -11.98109,
  "Probe3",        1,         10,              NA,     3.520567
)
expected2 <- tribble(
  ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
  # ------------ / -------- / -------------- / ----- / -------------
  "Probe1",        1,         NA,              NA,     NA,
  "Probe1",        2,         NA,              NA,     NA,
  "A",             1,         1,               1,      
  "A",             2,         1,               1,
  "B",             1,         2,               1,
  "B",             2,         2,               1,
  "C",             1,         3,               1,
  "C",             2,         3,               1,
  "Probe2",        1,         10,              NA,
  "Probe2",        2,         10,              NA,
  "A",             1,         20,              2,
  "A",             2,         20,              2,
  "B",             1,         0,               2,
  "B",             2,         0,               2,
  "Probe3",        1,         20,              NA
)

# -------------- tests ------------------------------

test_that("test correct list of dataframes", {

  actual <- correctForMemoryEffect(list(df1 = dataset1, df2 = dataset2))
  
  expect_length(actual, 2)
  expect_true(all_equal(actual$df1, expected1))
  expect_true(all_equal(actual$df2, expected2))
})