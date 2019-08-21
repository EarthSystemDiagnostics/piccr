library(testthat)
library(tidyverse)

context("Test the memory correction logic")

# -------------- define test data and expected output ------------

# in this dataset: 
# d18O: m1 = 0.5, m2 = 0.75, m3 = 1
# dD: m1 = 0.7, m2 = 0.9, m3 = 0.99
dataset1 <- tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
  # -- / -------------- / -------- / -------------- / ----- / -------------
   1,    "A",           1,         0.5,             1,      -5,
   2,    "A",           2,         0.75,            1,      -5,
   3,    "A",           3,         1,               1,      -5,
   4,    "C",           1,         1.5,             1,      1.3,
   5,    "C",           2,         1.75,            1,      3.1,
   6,    "C",           3,         2,               1,      3.91,
   7,    "B",           1,         2.5,             1,      1.173,
   8,    "B",           2,         2.75,            1,      0.391,
   9,    "B",           3,         3,               1,      0.0391
)
expected1 <- tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
  # -- / -------------- / -------- / -------------- / ----- / -------------
   1,    "A",           1,         NA,               1,      NA,
   2,    "A",           2,         NA,               1,      NA,
   3,    "A",           3,         NA,               1,      NA,
   4,    "C",           1,         1.75,             1,      3.26,
   5,    "C",           2,         1.75,             1,      2.69,
   6,    "C",           3,         1.75,             1,      2.52,
   7,    "B",           1,         2.75,             1,      0.68,
   8,    "B",           2,         2.75,             1,      0.51,
   9,    "B",           3,         2.75,             1,      0.46
)

# in this dataset: 
# d18O: m1 = 0.98, m2 = 0.985, m3 = 0.99, m4 = 0.995, m5 = 0.9999
# dD: ḿ1 = 0.98, m2 = 0.975, m3 = 0.98, m4 = 0.99, m5 = 0.995
dataset2 <- tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
  #--- / -------------- / -------- / -------------- / ----- / -------------
  1,     "Probe1",        1,         10,              NA,     -15,
  2,     "Probe1",        2,         10,              NA,     -15,
  3,     "Probe1",        3,         10,              NA,     -15,
  4,     "A",             1,         1.18,            1,      6.56,
  5,     "A",             2,         1.135,           1,      6.45,
  6,     "A",             3,         1.09,            1,      6.56,
  7,     "A",             4,         1.045,           1,      6.78,
  8,     "A",             5,         1.0009,          1,      6.89,
  9,     "B",             1,         1.980018,        1,      -11.6222,
  10,    "B",             2,         1.985013,        1,      -11.52775,
  11,    "B",             3,         1.990009,        1,      -11.6222,
  12,    "B",             4,         1.995005,        1,      -11.8111,
  13,    "B",             5,         1.9999,          1,      -11.90555,
  14,    "C",             1,         2.979998,        1,      58.56189,
  15,    "C",             2,         2.984999,        1,      58.20236,
  16,    "C",             3,         2.989999,        1,      58.56189,
  17,    "C",             4,         2.995,           1,      59.28094,
  18,    "C",             5,         2.9999,          1,      59.64047,
  19,    "Probe2",        1,         -4.840002,       NA,     -77.20719,
  20,    "Probe2",        2,         -4.880002,       NA,     -77.20719,
  21,    "Probe2",        3,         -4.920001,       NA,     -78.6036,
  22,    "Probe2",        4,         -4.960001,       NA,     -79.3018,
  23,    "A",             1,         0.8808,          2,      5.287928,
  24,    "A",             2,         0.9106,          2,      4.85991,
  25,    "A",             3,         0.9404,          2,      5.287928,
  26,    "B",             1,         1.978808,        2,      -11.65424,
  27,    "B",             2,         1.984106,        2,      -11.5678,
  28,    "B",             3,         1.989404,        2,      -11.65424,
  29,    "Probe3",        1,         19.63979,        NA,     4.666915,
  30,    "Probe3",        2,         19.72984,        NA,     4.583644,
  31,    "Probe3",        3,         19.81989,        NA,     4.666915
)
expected2 <- tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
  #--- / -------------- / -------- / -------------- / ----- / -------------
  1,     "Probe1",        1,         NA,              NA,     NA,
  2,     "Probe1",        2,         NA,              NA,     NA,
  3,     "Probe1",        3,         NA,              NA,     NA,
  4,     "A",             1,         1.04,            1,      6.74,
  5,     "A",             2,         1.04,            1,      6.74,
  6,     "A",             3,         1.04,            1,      6.74,
  7,     "A",             4,         1.05,            1,      6.74,
  8,     "A",             5,         1.05,            1,      6.74,
  9,     "B",             1,         1.99,            1,      -11.78,
  10,    "B",             2,         1.99,            1,      -11.78,
  11,    "B",             3,         1.99,            1,      -11.78,
  12,    "B",             4,         1.99,            1,      -11.78,
  13,    "B",             5,         2.0,             1,      -11.78,
  14,    "C",             1,         3.0,             1,      59.16,
  15,    "C",             2,         3.0,             1,      59.16,
  16,    "C",             3,         3.0,             1,      59.16,
  17,    "C",             4,         2.99,            1,      59.16,
  18,    "C",             5,         2.99,            1,      59.16,
  19,    "Probe2",        1,         -4.96,           NA,     -78.37,
  20,    "Probe2",        2,         -4.96,           NA,     -79.08,
  21,    "Probe2",        3,         -4.96,           NA,     -79.78,
  22,    "Probe2",        4,         -4.96,           NA,     -79.07,
  23,    "A",             1,         0.97,            2,      6.0,
  24,    "A",             2,         0.97,            2,      6.0,
  25,    "A",             3,         0.97,            2,      6.0,
  26,    "B",             1,         2.0,             2,      -11.8,
  27,    "B",             2,         2.0,             2,      -11.8,
  28,    "B",             3,         1.99,            2,      -11.8,
  29,    "Probe3",        1,         19.92,           NA,     4.81,
  30,    "Probe3",        2,         19.91,           NA,     4.81,
  31,    "Probe3",        3,         19.91,           NA,     4.81
)

# -------------- tests ------------------------------

test_that("test memory corrected datasets", {

  actual <- correctForMemoryEffect(list(df1 = dataset1, df2 = dataset2))
  
  expect_length(actual, 2)
  
  df1Rounded <- mutate(actual$df1$datasetMemoryCorrected, 
                       `d(D_H)Mean` = round(`d(D_H)Mean`, 2),
                       `d(18_16)Mean` = round(`d(18_16)Mean`, 2))
  expect_true(all_equal(df1Rounded, expected1))
  
  df2Rounded <- mutate(actual$df2$datasetMemoryCorrected, 
                       `d(D_H)Mean` = round(`d(D_H)Mean`, 2), 
                       `d(18_16)Mean` = round(`d(18_16)Mean`, 2))
  expect_true(all_equal(df2Rounded, expected2))
})

test_that("test memory coefficients", {
  
  memCoeffExpected1 <- tribble(
    ~`Inj Nr`, ~memoryCoeffD18O, ~memoryCoeffDD,
    # ------ / --------------- / --------------
    1,         0.75,             0.76,
    2,         1.0,              1.05,
    3,         1.25,             1.18
  )
  
  memCoeffExpected2 <- tribble(
    ~`Inj Nr`, ~memoryCoeffD18O, ~memoryCoeffDD,
    # ------ / --------------- / --------------
    1,         0.985,             0.992,
    2,         0.990,             0.986,
    3,         0.995,             0.992,
    4,         1.0,               1.002,
    5,         1.005,             1.007
  )
  
  actual <- correctForMemoryEffect(list(df1 = dataset1, df2 = dataset2))
  
  
  expect_true(all_equal(
    round(actual$df1$memoryCoefficients, 2), 
    memCoeffExpected1
  ))
  expect_true(all_equal(
    round(actual$df2$memoryCoefficients, 3), 
    memCoeffExpected2
  ))
})