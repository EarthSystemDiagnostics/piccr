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
   7,    "B",           1,         2.75,             1,      0.66,
   8,    "B",           2,         2.75,             1,      0.51,
   9,    "B",           3,         2.75,             1,      0.47
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
  23,    "A",             1,         0.97,            2,      6.01,
  24,    "A",             2,         0.97,            2,      6.01,
  25,    "A",             3,         0.97,            2,      6.01,
  26,    "B",             1,         1.99,             2,      -11.81,
  27,    "B",             2,         1.99,             2,      -11.81,
  28,    "B",             3,         1.99,            2,      -11.81,
  29,    "Probe3",        1,         19.91,           NA,     4.81,
  30,    "Probe3",        2,         19.91,           NA,     4.81,
  31,    "Probe3",        3,         19.91,           NA,     4.81
)
# in this dataset: 
# d18O: m1 = 0.5, m2 = 0.75, m3 = 0.875, m4 = 1
# dD: m1 = 0.4, m2 = 0.6, m3 = 0.8, m4 = 1
dataset3 <- tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
  # -- / -------------- / -------- / -------------- / ----- / -------------
   1,    "A",           1,         1,               1,      -5,
   2,    "A",           2,         1,               1,      -5,
   3,    "A",           1,         1,               1,      -5,
   4,    "A",           2,         1,               1,      -5,
   5,    "C",           1,         1.5,             1,      -4.2,
   6,    "C",           2,         1.75,            1,      -3.8,
   7,    "C",           1,         1.875,           1,      -3.4,
   8,    "C",           2,         2.0,             1,      -3.0,
   9,    "B",           1,         2.5,             1,      -2.2,
   10,   "B",           2,         2.75,            1,      -1.8,
   11,   "B",           1,         2.875,           1,      -1.4,
   12,   "B",           2,         3,               1,      -1
)

# -------------- tests ------------------------------

test_that("test memory corrected datasets", {

  actual1 <- correctForMemoryEffect(dataset1)
  actual2 <- correctForMemoryEffect(dataset2)

  df1Rounded <- mutate(actual1$datasetMemoryCorrected,
                       `d(D_H)Mean` = round(`d(D_H)Mean`, 2),
                       `d(18_16)Mean` = round(`d(18_16)Mean`, 2))
  expect_equal(df1Rounded, expected1)

  df2Rounded <- mutate(actual2$datasetMemoryCorrected,
                       `d(D_H)Mean` = round(`d(D_H)Mean`, 2),
                       `d(18_16)Mean` = round(`d(18_16)Mean`, 2))
  expect_equal(df2Rounded, expected2)
})

test_that("test memory coefficients", {

  memCoeffExpected1 <- tribble(
    ~`Inj Nr`, ~memoryCoeffD18O, ~memoryCoeffDD, ~AD18O,   ~ADD,       ~BD18O, ~BDD,  ~CD18O, ~CDD,
    # ------ / --------------- / -------------| -------- | --------- | ----- | ---- | ------ | ----
    1,         0.75,             0.76,          NA_real_,  NA_real_,  0.75,   0.71,   0.75,  0.81,
    2,         1.0,              1.05,          NA_real_,  NA_real_,  1,      1.06,   1,     1.04,
    3,         1.25,             1.18,          NA_real_,  NA_real_,  1.25,   1.22,   1.25,  1.15,
  )

  memCoeffExpected2 <- tribble(
    ~`Inj Nr`, ~memoryCoeffD18O, ~memoryCoeffDD, ~AD18O,   ~ADD,      ~BD18O, ~BDD,  ~CD18O,  ~CDD,
    # ------ / --------------- / ------------- | -------- | ------- | ----- | ----- | ----- | ----
    1,         0.985,             0.992,         NA_real_, NA_real_,  0.984,  0.992,  0.985,  0.992,
    2,         0.990,             0.986,         NA_real_, NA_real_,  0.990,  0.986,  0.990,  0.986,
    3,         0.995,             0.992,         NA_real_, NA_real_,  0.995,  0.992,  0.995,  0.992,
    4,         1.0,               1.002,         NA_real_, NA_real_,  1.000,  1.002,  1.000,  1.002,
    5,         1.005,             1.007,         NA_real_, NA_real_,  1.005,  1.007,  1.005,  1.007
  )

  actual1 <- correctForMemoryEffect(dataset1)
  actual2 <- correctForMemoryEffect(dataset2)
  
  expect_equal(
    round(actual1$memoryCoefficients, 2),
    memCoeffExpected1
  )
  expect_equal(
    round(actual2$memoryCoefficients, 3),
    memCoeffExpected2
  )
})

test_that("test that NA values don't spread in applyCalibration", {
  
  dataset1 <- tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
    # -- / -------------- / -------- / -------------- / ----- / -------------
    1,    "A",           1,         0.5,                1,      5,
    2,    "A",           2,         0.75,               1,      5,
    3,    "A",           3,         1,                  1,      5,
    4,    "B",           1,         2.5,                1,      1.173,
    5,    "B",           2,         2.75,               1,      0.391,
    6,    "B",           3,         3,                  1,      0.0391,
    7,    "C",           1,         NA,                 NA,      NA,
    8,    "C",           2,         NA,                 NA,      NA,
    9,    "C",           3,         NA,                 NA,      NA,
    10,   "B",           1,         2.5,                2,       NA,
    11,   "B",           2,         2.75,               2,       NA,
    12,   "B",           3,         3,                  2,       NA
  )
  dataset2 <- tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
    #--- / -------------- / -------- / -------------- / ----- / -------------
    4,     "A",             1,         1.18,            1,      6.56,
    5,     "A",             2,         NA,              1,      6.45,
    6,     "A",             3,         1.09,            1,      6.56,
    7,     "A",             4,         1.045,           1,      6.78,
    8,     "A",             5,         1.0009,          1,      6.89,
    9,     "B",             1,         1.980018,        1,      -11.6222,
    10,    "B",             2,         1.985013,        1,      -11.52775,
    11,    "B",             3,         1.990009,        1,      -11.6222,
    12,    "B",             4,         1.995005,        1,      NA,
    13,    "B",             5,         1.9999,          1,      -11.90555,
    14,    "C",             1,         2.979998,        1,      58.56189,
    15,    "C",             2,         2.984999,        1,      58.20236,
    16,    "C",             3,         2.989999,        1,      58.56189,
    17,    "C",             4,         NA,              1,      59.28094,
    18,    "C",             5,         2.9999,          1,      59.64047,
    19,    "Probe2",        1,         -4.840002,       NA,     -77.20719,
    20,    "Probe2",        2,         -4.880002,       NA,     -77.20719,
    21,    "Probe2",        3,         -4.920001,       NA,     -78.6036,
    22,    "Probe2",        4,         -4.960001,       NA,     -79.3018,
    23,    "A",             1,         NA,              2,      NA,
    24,    "A",             2,         NA,              2,      NA,
    25,    "A",             3,         NA,              2,      NA,
    26,    "B",             1,         1.978808,        2,      NA,
    27,    "B",             2,         1.984106,        2,      NA,
    28,    "B",             3,         1.989404,        2,      NA,
    29,    "Probe3",        1,         19.63979,        NA,     4.666915,
    30,    "Probe3",        2,         19.72984,        NA,     4.583644,
    31,    "Probe3",        3,         19.81989,        NA,     4.666915
  )
  
  actual1 <- correctForMemoryEffect(dataset1)
  actual2 <- correctForMemoryEffect(dataset2)
  
  actual1 <- actual1$datasetMemoryCorrected
  actual2 <- actual2$datasetMemoryCorrected
  
  expect_equal(sum(is.na(select(actual1, `d(18_16)Mean`))), 6)
  expect_equal(sum(is.na(select(actual1, `d(D_H)Mean`))), 9)
  expect_equal(sum(is.na(select(actual2, `d(18_16)Mean`))), 9)
  expect_equal(sum(is.na(select(actual2, `d(D_H)Mean`))), 12)
})

test_that("test injection range of mean memory coefficients", {

  actual <- calculateMemoryCoefficients(dataset1)
  expect_length(actual$`Inj Nr`, 3)

  dataset3 <- normalizeInjectionNumbers(dataset3)
  actual <- calculateMemoryCoefficients(dataset3)
  expect_length(actual$`Inj Nr`, 4)

})

test_that("different numbers of injections for the block 1 standards does not cause error", {
  
  dataset4 <- tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
    # -- / -------------- / -------- / -------------- / ----- / -------------
    1,    "A",           1,         0.5,             1,      -5,
    2,    "A",           2,         0.75,            1,      -5,
    3,    "A",           3,         1,               1,      -5,
    4,    "A",           4,         1,               1,      -5,
    5,    "C",           1,         1.5,             1,      1.3,
    6,    "C",           2,         1.75,            1,      3.1,
    7,    "C",           3,         2,               1,      3.91,
    8,    "B",           1,         2.5,             1,      1.173,
    9,    "B",           2,         2.75,            1,      0.391,
    10,   "B",           3,         3,               1,      0.0391
  )
  
  actual <- correctForMemoryEffect(dataset4)
  
  expect_is(actual, "list")
})

# in this dataset:
# d18O: m.tilde = 0.082085
# dD: m.tilde = 0.2231302
dataset5 <- tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`,
  #--- / -------------- / -------- / -------------- / ----- / -------------
  1,     "WU",            1,         -9.179,           1,    -62.145,
  2,     "WU",            2,         -9.933,           1,    -76.017,
  3,     "WU",            3,         -9.994,           1,    -79.111,
  4,     "WU",            4,         -9.999,           1,    -79.802,
  5,     "WU",            5,         -10,              1,    -79.956,
  6,     "WU",            6,         -10,              1,    -79.990,
  7,     "WU",            7,         -10,              1,    -79.998,
  8,     "WU",            8,         -10,              1,    -79.999,
  9,     "WU",            9,         -10,              1,    -80,
  10,    "WU",            10,        -10,              1,    -80,
  11,    "Std1",          1,         -28.358,          1,    -204.299,
  12,    "Std1",          2,         -29.865,          1,    -232.034,
  13,    "Std1",          3,         -29.989,          1,    -238.223,
  14,    "Std1",          4,         -29.999,          1,    -239.603,
  15,    "Std1",          5,         -30,              1,    -239.912,
  16,    "Std1",          6,         -30,              1,    -239.980,
  17,    "Std2",          1,         -39.179,          1,    -302.150,
  18,    "Std2",          2,         -39.933,          1,    -316.017,
  19,    "Std2",          3,         -39.994,          1,    -319.111,
  20,    "Std2",          4,         -39.999,          1,    -319.802,
  21,    "Std2",          5,         -40,              1,    -319.956,
  22,    "Std2",          6,         -40,              1,    -319.990,
  23,    "A",             1,         -35.410,          NA,   -288.923,
  24,    "A",             2,         -35.034,          NA,   -281.992,
  25,    "A",             3,         -35.003,          NA,   -280.444,
  26,    "A",             4,         -35.,             NA,   -280.099,
  27,    "A",             5,         -35.,             NA,   -280.022,
  28,    "A",             6,         -35.,             NA,   -280.005
)

test_that("test that no NA mean memory coefficients are kept", {

  actual <- calculateMemoryCoefficients(dataset5)

  expect_equal(nrow(actual), 6)
  expect_equal(sum(is.na(c(actual$memoryCoeffD18O, actual$memoryCoeffDD))), 0)

})

test_that("test that number of memory coefficients fits sample data", {

  dataset6 <- add_row(dataset5,
                      Line = 29, `Identifier 1` = "A", `Inj Nr` = 7,
                      `d(18_16)Mean` = -35., `d(D_H)Mean` = -280.001,
                      block = NA)

  actualCoeff <- calculateMemoryCoefficients(dataset6)
  sampleData  <- filter(dataset6, is.na(dataset6$block))

  expect_equal(max(sampleData$`Inj Nr`), max(actualCoeff$`Inj Nr`))

  actual <- correctForMemoryEffect(dataset6)$datasetMemoryCorrected
  actual <- filter(actual, `Identifier 1` == "A")

  expect_equal(sum(is.na(c(actual$`d(18_16)Mean`, actual$`d(D_H)Mean`))), 0)
  
})
