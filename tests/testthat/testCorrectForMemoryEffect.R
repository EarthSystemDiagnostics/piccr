library(tibble)
library(dplyr)

context("Test the memory correction logic")

# -------------- define test data and expected output ------------

# in this dataset:
# d18O: m1 = 0.5, m2 = 0.75, m3 = 1
# dD: m1 = 0.7, m2 = 0.9, m3 = 0.99
dataset1 <- tibble::tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~vial_group,
  # -- / -------------- / -------- / -------------- / ----- / ------------ / -----------
   1,    "A",             1,         0.5,             1,      -5,            1,
   2,    "A",             2,         0.75,            1,      -5,            1,
   3,    "A",             3,         1,               1,      -5,            1,
   4,    "C",             1,         1.5,             1,      1.3,           1,
   5,    "C",             2,         1.75,            1,      3.1,           1,
   6,    "C",             3,         2,               1,      3.91,          1,
   7,    "B",             1,         2.5,             1,      1.173,         1,
   8,    "B",             2,         2.75,            1,      0.391,         1,
   9,    "B",             3,         3,               1,      0.0391,        1
)
expected1 <- tibble::tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~vial_group,
  # -- / -------------- / -------- / -------------- / ----- / ------------ / -----------
   1,    "A",           1,         NA,               1,      NA,             1,
   2,    "A",           2,         NA,               1,      NA,             1,
   3,    "A",           3,         NA,               1,      NA,             1,
   4,    "C",           1,         1.75,             1,      3.26,           1,
   5,    "C",           2,         1.75,             1,      2.69,           1,
   6,    "C",           3,         1.75,             1,      2.52,           1,
   7,    "B",           1,         2.75,             1,      0.66,           1,
   8,    "B",           2,         2.75,             1,      0.51,           1,
   9,    "B",           3,         2.75,             1,      0.47,           1
)

# in this dataset:
# d18O: m1 = 0.98, m2 = 0.985, m3 = 0.99, m4 = 0.995, m5 = 0.9999
# dD: ḿ1 = 0.98, m2 = 0.975, m3 = 0.98, m4 = 0.99, m5 = 0.995
dataset2 <- tibble::tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~vial_group,
  #--- / -------------- / -------- / -------------- / ----- / ------------ / -----------
  1,     "Probe1",        1,         10,              NA,     -15,           1,
  2,     "Probe1",        2,         10,              NA,     -15,           1,
  3,     "Probe1",        3,         10,              NA,     -15,           1,
  4,     "A",             1,         1.18,            1,      6.56,          1,
  5,     "A",             2,         1.135,           1,      6.45,          1,
  6,     "A",             3,         1.09,            1,      6.56,          1,
  7,     "A",             4,         1.045,           1,      6.78,          1,
  8,     "A",             5,         1.0009,          1,      6.89,          1,
  9,     "B",             1,         1.980018,        1,      -11.6222,      1,
  10,    "B",             2,         1.985013,        1,      -11.52775,     1,
  11,    "B",             3,         1.990009,        1,      -11.6222,      1,
  12,    "B",             4,         1.995005,        1,      -11.8111,      1,
  13,    "B",             5,         1.9999,          1,      -11.90555,     1,
  14,    "C",             1,         2.979998,        1,      58.56189,      1,
  15,    "C",             2,         2.984999,        1,      58.20236,      1,
  16,    "C",             3,         2.989999,        1,      58.56189,      1,
  17,    "C",             4,         2.995,           1,      59.28094,      1,
  18,    "C",             5,         2.9999,          1,      59.64047,      1,
  19,    "Probe2",        1,         -4.840002,       NA,     -77.20719,     1,
  20,    "Probe2",        2,         -4.880002,       NA,     -77.20719,     1,
  21,    "Probe2",        3,         -4.920001,       NA,     -78.6036,      1,
  22,    "Probe2",        4,         -4.960001,       NA,     -79.3018,      1,
  23,    "A",             1,         0.8808,          2,      5.287928,      1,
  24,    "A",             2,         0.9106,          2,      4.85991,       1,
  25,    "A",             3,         0.9404,          2,      5.287928,      1,
  26,    "B",             1,         1.978808,        2,      -11.65424,     1,
  27,    "B",             2,         1.984106,        2,      -11.5678,      1,
  28,    "B",             3,         1.989404,        2,      -11.65424,     1,
  29,    "Probe3",        1,         19.63979,        NA,     4.666915,      1,
  30,    "Probe3",        2,         19.72984,        NA,     4.583644,      1,
  31,    "Probe3",        3,         19.81989,        NA,     4.666915,      1
)
expected2 <- tibble::tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~vial_group,
  #--- / -------------- / -------- / -------------- / ----- / ------------ / -----------
  1,     "Probe1",        1,         NA,              NA,     NA,            1,
  2,     "Probe1",        2,         NA,              NA,     NA,            1,
  3,     "Probe1",        3,         NA,              NA,     NA,            1,
  4,     "A",             1,         1.04,            1,      6.74,          1,
  5,     "A",             2,         1.04,            1,      6.74,          1,
  6,     "A",             3,         1.04,            1,      6.74,          1,
  7,     "A",             4,         1.05,            1,      6.74,          1,
  8,     "A",             5,         1.05,            1,      6.74,          1,
  9,     "B",             1,         1.99,            1,      -11.78,        1,
  10,    "B",             2,         1.99,            1,      -11.78,        1,
  11,    "B",             3,         1.99,            1,      -11.78,        1,
  12,    "B",             4,         1.99,            1,      -11.78,        1,
  13,    "B",             5,         2.0,             1,      -11.78,        1,
  14,    "C",             1,         3.0,             1,      59.16,         1,
  15,    "C",             2,         3.0,             1,      59.16,         1,
  16,    "C",             3,         3.0,             1,      59.16,         1,
  17,    "C",             4,         2.99,            1,      59.16,         1,
  18,    "C",             5,         2.99,            1,      59.16,         1,
  19,    "Probe2",        1,         -4.96,           NA,     -78.37,        1,
  20,    "Probe2",        2,         -4.96,           NA,     -79.08,        1,
  21,    "Probe2",        3,         -4.96,           NA,     -79.78,        1,
  22,    "Probe2",        4,         -4.96,           NA,     -79.07,        1,
  23,    "A",             1,         0.97,            2,      6.01,          1,
  24,    "A",             2,         0.97,            2,      6.01,          1,
  25,    "A",             3,         0.97,            2,      6.01,          1,
  26,    "B",             1,         1.99,             2,      -11.81,       1,
  27,    "B",             2,         1.99,             2,      -11.81,       1,
  28,    "B",             3,         1.99,            2,      -11.81,        1,
  29,    "Probe3",        1,         19.91,           NA,     4.81,          1,
  30,    "Probe3",        2,         19.91,           NA,     4.81,          1,
  31,    "Probe3",        3,         19.91,           NA,     4.81,          1
)
# in this dataset: 
# d18O: m1 = 0.5, m2 = 0.75, m3 = 0.875, m4 = 1
# dD: m1 = 0.4, m2 = 0.6, m3 = 0.8, m4 = 1
dataset3 <- tibble::tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~`vial_group`,
  # -- / -------------- / -------- / -------------- / ----- / ------------ / --------------
   1,    "A",             1,         1,               1,      -5,            1,
   2,    "A",             2,         1,               1,      -5,            1,
   3,    "A",             1,         1,               1,      -5,            1,
   4,    "A",             2,         1,               1,      -5,            1,
   5,    "C",             1,         1.5,             1,      -4.2,          1,
   6,    "C",             2,         1.75,            1,      -3.8,          1,
   7,    "C",             1,         1.875,           1,      -3.4,          1,
   8,    "C",             2,         2.0,             1,      -3.0,          1,
   9,    "B",             1,         2.5,             1,      -2.2,          1,
   10,   "B",             2,         2.75,            1,      -1.8,          1,
   11,   "B",             1,         2.875,           1,      -1.4,          1,
   12,   "B",             2,         3,               1,      -1,            1
)
# in this dataset:
# d18O: m1 = 0.5, m2 = 0.75, m3 = 0.875, m4 = 1
# dD: m1 = 0.4, m2 = 0.6, m3 = 0.8, m4 = 1
dataset4 <- tibble::tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~`vial_group`,
  # -- / -------------- / -------- / -------------- / ----- / ------------ / --------------
  1,     "A",             1,         1,               1,      -5,            1,
  2,     "A",             2,         1,               1,      -5,            1,
  3,     "A",             3,         1,               1,      -5,            1,
  4,     "A",             4,         1,               1,      -5,            1,
  5,     "C",             1,         1.5,             1,      -4.2,          1,
  6,     "C",             2,         1.75,            1,      -3.8,          1,
  7,     "C",             3,         1.875,           1,      -3.4,          1,
  8,     "C",             4,         2.0,             1,      -3.0,          1,
  9,     "A",             1,         1.5,             1,      -3.8,          2,
  10,    "A",             2,         1.25,            1,      -4.2,          2,
  11,    "A",             3,         1.125,           1,      -4.6,          2,
  12,    "A",             4,         1,               1,      -5,            2,
  13,    "B",             1,         2,               1,      -3.4,          1,
  14,    "B",             2,         2.5,             1,      -2.6,          1,
  15,    "B",             3,         2.75,            1,      -1.8,          1,
  16,    "B",             4,         3,               1,      -1,            1
)

# -------------- tests ------------------------------

test_that("test memory corrected datasets", {

  actual1 <- correctForMemoryEffect(dataset1)
  actual2 <- correctForMemoryEffect(dataset2)

  df1Rounded <- dplyr::mutate(actual1$datasetMemoryCorrected,
                              `d(D_H)Mean` = round(`d(D_H)Mean`, 2),
                              `d(18_16)Mean` = round(`d(18_16)Mean`, 2))
  expect_equal(df1Rounded, expected1)

  df2Rounded <- dplyr::mutate(actual2$datasetMemoryCorrected,
                              `d(D_H)Mean` = round(`d(D_H)Mean`, 2),
                              `d(18_16)Mean` = round(`d(18_16)Mean`, 2))
  expect_equal(df2Rounded, expected2)
})

test_that("test memory coefficients", {

  memCoeffExpected1 <- tibble::tribble(
    ~`Inj Nr`, ~A_vial1_d18O, ~A_vial1_dD, ~B_vial1_d18O, ~B_vial1_dD, ~C_vial1_d18O, ~C_vial1_dD, ~memoryCoeffD18O, ~memoryCoeffDD, ~sdMemoryCoeffD18O, ~sdMemoryCoeffDD,
    # ------ / ------------ / ---------- / ------------ / ---------- / ------------ / ---------- / --------------- / ------------- / ----------------- / ----------------
    1,         NA_real_,      NA_real_,    0.75,          0.71,        0.75,          0.81,        0.75,             0.76,           0,                  0.07,
    2,         NA_real_,      NA_real_,    1,             1.06,        1,             1.04,        1.0,              1.05,           0,                  0.02,
    3,         NA_real_,      NA_real_,    1.25,          1.22,        1.25,          1.15,        1.25,             1.18,           0,                  0.05
  )

  memCoeffExpected2 <- tibble::tribble(
    ~`Inj Nr`, ~A_vial1_d18O, ~A_vial1_dD, ~B_vial1_d18O, ~B_vial1_dD, ~C_vial1_d18O, ~C_vial1_dD, ~memoryCoeffD18O, ~memoryCoeffDD, ~sdMemoryCoeffD18O, ~sdMemoryCoeffDD,
    # ------ / ------------ / ---------- / ------------ / ---------- / ------------ / ---------- / --------------- / ------------- / ----------------- / ----------------
    1,         NA_real_,      NA_real_,    0.984,        0.992,        0.985,         0.992,       0.985,            0.992,          0.001,              0,
    2,         NA_real_,      NA_real_,    0.990,        0.986,        0.990,         0.986,       0.990,            0.986,          0,                  0,
    3,         NA_real_,      NA_real_,    0.995,        0.992,        0.995,         0.992,       0.995,            0.992,          0,                  0,
    4,         NA_real_,      NA_real_,    1.000,        1.002,        1.000,         1.002,       1.0,              1.002,          0,                  0,
    5,         NA_real_,      NA_real_,    1.005,        1.007,        1.005,         1.007,       1.005,            1.007,          0,                  0,
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
  
  dataset1 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~vial_group,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / -----------
    1,    "A",              1,         0.5,                1,      5,          1,
    2,    "A",              2,         0.75,               1,      5,          1,
    3,    "A",              3,         1,                  1,      5,          1,
    4,    "B",              1,         2.5,                1,      1.173,      1,
    5,    "B",              2,         2.75,               1,      0.391,      1,
    6,    "B",              3,         3,                  1,      0.0391,     1,
    7,    "C",              1,         NA,                 NA,      NA,        1,
    8,    "C",              2,         NA,                 NA,      NA,        1,
    9,    "C",              3,         NA,                 NA,      NA,        1,
    10,   "B",              1,         2.5,                2,       NA,        1,
    11,   "B",              2,         2.75,               2,       NA,        1,
    12,   "B",              3,         3,                  2,       NA,        1
  )
  dataset2 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~vial_group,
    #--- / -------------- / -------- / -------------- / ----- / ------------ / -----------
    4,     "A",             1,         1.18,            1,      6.56,          1,
    5,     "A",             2,         NA,              1,      6.45,          1,
    6,     "A",             3,         1.09,            1,      6.56,          1,
    7,     "A",             4,         1.045,           1,      6.78,          1,
    8,     "A",             5,         1.0009,          1,      6.89,          1,
    9,     "B",             1,         1.980018,        1,      -11.6222,      1,
    10,    "B",             2,         1.985013,        1,      -11.52775,     1,
    11,    "B",             3,         1.990009,        1,      -11.6222,      1,
    12,    "B",             4,         1.995005,        1,      NA,            1,
    13,    "B",             5,         1.9999,          1,      -11.90555,     1,
    14,    "C",             1,         2.979998,        1,      58.56189,      1,
    15,    "C",             2,         2.984999,        1,      58.20236,      1,
    16,    "C",             3,         2.989999,        1,      58.56189,      1,
    17,    "C",             4,         NA,              1,      59.28094,      1,
    18,    "C",             5,         2.9999,          1,      59.64047,      1,
    19,    "Probe2",        1,         -4.840002,       NA,     -77.20719,     1,
    20,    "Probe2",        2,         -4.880002,       NA,     -77.20719,     1,
    21,    "Probe2",        3,         -4.920001,       NA,     -78.6036,      1,
    22,    "Probe2",        4,         -4.960001,       NA,     -79.3018,      1,
    23,    "A",             1,         NA,              2,      NA,            1,
    24,    "A",             2,         NA,              2,      NA,            1,
    25,    "A",             3,         NA,              2,      NA,            1,
    26,    "B",             1,         1.978808,        2,      NA,            1,
    27,    "B",             2,         1.984106,        2,      NA,            1,
    28,    "B",             3,         1.989404,        2,      NA,            1,
    29,    "Probe3",        1,         19.63979,        NA,     4.666915,      1,
    30,    "Probe3",        2,         19.72984,        NA,     4.583644,      1,
    31,    "Probe3",        3,         19.81989,        NA,     4.666915,      1
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
  
  dataset4 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~vial_group,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / -----------
    1,    "A",              1,         0.5,             1,      -5,            1,
    2,    "A",              2,         0.75,            1,      -5,            1,
    3,    "A",              3,         1,               1,      -5,            1,
    4,    "A",              4,         1,               1,      -5,            1,
    5,    "C",              1,         1.5,             1,      1.3,           1,
    6,    "C",              2,         1.75,            1,      3.1,           1,
    7,    "C",              3,         2,               1,      3.91,          1,
    8,    "B",              1,         2.5,             1,      1.173,         1,
    9,    "B",              2,         2.75,            1,      0.391,         1,
    10,   "B",              3,         3,               1,      0.0391,        1
  )
  
  actual <- correctForMemoryEffect(dataset4)
  
  expect_is(actual, "list")
})

# in these datasets:
# d18O: m.tilde = 0.082085
# dD: m.tilde = 0.2231302
dataset5 <- tibble::tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~vial_group,
  #--- / -------------- / -------- / -------------- / ----- / ------------ / -----------
  1,     "WU",            1,         -9.179,           1,    -62.145,        1,
  2,     "WU",            2,         -9.933,           1,    -76.017,        1,
  3,     "WU",            3,         -9.994,           1,    -79.111,        1,
  4,     "WU",            4,         -9.999,           1,    -79.802,        1,
  5,     "WU",            5,         -10,              1,    -79.956,        1,
  6,     "WU",            6,         -10,              1,    -79.990,        1,
  7,     "WU",            7,         -10,              1,    -79.998,        1,
  8,     "WU",            8,         -10,              1,    -79.999,        1,
  9,     "WU",            9,         -10,              1,    -80,            1,
  10,    "WU",            10,        -10,              1,    -80,            1,
  11,    "Std1",          1,         -28.358,          1,    -204.299,       1,
  12,    "Std1",          2,         -29.865,          1,    -232.034,       1,
  13,    "Std1",          3,         -29.989,          1,    -238.223,       1,
  14,    "Std1",          4,         -29.999,          1,    -239.603,       1,
  15,    "Std1",          5,         -30,              1,    -239.912,       1,
  16,    "Std1",          6,         -30,              1,    -239.980,       1,
  17,    "Std2",          1,         -39.179,          1,    -302.150,       1,
  18,    "Std2",          2,         -39.933,          1,    -316.017,       1,
  19,    "Std2",          3,         -39.994,          1,    -319.111,       1,
  20,    "Std2",          4,         -39.999,          1,    -319.802,       1,
  21,    "Std2",          5,         -40,              1,    -319.956,       1,
  22,    "Std2",          6,         -40,              1,    -319.990,       1,
  23,    "A",             1,         -35.410,          NA,   -288.923,       1,
  24,    "A",             2,         -35.034,          NA,   -281.992,       1,
  25,    "A",             3,         -35.003,          NA,   -280.444,       1,
  26,    "A",             4,         -35.,             NA,   -280.099,       1,
  27,    "A",             5,         -35.,             NA,   -280.022,       1,
  28,    "A",             6,         -35.,             NA,   -280.005,       1
)
dataset6 <- tibble::tribble(
  ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~vial_group,
  #--- / -------------- / -------- / -------------- / ----- / ------------ / -----------
  1,     "WU",            1,         -9.179,           1,    -62.145,        1,
  2,     "WU",            2,         -9.933,           1,    -76.017,        1,
  3,     "WU",            3,         -9.994,           1,    -79.111,        1,
  4,     "WU",            4,         -9.999,           1,    -79.802,        1,
  5,     "WU",            5,         -10,              1,    -79.956,        1,
  6,     "WU",            6,         -10,              1,    -79.990,        1,
  7,     "WU",            7,         -10,              1,    -79.998,        1,
  8,     "WU",            8,         -10,              1,    -79.999,        1,
  9,     "WU",            9,         -10,              1,    -80,            1,
  10,    "WU",            10,        -10,              1,    -80,            1,
  11,    "Std1",          1,         -28.358,          1,    -204.299,       1,
  12,    "Std1",          2,         -29.865,          1,    -232.034,       1,
  13,    "Std1",          3,         -29.989,          1,    -238.223,       1,
  14,    "Std1",          4,         -29.999,          1,    -239.603,       1,
  15,    "Std1",          5,         -30,              1,    -239.912,       1,
  16,    "Std1",          6,         -30,              1,    -239.980,       1,
  17,    "Std2",          1,         -39.179,          1,    -302.150,       1,
  18,    "Std2",          2,         -39.933,          1,    -316.017,       1,
  19,    "Std2",          3,         -39.994,          1,    -319.111,       1,
  20,    "Std2",          4,         -39.999,          1,    -319.802,       1,
  21,    "Std2",          5,         -40,              1,    -319.956,       1,
  22,    "Std2",          6,         -40,              1,    -319.990,       1,
  23,    "A",             1,         -35.410,          NA,   -288.923,       1,
  24,    "A",             2,         -35.034,          NA,   -281.992,       1,
  25,    "A",             3,         -35.003,          NA,   -280.444,       1,
  26,    "A",             4,         -35.,             NA,   -280.099,       1,
  27,    "A",             5,         -35.,             NA,   -280.022,       1,
  28,    "A",             6,         -35.,             NA,   -280.005,       1,
  29,    "A",             7,         -35.,             NA,   -280.001,       1
)

test_that("test that no NA mean memory coefficients are kept", {

  actual <- calculateMemoryCoefficients(dataset5)

  expect_equal(nrow(actual), 6)
  expect_equal(sum(is.na(c(actual$memoryCoeffD18O, actual$memoryCoeffDD))), 0)

})

test_that("test that no sample data is lost in memory correction", {

  expect_error(actual <- correctForMemoryEffect(dataset6), NA)

  skip_if_not(exists("actual"), "previous test")

  actual <- actual$datasetMemoryCorrected %>%
    dplyr::filter(`Identifier 1` == "A")

  expect_equal(sum(is.na(c(actual$`d(18_16)Mean`, actual$`d(D_H)Mean`))), 0)

})

test_that("test that vial grouping gives correct memory coefficients output", {

  actual <- calculateMemoryCoefficients(dataset4)

  expect_equal(dim(actual), c(4, 13))
  expect_equal(colnames(actual),
               c("Inj Nr", "A_vial1_d18O", "A_vial1_dD",
                 "A_vial2_d18O", "A_vial2_dD",
                 "B_vial1_d18O", "B_vial1_dD",
                 "C_vial1_d18O", "C_vial1_dD",
                 "memoryCoeffD18O", "memoryCoeffDD",
                 "sdMemoryCoeffD18O", "sdMemoryCoeffDD"))

})

test_that("test memory correction for identical non-consecutive samples", {

  # in this dataset:
  # d18O: m1 = 0.5, m2 = 0.75, m3 = 0.875, m4 = 1
  # dD: m1 = 0.4, m2 = 0.6, m3 = 0.8, m4 = 1
  dataset7 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~`vial_group`,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / --------------
    1,     "A",             1,         1,               1,      -5,            1,
    2,     "A",             2,         1,               1,      -5,            1,
    3,     "A",             3,         1,               1,      -5,            1,
    4,     "A",             4,         1,               1,      -5,            1,
    5,     "A",             5,         1,               1,      -5,            1,
    6,     "A",             6,         1,               1,      -5,            1,
    7,     "B",             1,         1.5,             1,      -4.2,          1,
    8,     "B",             2,         1.75,            1,      -3.8,          1,
    9,     "B",             3,         1.875,           1,      -3.4,          1,
    10,    "B",             4,         2.0,             1,      -3.0,          1,
    11,    "B",             5,         2.0,             1,      -3.0,          1,
    12,    "B",             6,         2.0,             1,      -3.0,          1,
    13,    "C",             1,         2.5,             1,      -2.2,          1,
    14,    "C",             2,         2.75,            1,      -1.8,          1,
    15,    "C",             3,         2.875,           1,      -1.4,          1,
    16,    "C",             4,         3.0,             1,      -1,            1,
    17,    "C",             5,         3.0,             1,      -1,            1,
    18,    "C",             6,         3.0,             1,      -1,            1,
    19,    "Probe1",        1,         2.0,             NA,     3.4,           1,
    20,    "Probe1",        2,         1.5,             NA,     5.6,           1,
    21,    "Probe1",        3,         1.25,            NA,     7.8,           1,
    22,    "Probe2",        1,         1.5,             NA,     14.0,          1,
    23,    "Probe2",        2,         1.75,            NA,     16.0,          1,
    24,    "Probe2",        3,         1.875,           NA,     18.0,          1,
    25,    "Probe1",        1,         1.5,             NA,     16.0,          2,
    26,    "Probe1",        2,         1.25,            NA,     14.0,          2,
    27,    "Probe1",        3,         1.125,           NA,     12.0,          2,
    28,    "A",             1,         1,               2,      4,             2,
    29,    "A",             2,         1,               2,      1,             2,
    30,    "A",             3,         1,               2,      -2,            2
  )
  expected <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block, ~`d(D_H)Mean`, ~`vial_group`,
    # -- / -------------- / -------- / -------------- / ----- / ------------ / --------------
    1,     "A",             1,         NA,              1,      NA,            1,
    2,     "A",             2,         NA,              1,      NA,            1,
    3,     "A",             3,         NA,              1,      NA,            1,
    4,     "A",             4,         NA,              1,      NA,            1,
    5,     "A",             5,         NA,              1,      NA,            1,
    6,     "A",             6,         NA,              1,      NA,            1,
    7,     "B",             1,         2.0,             1,      -3.0,          1,
    8,     "B",             2,         2.0,             1,      -3.0,          1,
    9,     "B",             3,         2.0,             1,      -3.0,          1,
    10,    "B",             4,         2.0,             1,      -3.0,          1,
    11,    "B",             5,         2.0,             1,      -3.0,          1,
    12,    "B",             6,         2.0,             1,      -3.0,          1,
    13,    "C",             1,         3.0,             1,      -1.0,          1,
    14,    "C",             2,         3.0,             1,      -1.0,          1,
    15,    "C",             3,         3.0,             1,      -1.0,          1,
    16,    "C",             4,         3.0,             1,      -1.0,          1,
    17,    "C",             5,         3.0,             1,      -1.0,          1,
    18,    "C",             6,         3.0,             1,      -1.0,          1,
    19,    "Probe1",        1,         1.0,             NA,     10.0,          1,
    20,    "Probe1",        2,         1.0,             NA,     10.0,          1,
    21,    "Probe1",        3,         1.0,             NA,     10.0,          1,
    22,    "Probe2",        1,         2.0,             NA,     20.0,          1,
    23,    "Probe2",        2,         2.0,             NA,     20.0,          1,
    24,    "Probe2",        3,         2.0,             NA,     20.0,          1,
    25,    "Probe1",        1,         1.0,             NA,     10.0,          2,
    26,    "Probe1",        2,         1.0,             NA,     10.0,          2,
    27,    "Probe1",        3,         1.0,             NA,     10.0,          2,
    28,    "A",             1,         1.0,             2,      -5.0,          2,
    29,    "A",             2,         1.0,             2,      -5.0,          2,
    30,    "A",             3,         1.0,             2,      -5.0,          2
  )

  actual <- correctForMemoryEffect(dataset7)
  actualRounded <- dplyr::mutate(actual$datasetMemoryCorrected,
                                 `d(D_H)Mean` = round(`d(D_H)Mean`, 1),
                                 `d(18_16)Mean` = round(`d(18_16)Mean`, 1))

  expect_equal(actualRounded, expected)

})
