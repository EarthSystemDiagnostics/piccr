library(testthat)
library(tidyverse)

context("test calibrateUsingSimpleDriftCorrection")

# this dataset does not have drift. -> drift slope: 0
dataset1 <- tribble(
  ~`Identifier 1`, ~block, ~`Time Code`,               ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~useForDriftCorr,
  # ------------ / ----- / ---------------------- / -------------- / ------------ / ----------------
  "Std_A",         1,      "2019/11/2504:47:06",    1,               2,             TRUE,
  "Std_A",         1,      "2019/11/2504:55:10",    1,               2,             TRUE,
  "Std_B",         1,      "2019/11/2505:02:03",    1,               2,             TRUE,
  "Std_B",         1,      "2019/11/2505:08:55",    1,               2,             TRUE,
  "Probe_A",       NA,     "2019/11/2505:15:33",    1,               2,             NA,
  "Probe_B",       NA,     "2019/11/2505:21:00",    1,               2,             NA,
  "Std_A",         2,      "2019/11/2505:30:03",    1,               2,             TRUE,
  "Std_B",         2,      "2019/11/2505:37:09",    1,               2,             TRUE,
  "Probe_C",       NA,     "2019/11/2505:50:40",    1,               2,             NA,
  "Std_A",         3,      "2019/11/2506:00:00",    1,               2,             TRUE,
  "Std_A",         3,      "2019/11/2506:07:11",    1,               2,             TRUE,
  "Std_B",         3,      "2019/11/2506:13:28",    1,               2,             TRUE,
  "Std_B",         3,      "2019/11/2506:20:59",    1,               2,             TRUE
)

# this dataset has a constant drift. -> drift slope: d18O: 1 / sec; dD: -0.5 / sec
dataset2 <- tribble(
  ~`Identifier 1`, ~block, ~`Time Code`,               ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~useForDriftCorr,
  # ------------ / ----- / ---------------------- / -------------- / ------------ / ----------------
  "Std_A",         1,      "2019/11/2510:00:00",    0,                -3,            TRUE,
  "Std_B",         1,      "2019/11/2510:05:00",    300,              -145,          TRUE,
  "Probe_A",       NA,     "2019/11/2510:10:00",    10,               0,             NA,
  "Probe_B",       NA,     "2019/11/2510:15:00",    10,               0,             NA,
  "Std_A",         2,      "2019/11/2510:20:00",    1200,             -603,          TRUE,
  "Std_B",         2,      "2019/11/2510:25:00",    1500,             -745,          TRUE,
  "Probe_C",       NA,     "2019/11/2510:30:00",    10,               0,             NA,
  "Std_A",         3,      "2019/11/2510:35:00",    2100,             -1053,         TRUE,
  "Std_B",         3,      "2019/11/2510:40:00",    2400,             -1195,         TRUE
)
expected2 <- tribble(
  ~`Identifier 1`, ~block, ~`Time Code`,               ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~useForDriftCorr,
  # ------------ / ----- / ---------------------- / -------------- / ------------ / ----------------
  "Std_A",         1,      "2019/11/2510:00:00",    0,                -3,            TRUE,
  "Std_B",         1,      "2019/11/2510:05:00",    0,                5,             TRUE,
  "Probe_A",       NA,     "2019/11/2510:10:00",    -590,             300,           NA,
  "Probe_B",       NA,     "2019/11/2510:15:00",    -890,             450,           NA,
  "Std_A",         2,      "2019/11/2510:20:00",    0,                -3,            TRUE,
  "Std_B",         2,      "2019/11/2510:25:00",    0,                5,             TRUE,
  "Probe_C",       NA,     "2019/11/2510:30:00",    -1790,            900,           NA,
  "Std_A",         3,      "2019/11/2510:35:00",    0,                -3,            TRUE,
  "Std_B",         3,      "2019/11/2510:40:00",    0,                5,            TRUE
)

# this dataset has a different constant drift for both standards.
# d18O: Std_A 1/sec; Std_B 2/sec
# dD: StdA -0.5/sec; Std_B 0.5/sec
dataset3 <- tribble(
  ~`Identifier 1`, ~block, ~`Time Code`,               ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~useForDriftCorr,
  # ------------ / ----- / ---------------------- / -------------- / ------------ / ----------------
  "Std_A",         1,      "2019/11/2510:00:00",    0,                0,             TRUE,
  "Std_B",         1,      "2019/11/2510:05:00",    600,              150,           TRUE,
  "Probe_A",       NA,     "2019/11/2510:10:00",    0,                0,             NA,
  "Probe_B",       NA,     "2019/11/2510:15:00",    0,                0,             NA,
  "Std_A",         2,      "2019/11/2510:20:00",    1200,             -600,          TRUE,
  "Std_B",         2,      "2019/11/2510:25:00",    3000,             750,           TRUE,
  "Probe_C",       NA,     "2019/11/2510:30:00",    0,                0,             NA,
  "Std_A",         3,      "2019/11/2510:35:00",    2100,             -1050,         TRUE,
  "Std_B",         3,      "2019/11/2510:40:00",    4800,             1200,          TRUE
)
expected3 <- tribble(
  ~`Identifier 1`, ~block, ~`Time Code`,               ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~useForDriftCorr,
  # ------------ / ----- / ---------------------- / -------------- / ------------ / ----------------
  "Std_A",         1,      "2019/11/2510:00:00",    0,                0,             TRUE,
  "Std_B",         1,      "2019/11/2510:05:00",    150,              150,           TRUE,
  "Probe_A",       NA,     "2019/11/2510:10:00",    -900,             0,             NA,
  "Probe_B",       NA,     "2019/11/2510:15:00",    -1350,            0,             NA,
  "Std_A",         2,      "2019/11/2510:20:00",    -600,             -600,          TRUE,
  "Std_B",         2,      "2019/11/2510:25:00",    750,              750,           TRUE,
  "Probe_C",       NA,     "2019/11/2510:30:00",    -2700,            0,             NA,
  "Std_A",         3,      "2019/11/2510:35:00",    -1050,            -1050,         TRUE,
  "Std_B",         3,      "2019/11/2510:40:00",    1200,             1200,          TRUE
)

test_that("test linearDriftCorrection", {
  
  actual1 <- linearDriftCorrection(dataset1)
  actual2 <- linearDriftCorrection(dataset2)
  actual3 <- linearDriftCorrection(dataset3)
  
  expect_equal(mutate_if(actual1, is.numeric, round), dataset1)
  expect_equal(mutate_if(actual2, is.numeric, round), expected2)
  expect_equal(mutate_if(actual3, is.numeric, round), expected3)
})

test_that("test calculate drift slope alpha (dataset1)", {
  
  dataset1 <- addColumnSecondsSinceStart(dataset1)
  actual <- calculateDriftSlope(dataset1)
  
  expect_equal(actual$d18O, 0)
  expect_equal(actual$dD, 0)
})

test_that("test calculate drift slope alpha (dataset2)", {
  
  dataset2 <- addColumnSecondsSinceStart(dataset2)
  actual <- calculateDriftSlope(dataset2)
  
  expect_equal(actual$d18O, 1)
  expect_equal(actual$dD, -0.5)
})

test_that("test calculate drift slope alpha (dataset3)", {
  
  dataset3 <- addColumnSecondsSinceStart(dataset3)
  actual <- calculateDriftSlope(dataset3)
  
  expect_equal(actual$d18O, 1.5)
  expect_equal(actual$dD, 0)
})

test_that("test use only standards specified in config", {
  
  # this dataset has a different constant drift for both standards.
  # d18O: Std_A 1/sec; Std_B 2/sec
  # dD: StdA -0.5/sec; Std_B 0.5/sec
  dataset3 <- tribble(
    ~`Identifier 1`, ~block, ~`Time Code`,               ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~useForDriftCorr,
    # ------------ / ----- / ---------------------- / -------------- / ------------ / ----------------
    "Std_A",         1,      "2019/11/2510:00:00",    0,                0,             TRUE,
    "Std_B",         1,      "2019/11/2510:05:00",    600,              150,           FALSE,
    "Probe_A",       NA,     "2019/11/2510:10:00",    10,               0,             NA,
    "Probe_B",       NA,     "2019/11/2510:15:00",    10,               0,             NA,
    "Std_A",         2,      "2019/11/2510:20:00",    1200,             -600,          TRUE,
    "Std_B",         2,      "2019/11/2510:25:00",    3000,             750,           FALSE,
    "Probe_C",       NA,     "2019/11/2510:30:00",    10,               0,             NA,
    "Std_A",         3,      "2019/11/2510:35:00",    2100,             -1050,         TRUE,
    "Std_B",         3,      "2019/11/2510:40:00",    4800,             1200,          FALSE
  )
  
  dataset3 <- addColumnSecondsSinceStart(dataset3)
  actual <- calculateDriftSlope(dataset3)
  
  expect_equal(actual$d18O, 1)
  expect_equal(actual$dD, -0.5)
  
})