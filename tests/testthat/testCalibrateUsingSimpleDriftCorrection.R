library(tibble)
library(dplyr)

context("test calibrateUsingSimpleDriftCorrection")

# this dataset does not have drift. -> drift slope: 0
dataset1 <- tibble::tribble(
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
dataset2 <- tibble::tribble(
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
expected2 <- tibble::tribble(
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
dataset3 <- tibble::tribble(
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
expected3 <- tibble::tribble(
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

# as dataset2, but with more standard injections
dataset4 <- tibble::tribble(
  ~`Identifier 1`, ~block, ~`Time Code`,         ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~useForDriftCorr, ~vial_group,
  # ------------ / ----- / ------------------- / -------------- / ------------ / --------------- / -----------
  "Std_A",         1,      "2019/11/2510:00:00", 0,               -3,            TRUE,             1,
  "Std_A",         1,      "2019/11/2510:05:00", 300,             -153,          TRUE,             1,
  "Std_A",         1,      "2019/11/2510:10:00", 600,             -303,          TRUE,             1,
  "Std_A",         1,      "2019/11/2510:15:00", 900,             -453,          TRUE,             1,
  "Std_B",         1,      "2019/11/2510:20:00", 100,             -145,          TRUE,             1,
  "Std_B",         1,      "2019/11/2510:25:00", 400,             -295,          TRUE,             1,
  "Std_B",         1,      "2019/11/2510:30:00", 700,             -445,          TRUE,             1,
  "Std_B",         1,      "2019/11/2510:35:00", 1000,            -595,          TRUE,             1,
  "Probe_A",       NA,     "2019/11/2510:40:00", 10,              0,             NA,               1,
  "Probe_B",       NA,     "2019/11/2510:45:00", 10,              0,             NA,               1,
  "Std_A",         2,      "2019/11/2510:50:00", 3000,            -1503,         TRUE,             1,
  "Std_A",         2,      "2019/11/2510:55:00", 3300,            -1653,         TRUE,             1,
  "Std_A",         2,      "2019/11/2511:00:00", 3600,            -1803,         TRUE,             1,
  "Std_A",         2,      "2019/11/2511:05:00", 3900,            -1953,         TRUE,             1,
  "Std_B",         2,      "2019/11/2511:10:00", 3100,            -1645,         TRUE,             1,
  "Std_B",         2,      "2019/11/2511:15:00", 3400,            -1795,         TRUE,             1,
  "Std_B",         2,      "2019/11/2511:20:00", 3700,            -1945,         TRUE,             1,
  "Std_B",         2,      "2019/11/2511:25:00", 4000,            -2095,         TRUE,             1,
  "Probe_C",       NA,     "2019/11/2511:30:00", 10,              0,             NA,               1,
  "Std_A",         3,      "2019/11/2511:35:00", 5700,            -2853,         TRUE,             1,
  "Std_A",         3,      "2019/11/2511:40:00", 6000,            -3003,         TRUE,             1,
  "Std_A",         3,      "2019/11/2511:45:00", 6300,            -3153,         TRUE,             1,
  "Std_A",         3,      "2019/11/2511:50:00", 6600,            -3303,         TRUE,             1,
  "Std_B",         3,      "2019/11/2511:55:00", 5800,            -2995,         TRUE,             1,
  "Std_B",         3,      "2019/11/2512:00:00", 6100,            -3145,         TRUE,             1,
  "Std_B",         3,      "2019/11/2512:05:00", 6400,            -3295,         TRUE,             1,
  "Std_B",         3,      "2019/11/2512:10:00", 6700,            -3445,         TRUE,             1
)

expectedParams <- tibble::tibble(
  species = c(rep("d18O", 3), rep("dD", 3)),
  sample = c("Std_A", "Std_B", "mean", "Std_A", "Std_B", "mean"),
  slope = rep(0, 6),
  pValue = c(0.26, 0.29, NA, 0.26, 0.29, NA),
  residualRMSD = c(0, 0, NA, 0, 0, NA),
  rSquared = c(0.48, 0.47, NA, 0.48, 0.47, NA)
)

config <- list(use_memory_correction = TRUE)

test_that("running the calibration model", {

  # should throw an error
  expect_error(runDriftModel(dataset1, species = "unknown"))
})

test_that("test linearDriftCorrection", {
  
  actual1 <- linearDriftCorrection(dataset1, config)$dataset
  actual2 <- linearDriftCorrection(dataset2, config)$dataset
  actual3 <- linearDriftCorrection(dataset3, config)$dataset
  
  expect_equal(dplyr::mutate_if(actual1, is.numeric, round), dataset1)
  expect_equal(dplyr::mutate_if(actual2, is.numeric, round), expected2)
  expect_equal(dplyr::mutate_if(actual3, is.numeric, round), expected3)
})

test_that("test calculate drift slope alpha (dataset1)", {
  
  dataset1 <- addColumnSecondsSinceStart(dataset1)
  actual <- calculateDriftSlope(dataset1, config)
  
  expect_equal(actual, expectedParams)
})

test_that("test calculate drift slope alpha (dataset2)", {
  
  dataset2 <- addColumnSecondsSinceStart(dataset2)
  actual <- calculateDriftSlope(dataset2, config)
  
  expect_equal(actual$slope, c(rep(1, 3), rep(-0.5, 3)))
})

test_that("test calculate drift slope alpha (dataset3)", {
  
  dataset3 <- addColumnSecondsSinceStart(dataset3)
  actual <- calculateDriftSlope(dataset3, config)
  
  expect_equal(actual$slope, c(1, 2, 1.5, -0.5, 0.5, 0))
})

test_that("test use only standards specified in config", {
  
  # this dataset has a different constant drift for both standards.
  # d18O: Std_A 1/sec; Std_B 2/sec
  # dD: StdA -0.5/sec; Std_B 0.5/sec
  dataset3 <- tibble::tribble(
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
  actual <- calculateDriftSlope(dataset3, config)

  expect_equal(actual$slope, c(1, 1, -0.5, -0.5))
})

test_that("test correct drift slopes when memory correction is off", {

  config <- list(use_memory_correction = FALSE)

  dataset4 <- addColumnSecondsSinceStart(dataset4)
  actual <- calculateDriftSlope(dataset4, config)

  expect_equal(actual$slope, c(rep(1, 3), rep(-0.5, 3)))
})
