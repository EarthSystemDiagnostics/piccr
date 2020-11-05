library(tibble)
library(dplyr)

context("test calibrateUsingDoubleCalibration")


test_that("test calibrateUsingDoubleCalibration (no drift, calibration slope and intercept 0)", {
  
  dataset <- tibble::tribble(
    ~`Identifier 1`, ~block, ~`Time Code`,               ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~o18_True, ~H2_True, ~useForCalibration,
    # ------------ / ----- / ---------------------- / -------------- / ------------ / -------- / ------- / ------------------
    "Std_A",         1,      "2019/11/2504:47:06",    1,               2,             1,         2,        TRUE,
    "Std_A",         1,      "2019/11/2504:55:10",    1,               2,             1,         2,        TRUE,
    "Std_B",         1,      "2019/11/2505:02:03",    2,               3,             2,         3,        TRUE,
    "Std_B",         1,      "2019/11/2505:08:55",    2,               3,             2,         3,        TRUE,
    "Probe_A",       NA,     "2019/11/2505:15:33",    1,               2,             1,         2,        TRUE,
    "Probe_B",       NA,     "2019/11/2505:21:00",    1,               2,             1,         2,        TRUE,
    "Std_A",         2,      "2019/11/2505:30:03",    0,               0,             1,         2,        TRUE,
    "Std_B",         2,      "2019/11/2505:37:09",    0,               0,             1,         2,        TRUE,
    "Probe_C",       NA,     "2019/11/2505:50:40",    1,               2,             1,         2,        TRUE,
    "Std_A",         3,      "2019/11/2506:00:00",    1,               2,             1,         2,        FALSE,
    "Std_B",         3,      "2019/11/2506:07:11",    1,               2,             1,         2,        FALSE,
    "Probe_D",       NA,     "2019/11/2506:13:28",    1,               2,             1,         2,        TRUE,
    "Std_A",         4,      "2019/11/2506:20:59",    1,               2,             1,         2,        TRUE,
    "Std_A",         4,      "2019/11/2506:29:18",    1,               2,             1,         2,        TRUE,
    "Std_B",         4,      "2019/11/2506:36:59",    2,               3,             2,         3,        TRUE,
    "Std_B",         4,      "2019/11/2506:45:31",    2,               3,             2,         3,        TRUE
    )

  expectedParameter <- tibble::tibble(
    species = c("d18O", "dD", "d18O", "dD"),
    block = c(1, 1, 4, 4),
    timeStamp = c(672., 672., 6366., 6366.),
    intercept = rep(0., 4),
    slope = rep(1., 4),
    pValueIntercept = c(0.13, 0.26, 0.13, 0.26),
    pValueSlope = rep(0., 4),
    residualRMSD = rep(0., 4),
    rSquared = rep(1, 4)
  )

  expected <- list(dataset = dataset, parameter = expectedParameter)

  config <- list(use_memory_correction = TRUE, use_three_point_calibration = TRUE)
  
  actual <- calibrateUsingDoubleCalibration(dataset, config)

  expect_type(actual, "list")
  expect_length(actual, 2)

  actual$dataset <- dplyr::mutate(actual$dataset, `d(18_16)Mean` = round(`d(18_16)Mean`, 10),
                                  `d(D_H)Mean` = round(`d(D_H)Mean`, 10))
  actual$parameter$timeStamp <- round(actual$parameter$timeStamp, 0)
  
  expect_equal(actual, expected)
})

test_that("test getCalibrationSlopes (case slopes are zero)", {
  
  params <- tibble::tibble(
    species = c("d18O", "dD", "d18O", "dD"),
    block = c(1, 1, 56, 56),
    slope = rep(2, 4),
    intercept = rep(4, 4),
    timeStamp = c(1, 1, 10, 10)
  )
  
  actual <- getCalibrationSlopes(params)
  
  expect_length(actual, 2)
  expect_equal(actual$d18O$alpha, 0)
  expect_equal(actual$d18O$beta, 0)
  expect_equal(actual$dD$alpha, 0)
  expect_equal(actual$dD$beta, 0)
})

test_that("test getCalibrationSlopes (case slopes are not zero)", {

  params <- tibble::tibble(
    species = c("d18O", "dD", "d18O", "dD"),
    block = c(1, 1, 3, 3),
    slope = c(1, 4, 11, -16),
    intercept = c(-3, 0, -5, 60),
    timeStamp = c(4, 4, 24, 24)
  )
  
  actual <- getCalibrationSlopes(params)
  
  expect_length(actual, 2)
  expect_equal(actual$d18O$beta, 0.5)
  expect_equal(actual$d18O$alpha, -0.1)
  expect_equal(actual$dD$beta, -1)
  expect_equal(actual$dD$alpha, 3)
})

test_that("test applyDoubleCalibration", {
  
  dataset <- tibble::tribble(
    ~`Identifier 1`, ~block, ~`Time Code`,               ~`d(18_16)Mean`, ~`d(D_H)Mean`,
    # ------------ / ----- / ---------------------- / -------------- / ------------
    "Std_A",         1,      "2019/11/2504:47:06",    1,               2,
    "Std_A",         1,      "2019/11/2504:55:10",    1,               2,
    "Std_B",         1,      "2019/11/2505:02:03",    1,               2,
    "Std_B",         1,      "2019/11/2505:08:55",    1,               2,
    "Probe_A",       NA,     "2019/11/2505:15:33",    1,               2,
    "Probe_B",       NA,     "2019/11/2505:21:00",    1,               2,
    "Std_A",         2,      "2019/11/2505:30:03",    1,               2,
    "Std_B",         2,      "2019/11/2505:37:09",    1,               2,
    "Probe_C",       NA,     "2019/11/2505:50:40",    1,               2,
    "Std_A",         3,      "2019/11/2506:00:00",    1,               2,
    "Std_A",         3,      "2019/11/2506:07:11",    1,               2,
    "Std_B",         3,      "2019/11/2506:13:28",    1,               2,
    "Std_B",         3,      "2019/11/2506:20:59",    1,               2
  )
  params <- tibble::tibble(
    species = c("d18O", "dD", "d18O", "dD"),
    block = c(1, 1, 3, 3),
    slope = c(1, 4, 2164, 4),
    intercept = c(-3, 0, 4323, -2163),
    timeStamp = c(672.5, 672.5, 4998.5, 4998.5)
  )
  
  expected <- tibble::tribble(
    ~`Identifier 1`, ~block, ~`Time Code`,               ~`d(18_16)Mean`, ~`d(D_H)Mean`,
    # ------------ / ----- / ---------------------- / -------------- / ------------
    "Std_A",         1,      "2019/11/2504:47:06",    -2,              8,
    "Std_A",         1,      "2019/11/2504:55:10",    724,             -234,
    "Std_B",         1,      "2019/11/2505:02:03",    1344,            -440,
    "Std_B",         1,      "2019/11/2505:08:55",    1962,            -646,
    "Probe_A",       NA,     "2019/11/2505:15:33",    2558,            -846,
    "Probe_B",       NA,     "2019/11/2505:21:00",    3049,            -1009,
    "Std_A",         2,      "2019/11/2505:30:03",    3864,            -1280,
    "Std_B",         2,      "2019/11/2505:37:09",    4502,            -1494,
    "Probe_C",       NA,     "2019/11/2505:50:40",    5719,            -1899,
    "Std_A",         3,      "2019/11/2506:00:00",    6559,            -2179,
    "Std_A",         3,      "2019/11/2506:07:11",    7206,            -2394,
    "Std_B",         3,      "2019/11/2506:13:28",    7771,            -2583,
    "Std_B",         3,      "2019/11/2506:20:59",    8448,            -2808
  )
  
  actual <- applyDoubleCalibration(dataset, params)
  actual <- dplyr::mutate(actual, `d(18_16)Mean` = round(actual$`d(18_16)Mean`), `d(D_H)Mean` = round(actual$`d(D_H)Mean`))
  
  expect_equal(actual, expected)
})
