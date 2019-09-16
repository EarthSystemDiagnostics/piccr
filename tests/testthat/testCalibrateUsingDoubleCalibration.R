library(testthat)

context("test calibrateUsingDoubleCalibration")


test_that("test calibrateUsingDoubleCalibration (no drift, calibration slope and intercept 0)", {
  
  dataset <- tribble(
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
    "Std_A",         3,      "2019/11/2506:00:00",    1,               2,             1,         2,        TRUE,
    "Std_A",         3,      "2019/11/2506:07:11",    1,               2,             1,         2,        TRUE,
    "Std_B",         3,      "2019/11/2506:13:28",    2,               3,             2,         3,        TRUE,
    "Std_B",         3,      "2019/11/2506:20:59",    2,               3,             2,         3,        TRUE
  )
  
  config <- list(use_memory_correction = TRUE, use_three_point_calibration = TRUE)
  
  actual <- calibrateUsingDoubleCalibration(list(df1 = dataset), config)
  actual$df1 <- mutate(actual$df1, `d(18_16)Mean` = round(actual$df1$`d(18_16)Mean`, 10), 
                                   `d(D_H)Mean` = round(actual$df1$`d(D_H)Mean`, 10))
  
  expect_is(actual, "list")
  expect_length(actual, 1)
  expect_equal(actual$df1, dataset)
})

test_that("test getCalibrationSlopes (case slopes are zero)", {
  
  params1 <- list(
    d18O = list(slope = 2, intercept = 4),
    dD = list(slope = 2, intercept = 4)
  )
  params3 <- list(
    d18O = list(slope = 2, intercept = 4),
    dD = list(slope = 2, intercept = 4)
  )
  times = c(1, 10)
  
  actual <- getCalibrationSlopes(params1, params3, times)
  
  expect_length(actual, 2)
  expect_equal(actual$d18O$alpha, 0)
  expect_equal(actual$d18O$beta, 0)
  expect_equal(actual$dD$alpha, 0)
  expect_equal(actual$dD$beta, 0)
})

test_that("test getCalibrationSlopes (case slopes are not zero)", {
  
  params1 <- list(
    d18O = list(slope = 1, intercept = -3),
    dD = list(slope = 4, intercept = 0)
  )
  params3 <- list(
    d18O = list(slope = 11, intercept = -5),
    dD = list(slope = -16, intercept = 60)
  )
  times = c(4, 24)
  
  actual <- getCalibrationSlopes(params1, params3, times)
  
  expect_length(actual, 2)
  expect_equal(actual$d18O$beta, 0.5)
  expect_equal(actual$d18O$alpha, -0.1)
  expect_equal(actual$dD$beta, -1)
  expect_equal(actual$dD$alpha, 3)
})

test_that("test applyDoubleCalibration", {
  
  dataset <- tribble(
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
  params1 <- list(
    d18O = list(slope = 1, intercept = -3),
    dD = list(slope = 4, intercept = 0)
  )
  calibSlopes = list(
    d18O = list(alpha = 1, beta = 0.5),
    dD = list(alpha = -0.5, beta = 0)
  )
  
  expected <- tribble(
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
  
  actual <- applyDoubleCalibration(dataset, params1, calibSlopes)
  actual <- mutate(actual, `d(18_16)Mean` = round(actual$`d(18_16)Mean`), `d(D_H)Mean` = round(actual$`d(D_H)Mean`))
  
  expect_equal(actual, expected)
})