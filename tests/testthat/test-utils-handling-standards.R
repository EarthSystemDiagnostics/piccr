test_that("associating standards for single row works", {

  config <- list(standards = list(list(name = "STD_A", 
                                       o18_True = -2, 
                                       H2_True = 3,
                                       use_for_drift_correction = FALSE,
                                       use_for_calibration = TRUE,
                                       use_as_control_standard = FALSE), 
                                  list(name = "STD_B", 
                                       o18_True = 1.8,
                                       H2_True = -0.4,
                                       use_for_drift_correction = TRUE,
                                       use_for_calibration = TRUE,
                                       use_as_control_standard = TRUE)))
  df_1 <- tibble::tibble(
    `Identifier 1` = c("STD_A", "STD_B", "Probe", "STD_A"),
    val = 1:4
  )
  dfExpected_1 <- tibble::tibble(
    `Identifier 1` = c("STD_A", "STD_B", "Probe", "STD_A"),
    val = 1:4,
    o18_True = c(-2, 1.8, NA, -2),
    H2_True = c(3, -0.4, NA, 3),
    useForDriftCorr = c(F, T, NA, F),
    useForCalibration = c(T, T, NA, T),
    useAsControlStandard = c(F, T, NA, F)
  )
  df_2 <- tibble::tibble(
    `Identifier 1` = c("xyz", "STD_B", "Probe", "STD_A"),
    val = 1:4
  )
  dfExpected_2 <- tibble::tibble(
    `Identifier 1` = c("xyz", "STD_B", "Probe", "STD_A"),
    val = 1:4,
    o18_True = c(NA, 1.8, NA, -2),
    H2_True = c(NA, -0.4, NA, 3),
    useForDriftCorr = c(NA, T, NA, F),
    useForCalibration = c(NA, T, NA, T),
    useAsControlStandard = c(NA, T, NA, F)
  )
  
  df1Actual <- associateStandardsWithConfigInfo(df_1, config)
  df2Actual <- associateStandardsWithConfigInfo(df_2, config)
  
  expect_equal(df1Actual, dfExpected_1)
  expect_equal(df2Actual, dfExpected_2)

})

config <- list(standards = list(list(name = "STD_A"), list(name = "STD_B")))

test_that("grouping std for df with single row containing standard works", {

  df <- tibble::tibble(
    `Identifier 1` = "STD_A",
    val = 3
  )
  dfWithGroupedStandardsExpected <- tibble::tibble(
    `Identifier 1` = "STD_A",
    val = 3,
    block = 1L
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocks(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)

})

test_that("grouping std for df with single row not containing standard works", {

  df <- tibble::tibble(
    `Identifier 1` = "Probe",
    val = 3
  )
  dfWithGroupedStandardsExpected <- tibble::tibble(
    `Identifier 1` = "Probe",
    val = 3,
    block = NA_integer_
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocks(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)

})

test_that("grouping std for df with mulitple rows (std-probe) works", {

  df <- tibble::tibble(
    `Identifier 1` = c("STD_A", "Probe"),
    val = c(3, 7)
  )
  dfWithGroupedStandardsExpected <- tibble::tibble(
    `Identifier 1` = c("STD_A", "Probe"),
    val = c(3, 7),
    block = c(1L, NA)
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocks(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)

})

test_that("grouping std for df with mulitple rows (probe-std-probe) works", {

  df <- tibble::tibble(
    `Identifier 1` = c("Probe", "STD_A", "Probe"),
    val = c(1, 3, 7)
  )
  dfWithGroupedStandardsExpected <- tibble::tibble(
    `Identifier 1` = c("Probe", "STD_A", "Probe"),
    val = c(1, 3, 7),
    block = c(NA, 1L, NA)
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocks(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)

})

test_that("grouping std for df with mulitple rows (std-probe-std) works", {

  df <- tibble::tibble(
    `Identifier 1` = c("STD_A", "Probe", "STD_B"),
    val = c(1, 3, 7)
  )
  dfWithGroupedStandardsExpected <- tibble::tibble(
    `Identifier 1` = c("STD_A", "Probe", "STD_B"),
    val = c(1, 3, 7),
    block = c(1L, NA, 2L)
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocks(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)

})

test_that("grouping for df with many rows (std-probe-std-probe-std) works", {

  df <- tibble::tibble(
    `Identifier 1` = c("STD_A", "Probe", "Probe", "STD_B", "STD_A", "Probe", "STD_A"),
    val = 1:7
  )
  dfWithGroupedStandardsExpected <- tibble::tibble(
    `Identifier 1` = c("STD_A", "Probe", "Probe", "STD_B", "STD_A", "Probe", "STD_A"),
    val = 1:7,
    block = c(1L, NA, NA, 2L, 2L, NA, 3L)
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocks(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)

})
