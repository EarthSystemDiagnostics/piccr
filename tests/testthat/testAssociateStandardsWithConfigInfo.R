library(testthat)
library(tidyverse)

context("Test associating standards with their true values")

test_that("associating standards for single row (standard, including usage in phases)", {
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
  df_1 <- tibble(
    `Identifier 1` = c("STD_A", "STD_B", "Probe", "STD_A"),
    val = 1:4
  )
  dfExpected_1 <- tibble(
    `Identifier 1` = c("STD_A", "STD_B", "Probe", "STD_A"),
    val = 1:4,
    o18_True = c(-2, 1.8, NA, -2),
    H2_True = c(3, -0.4, NA, 3),
    useForDriftCorr = c(F, T, NA, F),
    useForCalibration = c(T, T, NA, T),
    useAsControlStandard = c(F, T, NA, F)
  )
  df_2 <- tibble(
    `Identifier 1` = c("xyz", "STD_B", "Probe", "STD_A"),
    val = 1:4
  )
  dfExpected_2 <- tibble(
    `Identifier 1` = c("xyz", "STD_B", "Probe", "STD_A"),
    val = 1:4,
    o18_True = c(NA, 1.8, NA, -2),
    H2_True = c(NA, -0.4, NA, 3),
    useForDriftCorr = c(NA, T, NA, F),
    useForCalibration = c(NA, T, NA, T),
    useAsControlStandard = c(NA, T, NA, F)
  )
  
  dfActual <- associateStandardsWithConfigInfo(list(df_1, df_2), config)
  
  expect_length(dfActual, 2)
  expect_equal(dfActual[[1]], dfExpected_1)
  expect_equal(dfActual[[2]], dfExpected_2)
})
