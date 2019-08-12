library(testthat)
library(tidyverse)
source("../R/associateStandardsWithConfigInfo.R")

context("Test associating standards with their true values")

test_that("associating standards for single row (standard, including usage in phases)", {
  config <- list(STANDARDS = list(list(NAME = "STD_A", 
                                       O18_VAL = -2, 
                                       H2_VAL = 3,
                                       MEMORYCORR = TRUE,
                                       DRIFTCORR = FALSE,
                                       CALIBRATION = TRUE), 
                                  list(NAME = "STD_B", 
                                       O18_VAL = 1.8,
                                       H2_VAL = -0.4,
                                       MEMORYCORR = NA,
                                       DRIFTCORR = TRUE,
                                       CALIBRATION = TRUE)))
  df_1 <- tibble(
    `Identifier 1` = c("STD_A", "STD_B", "Probe", "STD_A"),
    val = 1:4
  )
  dfExpected_1 <- tibble(
    `Identifier 1` = c("STD_A", "STD_B", "Probe", "STD_A"),
    val = 1:4,
    O18_True = c(-2, 1.8, NA, -2),
    H2_True = c(3, -0.4, NA, 3),
    useForMemCorr = c(T, NA, NA, T),
    useForDriftCorr = c(F, T, NA, F),
    useForCalibration = c(T, T, NA, T)
  )
  df_2 <- tibble(
    `Identifier 1` = c("xyz", "STD_B", "Probe", "STD_A"),
    val = 1:4
  )
  dfExpected_2 <- tibble(
    `Identifier 1` = c("xyz", "STD_B", "Probe", "STD_A"),
    val = 1:4,
    O18_True = c(NA, 1.8, NA, -2),
    H2_True = c(NA, -0.4, NA, 3),
    useForMemCorr = c(NA, NA, NA, T),
    useForDriftCorr = c(NA, T, NA, F),
    useForCalibration = c(NA, T, NA, T)
  )
  
  dfActual <- associateStandardsWithConfigInfo(list(df_1, df_2), config)
  
  expect_length(dfActual, 2)
  expect_true(all_equal(dfActual[[1]], dfExpected_1))
  expect_true(all_equal(dfActual[[2]], dfExpected_2))
})
