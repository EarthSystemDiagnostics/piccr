library(testthat)
library(tidyverse)
source("../R/associateStandardsWithTrueValues.R")

context("Test associating standards with their true values")

config <- list(STANDARDS = list(list(NAME = "STD_A", O18_VAL = -2, H2_VAL = 3), 
                                list(NAME = "STD_B", O18_VAL = 1.8, H2_VAL = -0.4)))

test_that("associating standards for single row (standard)", {
  df <- tibble(
    `Identifier 1` = "STD_A",
    val = 1
  )
  dfExpected <- tibble(
    `Identifier 1` = "STD_A",
    val = 1,
    O18_True = -2,
    H2_True = 3
  )
  
  dfActual <- associateStandardsWithTrueValuesForDataset(df, config)
  
  expect_true(all_equal(dfActual, dfExpected))
})

test_that("associating standards for multiple rows (standards)", {
  df <- tibble(
    `Identifier 1` = c("STD_A", "STD_B"),
    val = 1:2
  )
  dfExpected <- tibble(
    `Identifier 1` = c("STD_A", "STD_B"),
    val = 1:2,
    O18_True = c(-2, 1.8),
    H2_True = c(3, -0.4)
  )
  
  dfActual <- associateStandardsWithTrueValuesForDataset(df, config)
  
  expect_true(all_equal(dfActual, dfExpected))
})

test_that("associating standards for multiple rows (standards and probes)", {
  df <- tibble(
    `Identifier 1` = c("STD_A", "STD_B", "Probe", "STD_A"),
    val = 1:4
  )
  dfExpected <- tibble(
    `Identifier 1` = c("STD_A", "STD_B", "Probe", "STD_A"),
    val = 1:4,
    O18_True = c(-2, 1.8, NA, -2),
    H2_True = c(3, -0.4, NA, 3)
  )
  
  dfActual <- associateStandardsWithTrueValuesForDataset(df, config)
  
  expect_true(all_equal(dfActual, dfExpected))
})

test_that("associating standards for multiple datasets", {
  df_1 <- tibble(
    `Identifier 1` = c("STD_A", "STD_B", "Probe", "STD_A"),
    val = 1:4
  )
  dfExpected_1 <- tibble(
    `Identifier 1` = c("STD_A", "STD_B", "Probe", "STD_A"),
    val = 1:4,
    O18_True = c(-2, 1.8, NA, -2),
    H2_True = c(3, -0.4, NA, 3)
  )
  df_2 <- tibble(
    `Identifier 1` = c("xyz", "STD_B", "Probe", "STD_A"),
    val = 1:4
  )
  dfExpected_2 <- tibble(
    `Identifier 1` = c("xyz", "STD_B", "Probe", "STD_A"),
    val = 1:4,
    O18_True = c(NA, 1.8, NA, -2),
    H2_True = c(NA, -0.4, NA, 3)
  )
  
  dfActual <- associateStandardsWithTrueValues(list(df_1, df_2), config)
  
  expect_length(dfActual, 2)
  expect_true(all_equal(dfActual[[1]], dfExpected_1))
  expect_true(all_equal(dfActual[[2]], dfExpected_2))
})