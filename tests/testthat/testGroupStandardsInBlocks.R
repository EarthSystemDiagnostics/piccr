library(testthat)
library(tidyverse)

context("Test grouping standards in blocks")

config <- list(standards = list(list(name = "STD_A"), list(name = "STD_B")))

test_that("test for dataframe with single row (contains standard)", {
  df <- tibble(
    `Identifier 1` = "STD_A",
    val = 3
  )
  dfWithGroupedStandardsExpected <- tibble(
    `Identifier 1` = "STD_A",
    val = 3,
    block = 1
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocksForDataset(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)
})

test_that("test for dataframe with single row (does not contain standard)", {
  df <- tibble(
    `Identifier 1` = "Probe",
    val = 3
  )
  dfWithGroupedStandardsExpected <- tibble(
    `Identifier 1` = "Probe",
    val = 3,
    block = NA
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocksForDataset(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)
})

test_that("test for dataframe with mulitple rows (first standard, then probe)", {
  df <- tibble(
    `Identifier 1` = c("STD_A", "Probe"),
    val = c(3, 7)
  )
  dfWithGroupedStandardsExpected <- tibble(
    `Identifier 1` = c("STD_A", "Probe"),
    val = c(3, 7),
    block = c(1, NA)
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocksForDataset(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)
})

test_that("test for dataframe with mulitple rows (probe, standard, probe)", {
  df <- tibble(
    `Identifier 1` = c("Probe", "STD_A", "Probe"),
    val = c(1, 3, 7)
  )
  dfWithGroupedStandardsExpected <- tibble(
    `Identifier 1` = c("Probe", "STD_A", "Probe"),
    val = c(1, 3, 7),
    block = c(NA, 1, NA)
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocksForDataset(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)
})

test_that("test for dataframe with mulitple rows (standard, probe, standard)", {
  df <- tibble(
    `Identifier 1` = c("STD_A", "Probe", "STD_B"),
    val = c(1, 3, 7)
  )
  dfWithGroupedStandardsExpected <- tibble(
    `Identifier 1` = c("STD_A", "Probe", "STD_B"),
    val = c(1, 3, 7),
    block = c(1, NA, 2)
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocksForDataset(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)
})

test_that("test for list of dataframes with mulitple rows (standard, probe, standard, probe, standard)", {
  df <- tibble(
    `Identifier 1` = c("STD_A", "Probe", "Probe", "STD_B", "STD_A", "Probe", "STD_A"),
    val = 1:7
  )
  dfWithGroupedStandardsExpected <- tibble(
    `Identifier 1` = c("STD_A", "Probe", "Probe", "STD_B", "STD_A", "Probe", "STD_A"),
    val = 1:7,
    block = c(1, NA, NA, 2, 2, NA, 3)
  )
  dfWithGroupedStandardsActual <- groupStandardsInBlocksForDataset(df, config)
  
  expect_equal(dfWithGroupedStandardsExpected, dfWithGroupedStandardsActual)
})

test_that("test for list of dataframes", {
  df_1<- tibble(
    `Identifier 1` = c("STD_A", "Probe", "Probe", "STD_B", "STD_A", "Probe", "STD_A"),
    val = 1:7
  )
  dfWithGroupedStandardsExpected_1 <- tibble(
    `Identifier 1` = c("STD_A", "Probe", "Probe", "STD_B", "STD_A", "Probe", "STD_A"),
    val = 1:7,
    block = c(1, NA, NA, 2, 2, NA, 3)
  )
  df_2 <- tibble(
    `Identifier 1` = c("STD_A", "Probe", "STD_B"),
    val = c(1, 3, 7)
  )
  dfWithGroupedStandardsExpected_2 <- tibble(
    `Identifier 1` = c("STD_A", "Probe", "STD_B"),
    val = c(1, 3, 7),
    block = c(1, NA, 2)
  )
  
  dfsWithGroupedStandardsActual <- groupStandardsInBlocks(list(df_1, df_2), config)
  
  expect_equal(dfWithGroupedStandardsExpected_1, dfsWithGroupedStandardsActual[[1]])
  expect_equal(dfWithGroupedStandardsExpected_2, dfsWithGroupedStandardsActual[[2]])
  expect_length(dfsWithGroupedStandardsActual, 2)
})