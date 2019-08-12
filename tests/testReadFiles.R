library(testthat)
library(dplyr)
library(readr)
source("../R/readFiles.R")

context("Test data reading")

test_that("test number of files read is correct", {
  data <- readFiles(list(FILE_ID = "\\.csv"), folder = "test_data")
  expect_length(data, 3)
})

test_that("test content of files read is correct", {
  data <- readFiles(list(FILE_ID = "\\.csv"), folder = "test_data")
  expect_true(all_equal(data$HIDS2041_IsoWater_20151125_111138.csv, read_csv("test_data/HIDS2041_IsoWater_20151125_111138.csv")))
  expect_true(all_equal(data$HIDS2041_IsoWater_20151126_115726.csv, read_csv("test_data/HIDS2041_IsoWater_20151126_115726.csv")))
  expect_true(all_equal(data$HIDS2041_IsoWater_20151127_143940.csv, read_csv("test_data/HIDS2041_IsoWater_20151127_143940.csv")))
})