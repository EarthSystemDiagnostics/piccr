library(testthat)
library(dplyr)
library(readr)

context("Test data reading")

config <- list(input_directory = "test_data", file_extension = ".csv")

test_that("test number of files read is correct", {
  data <- readFiles(config)
  
  expect_length(data, 3)
})

test_that("test content of files read is correct", {
  data <- readFiles(config)
  
  expect_equal(data$HIDS2041_IsoWater_20151125_111138.csv, 
                        read_csv("test_data/no_spaces/HIDS2041_IsoWater_20151125_111138.csv"))
  expect_equal(data$HIDS2041_IsoWater_20151126_115726.csv, 
                        read_csv("test_data/no_spaces/HIDS2041_IsoWater_20151126_115726.csv"))
  expect_equal(data$HIDS2041_IsoWater_20151127_143940.csv, 
                        read_csv("test_data/no_spaces/HIDS2041_IsoWater_20151127_143940.csv"))
})