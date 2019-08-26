library(testthat)
library(readr)
library(yaml)

context("test processData")

test_that("check that no NAs were introduced", {
  
  configPath <- system.file("extdata", "config.yaml", package = "piccr")
  config <- yaml::read_yaml(configPath)
  
  datasets <- list(
    HIDS2041_IsoWater_20151126_115726.csv = read_csv("test_data/HIDS2041_IsoWater_20151126_115726.csv"),
    HIDS2041_IsoWater_20151125_111138.csv = read_csv("test_data/HIDS2041_IsoWater_20151125_111138.csv"),  
    HIDS2041_IsoWater_20151127_143940.csv = read_csv("test_data/HIDS2041_IsoWater_20151127_143940.csv")
  )
  
  actual <- processData(datasets, config)
  
  expect_length(actual, 4)
  expect_length(actual$processed, 3)
  
  for (dataset in actual$processed) {
    expect_equal(sum(is.na(dataset$`delta.O18`)), 1)
    expect_equal(sum(is.na(dataset$`delta.H2`)), 1)
  }
  
})