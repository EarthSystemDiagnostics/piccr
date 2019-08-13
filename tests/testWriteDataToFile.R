library(testthat)
library(readr)
library(stringr)

source("../R/helpers/writeDataToFile.R")


context("Test writing data to file (csv output)")

config <- list(output_directory = tempdir())
datasets <- list(processed = list(d1.csv = read_csv("a,b\n1.2,3.1"), 
                                  d2.csv = read_csv("c,d\n3.1,\n9,-2")))

test_that("test correct files are created (output dir exists)", {
  writeDataToFile(datasets, config)
  
  expect_true(file.exists(file.path(tempdir(), "d1.csv")))
  expect_true(file.exists(file.path(tempdir(), "d2.csv")))
})

test_that("test correct files are created (output dir does not exists)", {
  
  # make sure that the directory does not exist
  unlink("some_dir_that_does_not_exist", recursive = TRUE)
  stopifnot(!dir.exists("some_dir_that_does_not_exist"))
  
  config <- list(output_directory = "some_dir_that_does_not_exist")
  
  writeDataToFile(datasets, config)
  
  expect_true(file.exists(file.path("some_dir_that_does_not_exist", "d1.csv")))
  expect_true(file.exists(file.path("some_dir_that_does_not_exist", "d2.csv")))
  
  unlink("some_dir_that_does_not_exist", recursive = TRUE)
})

test_that("test file contents are correct", {
  writeDataToFile(datasets, config)
  
  expect_identical(read_file(file.path(tempdir(), "d1.csv")),
                   "a,b\n1.2,3.1\n")
  expect_identical(read_file(file.path(tempdir(), "d2.csv")),
                   "c,d\n3.1,\n9,-2\n")
})