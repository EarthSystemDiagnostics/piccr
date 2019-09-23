library(testthat)
library(readr)
library(stringr)

context("Test writing data to file (csv output)")

d1Content <- "Identifier 1,a,b\na,1.2,3.1\n"
d2Content <- "Identifier 1,c,d\na,3.1,\nb,9,-2\n"

config <- list(output_directory = tempdir(), include_standards_in_output = TRUE)
datasets <- list(
  list(name = "d1.csv", processed = read_csv(d1Content)), 
  list(name = "d2.csv", processed = read_csv(d2Content))
)

test_that("test correct files are created (output dir exists)", {
  writeDataToFile(datasets, config)
  
  expect_true(file.exists(file.path(tempdir(), "d1.csv")))
  expect_true(file.exists(file.path(tempdir(), "d2.csv")))
})

test_that("test correct files are created (output dir does not exists)", {
  
  # make sure that the directory does not exist
  unlink("some_dir_that_does_not_exist", recursive = TRUE)
  stopifnot(!dir.exists("some_dir_that_does_not_exist"))
  
  config <- list(output_directory = "some_dir_that_does_not_exist", include_standards_in_output = TRUE)
  
  writeDataToFile(datasets, config)
  
  expect_true(file.exists(file.path("some_dir_that_does_not_exist", "d1.csv")))
  expect_true(file.exists(file.path("some_dir_that_does_not_exist", "d2.csv")))
  
  unlink("some_dir_that_does_not_exist", recursive = TRUE)
})

test_that("test file contents are correct", {
  writeDataToFile(datasets, config)
  
  expect_identical(read_file(file.path(tempdir(), "d1.csv")),
                   d1Content)
  expect_identical(read_file(file.path(tempdir(), "d2.csv")),
                   d2Content)
})

test_that("test don't include standards in output (case standard in file)", {
  config <- list(output_directory = tempdir(), 
                 include_standards_in_output = FALSE,
                 standards = list(list(name = "a"), list(name = "c")))
  
  writeDataToFile(datasets, config)
  
  expect_identical(read_file(file.path(tempdir(), "d1.csv")),
                   "Identifier 1,a,b\n")
  
  expect_identical(read_file(file.path(tempdir(), "d2.csv")),
                   "Identifier 1,c,d\nb,9,-2\n")
})

test_that("test don't include standards in output (case only probes in file)", {
  config <- list(output_directory = tempdir(), 
                 include_standards_in_output = FALSE,
                 standards = list(list(name = "c")))
  
  writeDataToFile(datasets, config)
  
  expect_identical(read_file(file.path(tempdir(), "d1.csv")),
                   d1Content)
  
  expect_identical(read_file(file.path(tempdir(), "d2.csv")),
                   d2Content)
})

test_that("test don't include standards in output (case standard list empty)", {
  config <- list(output_directory = tempdir(), 
                 include_standards_in_output = FALSE,
                 standards = list())
  
  writeDataToFile(datasets, config)
  
  expect_identical(read_file(file.path(tempdir(), "d1.csv")),
                   d1Content)
  
  expect_identical(read_file(file.path(tempdir(), "d2.csv")),
                   d2Content)
})