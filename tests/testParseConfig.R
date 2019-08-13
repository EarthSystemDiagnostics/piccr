library(testthat)

context("Test parse config")

test_that("Config parsed correctly", {
  config <- parseConfig("test_data/config.yaml")
  expect_type(config, "list")
  expect_length(config, 10)
})

test_that("Error is thrown on incorrect path", {
  expect_error(parseConfig("some_file_that_does_not_exist.yaml"))
})