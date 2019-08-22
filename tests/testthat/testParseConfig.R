library(testthat)

context("Test parse config")

# test fails covr::package_coverage() for unknown reasons
# TODO: fix covr issue
test_that("Config parsed correctly", {
  config <- parseConfig(system.file("extdata", "config.yaml", package = "piccr"))
  expect_type(config, "list")
  expect_length(config, 9)
})

test_that("Error is thrown on incorrect path", {
  expect_error(parseConfig("some_file_that_does_not_exist.yaml"))
  expect_error(parseConfig("some_folder_that_does_not_exist/some_file_that_does_not_exist.yaml"))
})