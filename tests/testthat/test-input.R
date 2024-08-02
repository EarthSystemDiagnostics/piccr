test_that("config parsed correctly", {

  config <- parseConfig(system.file("extdata", "config.yaml", package = "piccr"))

  expect_type(config, "list")
  expect_length(config, 10)

})

test_that("error is thrown on incorrect path", {

  expect_error(parseConfig("some_file_that_does_not_exist.yaml"))
  expect_error(parseConfig("some_folder_that_does_not_exist/some_file_that_does_not_exist.yaml"))

})

test_that("number of files read is correct", {

  config <- list(input_directory = "test_data", file_extension = ".csv")
  data <- readFiles(config)
  
  expect_length(data, 3)

})

test_that("content of files read is correct", {

  config <- list(input_directory = "test_data", file_extension = ".csv")
  data <- readFiles(config)
  
  expect_equal(data$HIDS2041_IsoWater_20151125_111138.csv, 
                        readr::read_csv("test_data/no_spaces/HIDS2041_IsoWater_20151125_111138.csv"))
  expect_equal(data$HIDS2041_IsoWater_20151126_115726.csv, 
                        readr::read_csv("test_data/no_spaces/HIDS2041_IsoWater_20151126_115726.csv"))
  expect_equal(data$HIDS2041_IsoWater_20151127_143940.csv, 
                        readr::read_csv("test_data/no_spaces/HIDS2041_IsoWater_20151127_143940.csv"))

})
