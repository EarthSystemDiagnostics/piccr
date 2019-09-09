library(testthat)
library(tidyverse)

context("test creating and writing a summary file with quality control information")

test_that("test", {
  
  # ------------ INITIALIZE INPUTS -------------
  
  df1 <- tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~block, ~delta.O18, ~delta.H2,
    # -- / -------------- / -------------- / ----- / --------- / -------- 
    1,     "C",             "x",             1,      4,          10,
    2,     "A",             "y",             NA,     12,        -2,
    3,     "B",             "z",             2,      1,         -8,
    4,     "C",             "x",             3,      -6,         1
  )
  df2 <- tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~block, ~delta.O18, ~delta.H2,
    # -- / -------------- / -------------- / ----- / --------- / -------- 
    1,     "C",             "x",             1,      2,          20,
    2,     "A",             "y",             NA,     4.5,        0,
    3,     "B",             "z",             2,      10,         -5,
    4,     "C",             "x",             2,      -3,         0
  )
  processedData <- list(
    processed = list(
      df1 = df1, df2 = df2
    ),
    pooledStdDev = list(
      df1 = list(d18O = 3, dD = 4.7),
      df2 = list(d18O = 0.0, dD = -20)
    )
  )
  config <- list(
    standards = list(
      list(name = "C", o18_True = 4, H2_True = 10),
      list(name = "B", o18_True = 0, H2_True = -4)
    )
  )
  file <- tempfile()
  
  # ----------- CALL FUNCTION UNDER TEST ----------
  
  outputSummaryFile(processedData, config, file)
  
  # ----------- MAKE EXPECTATIONS --------------
  
  actual <- read_file(file)
  expected <- str_c(
    "### AVERAGE OVER ALL FILES: ###\n\n",
    "pooled standard deviation delta O18: 1.50\n",
    "pooled standard deviation delta H2: -7.65\n\n",
    "### VALUES FOR EACH FILE: ###\n\n",
    "file name, pooled sd delta O18, pooled sd delta H2\n",
    "df1, 3.00, 4.70\n",
    "df2, 0.00, -20.00\n\n",
    "### INTER STANDARD BIAS TO LITERATURE VALUES FOR EACH FILE: ###\n\n",
    "file name, standard, block, bias O18, bias H2\n",
    "df1, C, 1, 0.00, 0.00\n",
    "df1, B, 2, 1.00, -4.00\n",
    "df1, C, 3, -10.00, -9.00\n",
    "df2, C, 1, -2.00, 10.00\n",
    "df2, B, 2, 10.00, -1.00\n",
    "df2, C, 2, -7.00, -10.00"
  )
  
  expect_equal(actual, expected)
})