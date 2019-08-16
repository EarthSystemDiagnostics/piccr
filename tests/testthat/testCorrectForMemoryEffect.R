library(testthat)
library(tidyverse)

context("Test the memory correction logic")

test_that("test memory correction (dataset contains only block 1 standards)", {
  
  # in this dataset: m1 = 0.5, m2 = 0.75, m3 = 1
  dataset <- tribble(
    ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block,
    # ------------ / -------- / -------------- / -------
      "A",           1,         0.5,             1,
      "A",           2,         0.75,            1,
      "A",           3,         1,               1,
      "B",           1,         1.5,             1,
      "B",           2,         1.75,            1,
      "B",           3,         2,               1,
      "C",           1,         2.5,             1,
      "C",           2,         2.75,            1,
      "C",           3,         3,               1
  )
  
  expected <- tribble(
    ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block,
    # ------------ / -------- / -------------- / -------
    "A",           1,         1,               1,
    "A",           2,         1,               1,
    "A",           3,         1,               1,
    "B",           1,         2,               1,
    "B",           2,         2,               1,
    "B",           3,         2,               1,
    "C",           1,         3,               1,
    "C",           2,         3,               1,
    "C",           3,         3,               1
  )
  
  actual <- correctSingleDatasetForMemoryEffect(dataset)
  
  expect_true(all_equal(expected, actual))
})

test_that("test memory coefficient calculation (dataset contains other blocks and probes)", {
  
  # in this dataset: m1 = 0.5, m2 = 1
  dataset <- tribble(
    ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block,
    # ------------ / -------- / -------------- / -------
    "Probe1",        1,         10,              NA,
    "Probe1",        2,         10,              NA,
    "A",             1,         5.5,             1,
    "A",             2,         1,               1,
    "B",             1,         1.5,             1,
    "B",             2,         2,               1,
    "C",             1,         2.5,             1,
    "C",             2,         3,               1,
    "Probe2",        1,         6.5,             NA,
    "Probe2",        2,         10,              NA,
    "A",             1,         15,              2,
    "A",             2,         20,              2,
    "B",             1,         10,              2,
    "B",             2,         0,               2,
    "Probe3",        1,         10,              NA
  )
  
  expected <- tribble(
    ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~block,
    # ------------ / -------- / -------------- / -------
    "Probe1",        1,         10,              NA,
    "Probe1",        2,         10,              NA,
    "A",             1,         1,               1,
    "A",             2,         1,               1,
    "B",             1,         2,               1,
    "B",             2,         2,               1,
    "C",             1,         3,               1,
    "C",             2,         3,               1,
    "Probe2",        1,         10,              NA,
    "Probe2",        2,         10,              NA,
    "A",             1,         20,              2,
    "A",             2,         20,              2,
    "B",             1,         0,               2,
    "B",             2,         0,               2,
    "Probe3",        1,         20,              NA
  )
  
  actual <- correctSingleDatasetForMemoryEffect(dataset)
  
  expect_true(all_equal(expected, actual))
})