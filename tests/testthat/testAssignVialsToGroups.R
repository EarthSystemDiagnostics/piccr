library(testthat)
library(tidyverse)

context("Test assigning vials into groups")

config <- list(standards = list(list(name = "STD_A"),
                                list(name = "STD_B"),
                                list(name = "STD_C")))

test_that("test that grouping works as expected", {

  dataset1 <- tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`,
    # -- / -------------- / -------- / -------------- / -------------
    1,     "STD_A",         1,         1,               1,
    2,     "STD_A",         2,         1,               1,
    3,     "STD_B",         1,         2,               2,
    4,     "STD_B",         2,         2,               2,
    5,     "PROBE_A",       1,         10,              10,
    6,     "PROBE_B",       1,         20,              20,
    7,     "STD_C",         1,         3,               3,
    8,     "STD_C",         2,         3,               3,
    9,     "PROBE_C",       1,         30,              30
  )

  expected1 <- tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~vial_group,
    # -- / -------------- / -------- / -------------- / -------------/ ------------
    1,     "STD_A",         1,         1,               1,              1,
    2,     "STD_A",         2,         1,               1,              1,
    3,     "STD_B",         1,         2,               2,              1,
    4,     "STD_B",         2,         2,               2,              1,
    5,     "PROBE_A",       1,         10,              10,             1,
    6,     "PROBE_B",       1,         20,              20,             1,
    7,     "STD_C",         1,         3,               3,              1,
    8,     "STD_C",         2,         3,               3,              1,
    9,     "PROBE_C",       1,         30,              30,             1
  )

  dataset2 <- tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`,
    # -- / -------------- / -------- / -------------- / -------------
    1,     "STD_A",         1,         1,               1,
    2,     "STD_A",         2,         1,               1,
    3,     "STD_B",         1,         2,               2,
    4,     "STD_B",         2,         2,               2,
    5,     "STD_A",         1,         1,               1,
    6,     "STD_A",         2,         1,               1,
    7,     "PROBE_A",       1,         10,              10,
    8,     "PROBE_B",       1,         20,              20,
    9,     "STD_C",         1,         3,               3,
    10,    "STD_C",         2,         3,               3,
    11,    "PROBE_C",       1,         30,              30,
    12,    "STD_C",         1,         3,               3,
    13,    "STD_C",         2,         3,               3,
  )

  expected2 <- tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~vial_group,
    # -- / -------------- / -------- / -------------- / -------------/ ------------
    1,     "STD_A",         1,         1,               1,             1,
    2,     "STD_A",         2,         1,               1,             1,
    3,     "STD_B",         1,         2,               2,             1,
    4,     "STD_B",         2,         2,               2,             1,
    5,     "STD_A",         1,         1,               1,             2,
    6,     "STD_A",         2,         1,               1,             2,
    7,     "PROBE_A",       1,         10,              10,            1,
    8,     "PROBE_B",       1,         20,              20,            1,
    9,     "STD_C",         1,         3,               3,             1,
    10,    "STD_C",         2,         3,               3,             1,
    11,    "PROBE_C",       1,         30,              30,            1,
    12,    "STD_C",         1,         3,               3,             2,
    13,    "STD_C",         2,         3,               3,             2
  )
  
  actual1 <- dataset1 %>%
    assignVialsToGroups()
  actual2 <- dataset2 %>%
    assignVialsToGroups()

  expect_equal(actual1, expected1)
  expect_equal(actual2, expected2)

})

test_that("test that vial grouping yields proper injection numbers", {

  dataset <- tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`,
    # -- / -------------- / -------- / -------------- / -------------
    1,     "STD_A",         1,         1,               1,
    2,     "STD_A",         2,         1,               1,
    3,     "STD_B",         1,         2,               2,
    4,     "STD_B",         2,         2,               2,
    5,     "STD_C",         1,         3,               3,
    6,     "STD_C",         2,         3,               3,
    7,     "STD_A",         1,         1,               1,
    8,     "STD_A",         2,         1,               1,
    9,     "STD_A",         1,         1,               1,
    10,    "STD_A",         2,         1,               1,
    11,    "PROBE_A",       1,         10,              10,
    12,    "PROBE_B",       1,         20,              20,
    13,    "PROBE_A",       1,         10,              10,
    14,    "STD_B",         1,         2,               2,
    15,    "STD_B",         2,         2,               2,
    16,    "STD_A",         1,         1,               1,
    17,    "STD_A",         2,         1,               1
  )

  expected <- tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~block, ~vial_group,
    # -- / -------------- / -------- / -------------- / -------------/ ------/ ------------
    1,     "STD_A",         1,         1,               1,             1,      1,
    2,     "STD_A",         2,         1,               1,             1,      1,
    3,     "STD_B",         1,         2,               2,             1,      1,
    4,     "STD_B",         2,         2,               2,             1,      1,
    5,     "STD_C",         1,         3,               3,             1,      1,
    6,     "STD_C",         2,         3,               3,             1,      1,
    7,     "STD_A",         1,         1,               1,             1,      2,
    8,     "STD_A",         2,         1,               1,             1,      2,
    9,     "STD_A",         3,         1,               1,             1,      2,
    10,    "STD_A",         4,         1,               1,             1,      2,
    11,    "PROBE_A",       1,         10,              10,            NA,     1,
    12,    "PROBE_B",       1,         20,              20,            NA,     1,
    13,    "PROBE_A",       1,         10,              10,            NA,     2,
    14,    "STD_B",         1,         2,               2,             2,      2,
    15,    "STD_B",         2,         2,               2,             2,      2,
    16,    "STD_A",         1,         1,               1,             2,      3,
    17,    "STD_A",         2,         1,               1,             2,      3
  )

  expected$`Inj Nr` <- as.integer(expected$`Inj Nr`)

  actual <- dataset %>%
    groupStandardsInBlocks(config) %>%
    assignVialsToGroups() %>%
    normalizeInjectionNumbers()

  expect_equal(actual, expected)

})
