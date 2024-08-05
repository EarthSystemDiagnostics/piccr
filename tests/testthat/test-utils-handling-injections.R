test_that("vial count of very first standard in file is correct", {

  dataset1 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~`Inj Nr`, ~Sample, ~vial_group,
    # ---/ ---------------/ ---------------/ ---------/ -------/ -----------/
    1,     "A",             "x",             1,         1,       1,
    2,     "A",             "x",             2,         1,       1,
    3,     "A",             "x",             3,         1,       1,
    4,     "B",             "y",             1,         2,       1,
    5,     "B",             "y",             2,         2,       1,
    6,     "B",             "y",             3,         2,       1,
    7,     "C",             "z",             1,         3,       1,
    8,     "C",             "z",             2,         3,       1,
    9,     "C",             "z",             3,         3,       1
  )
  dataset2 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~`Inj Nr`, ~Sample, ~vial_group,
    # ---/ ---------------/ ---------------/ ---------/ -------/ -----------/
    1,     "A",             "x",             1,         1,       1,
    2,     "A",             "x",             2,         1,       1,
    3,     "A",             "x",             3,         1,       1,
    4,     "A",             "x",             1,         2,       1,
    5,     "A",             "x",             2,         2,       1,
    6,     "A",             "x",             3,         2,       1,
    7,     "B",             "z",             1,         3,       1,
    8,     "B",             "z",             2,         3,       1,
    9,     "B",             "z",             3,         3,       1,
    10,    "A",             "x",             1,         4,       2,
    11,    "A",             "x",             2,         4,       2,
    12,    "A",             "x",             3,         4,       2,
    13,    "C",             "z",             1,         5,       1,
    14,    "C",             "z",             2,         5,       1,
    15,    "C",             "z",             3,         5,       1,
    16,    "C",             "z",             1,         6,       2,
    17,    "C",             "z",             2,         6,       2,
    18,    "C",             "z",             3,         6,       2,
    19,    "D",             "f",             1,         7,       1,
    20,    "D",             "f",             2,         7,       1,
    21,    "D",             "f",             3,         7,       1
  )

  actual1 <- getVialCountOfFirstStd(dataset1)
  actual2 <- getVialCountOfFirstStd(dataset2)

  expect_equal(actual1, 1)
  expect_equal(actual2, 2)

})

test_that("vial grouping works as expected", {

  dataset1 <- tibble::tribble(
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

  expected1 <- tibble::tribble(
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

  dataset2 <- tibble::tribble(
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

  expected2 <- tibble::tribble(
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

test_that("vial grouping yields proper injection numbers", {

  config <- list(standards = list(list(name = "STD_A"),
                                  list(name = "STD_B"),
                                  list(name = "STD_C")))

  dataset <- tibble::tribble(
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

  expected <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~block, ~vial_group,
    # -- / -------------- / -------- / -------------- / -------------/ ------/ ------------
    1,     "STD_A",         1L,        1,               1,             1L,     1,
    2,     "STD_A",         2L,        1,               1,             1L,     1,
    3,     "STD_B",         1L,        2,               2,             1L,     1,
    4,     "STD_B",         2L,        2,               2,             1L,     1,
    5,     "STD_C",         1L,        3,               3,             1L,     1,
    6,     "STD_C",         2L,        3,               3,             1L,     1,
    7,     "STD_A",         1L,        1,               1,             1L,     2,
    8,     "STD_A",         2L,        1,               1,             1L,     2,
    9,     "STD_A",         3L,        1,               1,             1L,     2,
    10,    "STD_A",         4L,        1,               1,             1L,     2,
    11,    "PROBE_A",       1L,        10,              10,            NA,     1,
    12,    "PROBE_B",       1L,        20,              20,            NA,     1,
    13,    "PROBE_A",       1L,        10,              10,            NA,     2,
    14,    "STD_B",         1L,        2,               2,             2L,     2,
    15,    "STD_B",         2L,        2,               2,             2L,     2,
    16,    "STD_A",         1L,        1,               1,             2L,     3,
    17,    "STD_A",         2L,        1,               1,             2L,     3
  )

  actual <- dataset %>%
    groupStandardsInBlocks(config) %>%
    assignVialsToGroups() %>%
    normalizeInjectionNumbers()

  expect_equal(actual, expected)

})

test_that("vial grouping workds for true delta and true previous delta", {

  dataset1 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~vial_group,
    # -- / -------------- / -------- / -------------- / -------------/ ------------
    1,     "STD_A",         1,         1,               10,            1,
    2,     "STD_A",         2,         2,               20,            1,
    3,     "STD_A",         3,         3,               30,            1,
    4,     "STD_B",         1,         2,               20,            1,
    5,     "STD_B",         2,         3,               30,            1,
    6,     "STD_B",         3,         4,               40,            1,
    7,     "STD_C",         1,         3,               30,            1,
    8,     "STD_C",         2,         4,               40,            1,
    9,     "STD_C",         3,         5,               50,            1
  )

  expected1 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~vial_group, ~deltaTrueD18O, ~deltaTrueDD, ~deltaTruePrevD18O, ~deltaTruePrevDD,
    # -- / -------------- / -------- / -------------- / -------------/ -----------/ --------------/ ------------/ ------------------/ -----------------
    1,     "STD_A",         1,         1,               10,            1,           2,              20,           NA,                 NA,
    2,     "STD_A",         2,         2,               20,            1,           2,              20,           NA,                 NA,
    3,     "STD_A",         3,         3,               30,            1,           2,              20,           NA,                 NA,
    4,     "STD_B",         1,         2,               20,            1,           3,              30,           2,                  20,
    5,     "STD_B",         2,         3,               30,            1,           3,              30,           2,                  20,
    6,     "STD_B",         3,         4,               40,            1,           3,              30,           2,                  20,
    7,     "STD_C",         1,         3,               30,            1,           4,              40,           3,                  30,
    8,     "STD_C",         2,         4,               40,            1,           4,              40,           3,                  30,
    9,     "STD_C",         3,         5,               50,            1,           4,              40,           3,                  30
  )

  dataset2 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~vial_group,
    # -- / -------------- / -------- / -------------- / -------------/ ------------
    1,     "STD_A",         1,         1,               10,            1,
    2,     "STD_A",         2,         2,               20,            1,
    3,     "STD_A",         3,         3,               30,            1,
    4,     "STD_B",         1,         2,               20,            1,
    5,     "STD_B",         2,         3,               30,            1,
    6,     "STD_B",         3,         4,               40,            1,
    7,     "STD_A",         1,         1,               10,            2,
    8,     "STD_A",         2,         2,               20,            2,
    9,     "STD_A",         3,         3,               30,            2,
    10,    "STD_C",         1,         3,               30,            1,
    11,    "STD_C",         2,         4,               40,            1,
    12,    "STD_C",         3,         5,               50,            1
  )

  expected2 <- tibble::tribble(
    ~Line, ~`Identifier 1`, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~vial_group, ~deltaTrueD18O, ~deltaTrueDD, ~deltaTruePrevD18O, ~deltaTruePrevDD,
    # -- / -------------- / -------- / -------------- / -------------/ -----------/ --------------/ ------------/ ------------------/ -----------------
    1,     "STD_A",         1,         1,               10,            1,           2,              20,           NA,                 NA,
    2,     "STD_A",         2,         2,               20,            1,           2,              20,           NA,                 NA,
    3,     "STD_A",         3,         3,               30,            1,           2,              20,           NA,                 NA,
    4,     "STD_B",         1,         2,               20,            1,           3,              30,           2,                  20,
    5,     "STD_B",         2,         3,               30,            1,           3,              30,           2,                  20,
    6,     "STD_B",         3,         4,               40,            1,           3,              30,           2,                  20,
    7,     "STD_A",         1,         1,               10,            2,           2,              20,           3,                  30,
    8,     "STD_A",         2,         2,               20,            2,           2,              20,           3,                  30,
    9,     "STD_A",         3,         3,               30,            2,           2,              20,           3,                  30,
    10,    "STD_C",         1,         3,               30,            1,           4,              40,           2,                  20,
    11,    "STD_C",         2,         4,               40,            1,           4,              40,           2,                  20,
    12,    "STD_C",         3,         5,               50,            1,           4,              40,           2,                  20
  )

  actual1 <- getDeltaTrueAndDeltaTruePrevForEachSample(dataset1, `Identifier 1`, `vial_group`)
  actual2 <- getDeltaTrueAndDeltaTruePrevForEachSample(dataset2, `Identifier 1`, `vial_group`)

  expect_equal(actual1, expected1)
  expect_equal(actual2, expected2)

})
