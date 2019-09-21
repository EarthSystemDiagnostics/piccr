library(testthat)
library(tidyverse)

context("test ProcessDataForOutput")

test_that("test output structure", {

  dataset1 <- tribble(
    ~Line, ~`Identifier 1`, ~`Identifier 2`, ~block, ~`Inj Nr`, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~dExcess, ~o18_True, ~H2_True, ~useAsControlStandard,
    # -- / -------------- / -------------- / ----- / -------- / -------------- / ------------ / --------/ ---------/ --------/ ---------------------
    1,     "WU",            "w",             1,      1,         0.9,             8.5,           15,       1,         10,       FALSE,
    2,     "WU",            "w",             1,      2,         1,               9,             20,       1,         10,       FALSE,
    3,     "WU",            "w",             1,      3,         1.1,             10.8,          25,       1,         10,       FALSE,
    1,     "C",             "x",             1,      1,         1.9,             19,            15,       2,         20,       FALSE,
    2,     "C",             "x",             1,      2,         2.1,             20.7,          20,       2,         20,       FALSE,
    3,     "C",             "x",             1,      3,         2,               22.1,          25,       2,         20,       FALSE,
    4,     "probe1",        "p",             NA,     1,         4,               49,            4,        5,         50,       FALSE,
    5,     "probe1",        "p",             NA,     2,         5,               49,            5,        5,         50,       FALSE,
    6,     "QC",            "qq",            2,      1,         10.4,            95,            11,       10,        100,      TRUE,
    7,     "QC",            "qq",            2,      2,         9.8,             102.5,         9,        10,        100,      TRUE,
    4,     "probe2",        "pp",            NA,     1,         6,               60,            4,        7,         70,       FALSE,
    5,     "probe2",        "pp",            NA,     2,         7,               71,            5,        7,         70,       FALSE,
    6,     "B",             "z",             3,      1,         2.8,             28.5,          11,       3,         30,       FALSE,
    7,     "B",             "z",             3,      2,         3.2,             31,            9,        3,         30,       FALSE,
    8,     "C",             "x",             3,      1,         -5.2,            -48.7,         -2,       -5,        -50,      FALSE,
    9,     "C",             "x",             3,      2,         -5.3,            -50.4,         2,        -5,        -50,      FALSE
  )

  actual <- processDataForOutput(list(df1 = dataset1),
                                 list(average_over_last_n_inj = "all"))

  expect_length(actual, 1)
  
})
