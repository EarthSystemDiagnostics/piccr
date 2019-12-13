library(tibble)

context("test calculatePooledStdDev")

test_that("test calculatePooledStdDev", {
  
  dataset1 <- tibble::tribble(
    ~`Identifier 1`, ~block, ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~vial_group,
    # ------------ / ----- / -------------- / ------------ / -----------
    "C",             1,      1,               1,             1,
    "C",             1,      2,               3,             1,
    "C",             1,      3,               5,             1,
    "A",             1,      1,               2,             1,
    "A",             1,      1,               3,             1,
    "B",             NA,     4,               1,             1,
    "B",             NA,     5,               3,             1,
    "B",             NA,     7,               9,             1,
    "C",             2,      1,               0,             2,
    "C",             2,      2,               -1,            2,
    "C",             2,      3,               3,             2,
    "C",             2,      4,               -4,            2
  )
  
  actual <- calculatePoooledSD(dataset1)
  
  expect_equal(actual$d18O, 1.207615, tolerance = 1e-6)
  expect_equal(actual$dD, 2.919047,  tolerance = 1e-6)
})
