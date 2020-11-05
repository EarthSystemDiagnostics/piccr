library(tibble)

context("test addColumnDExcess (d_excess = dH - 8 * d18O)")

test_that("test addColumnDExcess", {
  
  dataset1 <- tibble::tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol,
    # ------------ / ------------ / -------- /
    1,               4,             10,
    2,               3,             10,
    3,               2,             10,
    -4,              1,             10,
    5,               0,             10
  )
  expected1 <- tibble::tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol, ~dExcess,
    # ------- / --------- / -------- / --------
    1,         4,             10,        -4,
    2,         3,             10,        -13,
    3,         2,             10,        -22,
    -4,        1,             10,        33,
    5,         0,             10,        -40
  ) 
  dataset2 <- tibble::tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol,
    # ------- / --------- / -------
    1,          8,             10,
    2,          16,            10,
    3,          32,            10,
    -5,         100,           10,
    -20,        0,             10
  )
  expected2 <- tibble::tribble(
    ~`d(18_16)Mean`, ~`d(D_H)Mean`, ~otherCol, ~dExcess,
    # ------- / -------- / -------- / --------
    1,         8,             10,        0,
    2,         16,            10,        0,
    3,         32,            10,        8,
    -5,        100,           10,        140,
    -20,       0,             10,        160
  )
  
  actual1 <- addColumnDExcess(dataset1)
  actual2 <- addColumnDExcess(dataset2)
  
  expect_equal(actual1, expected1)
  expect_equal(actual2, expected2)
})
