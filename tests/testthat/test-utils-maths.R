test_that("calculating pooled standard deviation works", {
  
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
  
  actual <- calculatePooledSD(dataset1)
  
  expect_equal(actual$d18O, 1.207615, tolerance = 1e-6)
  expect_equal(actual$dD, 2.919047,  tolerance = 1e-6)

})

test_that("calculating root mean square deviation works", {

  expect_equal(calculateRMSD(1 : 5, 2 : 6), 1)
  expect_equal(calculateRMSD(c(1, sqrt(2), 2, 3)), 2)
  expect_error(calculateRMSD(1 : 5, 2 : 3))

})

test_that("calculating and adding d-excess column works", {
  
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
