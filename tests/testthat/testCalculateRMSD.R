context("test calculateRMSD")

test_that("test calculating root mean square deviation", {

  expect_equal(calculateRMSD(1 : 5, 2 : 6), 1)
  expect_error(calculateRMSD(1 : 5, 2 : 3))

})
