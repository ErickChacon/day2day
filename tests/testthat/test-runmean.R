context("Rcpp functions")

test_that("moving average", {
  expect_equal(runmean(1:5, 3), c(NA, NA, 2:4))
  expect_equal(runmean(1:5, 5), c(NA, NA, NA, NA, 3))
})

