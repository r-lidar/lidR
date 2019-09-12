context("utils metrics")

test_that("LAD works", {
  set.seed(42)
  z = runif(1e4, 0, 20)
  lad = LAD(z,  z0 = 0)

  expect_equal(lad$z, seq(0.5, 19.5 , 1))
})
