context("metrics")

test_that("gap fraction returns proper values", {
  set.seed(1)
  Z = runif(50000, 0, 5)
  gf = 1-gap_fraction_profile(Z, 1, 0)$gf

  expect_equal(gf, c(1, 0.5, 0.333, 0.25, 0.2), tolerance = 0.01)
})

test_that("entropy returns proper values", {
  set.seed(1)
  Z = runif(20000, 0, 5)
  S = entropy(Z)

  expect_equal(S, 1, tolerance = 0.0001)

  set.seed(42)
  Z = runif(20000, 0, 1)
  Z = c(Z, 5)
  S = entropy(Z)

  expect_equal(S, 0)
})

test_that("VCI returns the same as entropy values", {
  Z = runif(100, 0, 5)
  S = entropy(Z, zmax = 6)
  V = VCI(Z, 6)

  expect_equal(S, V)
})

