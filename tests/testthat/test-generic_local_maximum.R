test_that("locate local maxima works", {
  skip_on_cran()

  u <- lidR:::locate_localmaxima(example, 3)
  expect_is(u, "sf")
  expect_equal(dim(u), c(3,15))

  u <- lidR:::locate_localmaxima(example, c(15, 3))
  expect_equal(dim(u), c(1,15))

  u <- lidR:::locate_localmaxima(example, c(3, 15))
  expect_equal(dim(u), c(3,15))

  u <- lidR:::locate_localmaxima(example, c(15, 3, pi/4))
  expect_equal(dim(u), c(3,15))
})
