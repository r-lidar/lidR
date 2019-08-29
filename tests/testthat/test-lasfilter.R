context("lasfilter")

set.seed(42)
las = lidR:::dummy_las(500)

test_that("filter works", {
  las2 = lasfilter(las, X > 50)
  expect_equal(npoints(las2), 230)

  las2 = lasfilter(las, Intensity > 25)
  expect_equal(npoints(las2), 304)

  expect_error(lasfilter(las, plop > 25), "'plop'")
})

test_that("filter first return works", {
  expect_equal(dim(lasfilterfirst(las)@data)[1], 0.6*500)
})

test_that("filter firstlast return works", {
  expect_equal(dim(lasfilterfirstlast(las)@data)[1], 0.9*500)
})

test_that("filter ground works", {
  expect_equal(dim(lasfilterground(las)@data)[1], 0.2*500)
})

test_that("filter firstlast works", {
  expect_equal(dim(lasfilterlast(las)@data)[1], 0.6*500)
})

test_that("filter on non conditionnal statement return an error", {
  expect_error(lasfilter(las, y = 2))
})

test_that("filter memory optimization works", {
  las3 = lasfilter(las, X > 0)
  expect_reference(las@data, las3@data)
})

