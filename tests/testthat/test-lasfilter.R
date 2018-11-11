context("lasfilter")

las = lidR:::dummy_las(500)

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

