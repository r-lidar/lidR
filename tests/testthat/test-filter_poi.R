context("filter_poi")

las = random_500_points

test_that("filter works", {
  las2 = filter_poi(las, X > 50)
  expect_equal(npoints(las2), 230)

  las2 = filter_poi(las, Intensity > 25)
  expect_equal(npoints(las2), 294)

  expect_error(filter_poi(las, plop > 25), "'plop'")
})

test_that("filter first return works", {
  expect_equal(dim(filter_first(las)@data)[1], 0.6*500)
})

test_that("filter firstlast return works", {
  expect_equal(dim(filter_firstlast(las)@data)[1], 0.9*500)
})

test_that("filter ground works", {
  expect_equal(dim(filter_ground(las)@data)[1], 0.2*500)
})

test_that("filter firstlast works", {
  expect_equal(dim(filter_last(las)@data)[1], 0.6*500)
})

test_that("filter on non conditionnal statement return an error", {
  expect_error(filter_poi(las, y = 2))
})

test_that("filter memory optimization works", {
  las3 = filter_poi(las, X > 0)
  expect_reference(las@data, las3@data)
})

test_that("filter preserves spatial index metadata", {
  sensor(las) <- "tls"
  index(las) <- "octree"
  las@index$xprt <- "dummy"
  las2 = filter_poi(las, Z > 10)
  expect_equal(index(las), index(las2))
  expect_equal(sensor(las), sensor(las2))
  expect_true(is.null(las2@index$xprt))
})


