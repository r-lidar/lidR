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

test_that("clip rectangle works", {
  rect = lasclipRectangle(las, 10, 10, 50, 50)
  expect_true(extent(rect) <= raster::extent(10,50,10,50))
})

test_that("clip circle works", {
  circ = lasclipCircle(las, 50, 50, 10)
  expect_true(extent(circ) <= raster::extent(40,60,40,60))
})

test_that("clip generic with cuboid works", {
  mcube = matrix(c(10,10,0,50,50,20), nrow = 2, byrow = T)
  cube = lasclip(las, geometry = "cuboid", mcube)
  expect_true(extent(cube) <= raster::extent(10,50,10,50))
  expect_lt(max(cube@data$Z), 20)
})

test_that("clip generic with cuboid order the coordinates", {
  mcube = matrix(c(50,50,20,10,10,0), nrow = 2, byrow = T)
  cube = lasclip(las, geometry = "cuboid", mcube)
  expect_true(extent(cube) <= raster::extent(10,50,10,50))
  expect_lt(max(cube@data$Z), 20)
})

test_that("clip generic with rectangle works", {
  mrect = matrix(c(10,10,50,50), nrow = 2, byrow = T)
  rect  = lasclipRectangle(las, 10, 10, 50, 50)
  rect2 = lasclip(las, geometry = "rectangle", mrect)
  expect_equal(rect2, rect)
})

test_that("filter on non matching data return null", {
  expect_warning(lasfilter(las, X > 200))
  expect_equal(suppressWarnings(lasfilter(las, X > 200)), NULL)
})

